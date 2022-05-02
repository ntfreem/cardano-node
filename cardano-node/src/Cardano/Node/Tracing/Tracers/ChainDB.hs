{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.ChainDB
  ( severityChainDB
  , namesForChainDBTraceEvents
  , withAddedToCurrentChainEmptyLimited
  , docChainDBTraceEvent
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Numeric (showFFloat)
import           Prelude (id)
import           Text.Show

import           Cardano.Logging
import           Cardano.Node.Tracing.Era.Byron ()
import           Cardano.Node.Tracing.Era.Shelley ()
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Render
import           Cardano.Prelude hiding (Show, show, trace)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HeaderEnvelopeError (..), HeaderError (..),
                   OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger, LedgerEvent (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import qualified Ouroboros.Consensus.Protocol.PBFT as PBFT
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (chunkNoToInt)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Types (UpdateLedgerDbTraceEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.Types as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl as VolDb
import           Ouroboros.Consensus.Util.Condense (condense)

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Ouroboros.Network.AnchoredFragment as AF

{-# ANN module ("HLint: ignore Redundant bracket" :: Text) #-}

withAddedToCurrentChainEmptyLimited
  :: Trace IO (ChainDB.TraceEvent blk)
  -> IO (Trace IO (ChainDB.TraceEvent blk))
withAddedToCurrentChainEmptyLimited tr = do
  ltr <- limitFrequency 1.25 "AddedToCurrentChainLimiter" tr mempty
  routingTrace (selecting ltr) tr
 where
    selecting
      ltr
      (ChainDB.TraceAddBlockEvent (ChainDB.AddedToCurrentChain events _ _ _)) =
        if null events
          then pure ltr
          else pure tr
    selecting _ _ = pure tr

kindContext :: Text -> A.Object -> A.Object
kindContext toAdd = runIdentity . KeyMap.alterF f "kind"
  where
    f Nothing = Identity $ Just (String toAdd)
    f (Just (String old)) = Identity $ Just (String (toAdd <> "." <> old))
    f _ = Identity Nothing

--------------------------------------------------------------------------------
-- ChainDB Tracer
--------------------------------------------------------------------------------

severityChainDB :: ChainDB.TraceEvent blk -> SeverityS
severityChainDB (ChainDB.TraceAddBlockEvent v)          = sevTraceAddBlockEvent v
severityChainDB (ChainDB.TraceFollowerEvent v)          = sevTraceFollowerEvent v
severityChainDB (ChainDB.TraceCopyToImmutableDBEvent v) = sevTraceCopyToImmutableDBEvent v
severityChainDB (ChainDB.TraceGCEvent v)                = sevTraceGCEvent v
severityChainDB (ChainDB.TraceInitChainSelEvent v)      = sevTraceInitChainSelEvent v
severityChainDB (ChainDB.TraceOpenEvent v)              = sevTraceOpenEvent v
severityChainDB (ChainDB.TraceIteratorEvent v)          = sevTraceIteratorEvent v
severityChainDB (ChainDB.TraceLedgerEvent v)            = sevTraceLedgerEvent v
severityChainDB (ChainDB.TraceLedgerReplayEvent v)      = sevTraceLedgerReplayEvent v
severityChainDB (ChainDB.TraceImmutableDBEvent v)       = sevTraceImmutableDBEvent v
severityChainDB (ChainDB.TraceVolatileDBEvent v)        = sevTraceVolatileDBEvent v

namesForChainDBTraceEvents :: ChainDB.TraceEvent blk -> [Text]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent ev) =
  "AddBlockEvent" : namesForChainDBAddBlock ev
namesForChainDBTraceEvents (ChainDB.TraceFollowerEvent ev) =
  "FollowerEvent" : namesForChainDBFollower ev
namesForChainDBTraceEvents (ChainDB.TraceCopyToImmutableDBEvent ev) =
  "CopyToImmutableDBEvent" : namesForChainDBCopyToImmutable ev
namesForChainDBTraceEvents (ChainDB.TraceGCEvent ev) =
  "GCEvent" : namesForChainDBGCEvent ev
namesForChainDBTraceEvents (ChainDB.TraceInitChainSelEvent ev) =
  "InitChainSelEvent" : namesForInitChainSel ev
namesForChainDBTraceEvents (ChainDB.TraceOpenEvent ev) =
  "OpenEvent" : namesForChainDBOpenEvent ev
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent ev) =
  "IteratorEvent" : namesForChainDBIteratorEvent ev
namesForChainDBTraceEvents (ChainDB.TraceLedgerEvent ev) =
  "LedgerEvent" : namesForChainDBLedgerEvent ev
namesForChainDBTraceEvents (ChainDB.TraceLedgerReplayEvent ev) =
  "LedgerEvent" : namesForChainDBLedgerReplayEvent ev
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent ev) =
  "ImmDbEvent" : namesForChainDBImmutableDBEvent ev
namesForChainDBTraceEvents (ChainDB.TraceVolatileDBEvent ev) =
  "VolatileDbEvent" : namesForChainDBVolatileDBEvent ev


instance (  LogFormatting (Header blk)
          , LogFormatting (LedgerEvent blk)
          , LogFormatting (RealPoint blk)
          , ConvertRawHash blk
          , ConvertRawHash (Header blk)
          , HasHeader (Header blk)
          , LedgerSupportsProtocol blk
          , InspectLedger blk
          ) => LogFormatting (ChainDB.TraceEvent blk) where
  forHuman (ChainDB.TraceAddBlockEvent v)          = forHuman v
  forHuman (ChainDB.TraceFollowerEvent v)          = forHuman v
  forHuman (ChainDB.TraceCopyToImmutableDBEvent v) = forHuman v
  forHuman (ChainDB.TraceGCEvent v)                = forHuman v
  forHuman (ChainDB.TraceInitChainSelEvent v)      = forHuman v
  forHuman (ChainDB.TraceOpenEvent v)              = forHuman v
  forHuman (ChainDB.TraceIteratorEvent v)          = forHuman v
  forHuman (ChainDB.TraceLedgerEvent v)            = forHuman v
  forHuman (ChainDB.TraceLedgerReplayEvent v)      = forHuman v
  forHuman (ChainDB.TraceImmutableDBEvent v)       = forHuman v
  forHuman (ChainDB.TraceVolatileDBEvent v)        = forHuman v

  forMachine details (ChainDB.TraceAddBlockEvent v) =
    kindContext "AddBlockEvent" $ forMachine details v
  forMachine details (ChainDB.TraceFollowerEvent v) =
    kindContext "FollowerEvent" $ forMachine details v
  forMachine details (ChainDB.TraceCopyToImmutableDBEvent v) =
    kindContext "CopyToImmutableDBEvent" $ forMachine details v
  forMachine details (ChainDB.TraceGCEvent v) =
    kindContext "TraceGCEvent" $ forMachine details v
  forMachine details (ChainDB.TraceInitChainSelEvent v) =
    kindContext "InitChainSelEvent" $ forMachine details v
  forMachine details (ChainDB.TraceOpenEvent v) =
    kindContext "OpenEvent" $ forMachine details v
  forMachine details (ChainDB.TraceIteratorEvent v) =
    kindContext "IteratorEvent" $ forMachine details v
  forMachine details (ChainDB.TraceLedgerEvent v) =
    kindContext "LedgerEvent" $ forMachine details v
  forMachine details (ChainDB.TraceLedgerReplayEvent v) =
    kindContext "LedgerReplayEvent" $ forMachine details v
  forMachine details (ChainDB.TraceImmutableDBEvent v) =
    kindContext "ImmDbEvent" $ forMachine details v
  forMachine details (ChainDB.TraceVolatileDBEvent v) =
    kindContext "VolatileDBEvent" $ forMachine details v

  asMetrics (ChainDB.TraceAddBlockEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceFollowerEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceCopyToImmutableDBEvent v) = asMetrics v
  asMetrics (ChainDB.TraceGCEvent v)                = asMetrics v
  asMetrics (ChainDB.TraceInitChainSelEvent v)      = asMetrics v
  asMetrics (ChainDB.TraceOpenEvent v)              = asMetrics v
  asMetrics (ChainDB.TraceIteratorEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceLedgerEvent v)            = asMetrics v
  asMetrics (ChainDB.TraceLedgerReplayEvent v)      = asMetrics v
  asMetrics (ChainDB.TraceImmutableDBEvent v)       = asMetrics v
  asMetrics (ChainDB.TraceVolatileDBEvent v)        = asMetrics v

docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk)
docChainDBTraceEvent = Documented $
    mapDoc ChainDB.TraceAddBlockEvent docChainDBAddBlock
    <> mapDoc ChainDB.TraceFollowerEvent docChainDBFollower
    <> mapDoc ChainDB.TraceCopyToImmutableDBEvent docChainDBImmtable
    <> mapDoc ChainDB.TraceGCEvent docChainDBGCEvent
    <> mapDoc ChainDB.TraceInitChainSelEvent docChainDBInitChainSel
    <> mapDoc ChainDB.TraceOpenEvent docChainDBOpenEvent
    <> mapDoc ChainDB.TraceIteratorEvent docChainDBIteratorEvent
    <> mapDoc ChainDB.TraceLedgerEvent docChainDBLedgerEvent
    <> mapDoc ChainDB.TraceLedgerReplayEvent docChainDBLedgerReplayEvent
    <> mapDoc ChainDB.TraceImmutableDBEvent docChainDBImmutableDBEvent
    <> mapDoc ChainDB.TraceVolatileDBEvent docChainDBVolatileDBEvent

--------------------------------------------------------------------------------
-- AddBlockEvent
--------------------------------------------------------------------------------

sevTraceAddBlockEvent :: ChainDB.TraceAddBlockEvent blk -> SeverityS
sevTraceAddBlockEvent ChainDB.IgnoreBlockOlderThanK {} = Info
sevTraceAddBlockEvent ChainDB.IgnoreBlockAlreadyInVolatileDB {} = Info
sevTraceAddBlockEvent ChainDB.IgnoreInvalidBlock {} = Info
sevTraceAddBlockEvent ChainDB.AddedBlockToQueue {} = Info
sevTraceAddBlockEvent ChainDB.PoppedBlockFromQueue {} = Info
sevTraceAddBlockEvent ChainDB.BlockInTheFuture {} = Info
sevTraceAddBlockEvent ChainDB.AddedBlockToVolatileDB {} = Info
sevTraceAddBlockEvent ChainDB.TryAddToCurrentChain {} = Debug
sevTraceAddBlockEvent ChainDB.TrySwitchToAFork {} = Info
sevTraceAddBlockEvent ChainDB.StoreButDontChange {} = Debug
sevTraceAddBlockEvent (ChainDB.AddedToCurrentChain events _ _ _) =
      maximumDef Notice (map sevLedgerEvent events)
sevTraceAddBlockEvent (ChainDB.SwitchedToAFork events _ _ _) =
      maximumDef Notice (map sevLedgerEvent events)
sevTraceAddBlockEvent (ChainDB.AddBlockValidation ev') = sevTraceValidationEvent ev'
sevTraceAddBlockEvent ChainDB.ChainSelectionForFutureBlock{} = Debug
sevTraceAddBlockEvent ChainDB.PipeliningEvent{} = Info   -- TODO Debug?
sevTraceAddBlockEvent ChainDB.AddingBlockToQueue{} = Info   -- TODO
sevTraceAddBlockEvent ChainDB.PoppingBlockFromQueue{} = Info   -- TODO
sevTraceAddBlockEvent ChainDB.AddingBlockToVolatileDB{} = Info   -- TODO
sevTraceAddBlockEvent ChainDB.SwitchingSelection{} = Info   -- TODO

sevLedgerEvent :: LedgerEvent blk -> SeverityS
sevLedgerEvent (LedgerUpdate _)  = Notice
sevLedgerEvent (LedgerWarning _) = Critical

sevTraceValidationEvent :: ChainDB.TraceValidationEvent blk -> SeverityS
sevTraceValidationEvent ChainDB.InvalidBlock {} = Error
sevTraceValidationEvent ChainDB.ValidCandidate {} = Info
sevTraceValidationEvent ChainDB.CandidateContainsFutureBlocks {} = Debug
sevTraceValidationEvent ChainDB.UpdateLedgerDbTraceEvent {} = Debug
sevTraceValidationEvent ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{} = Error

namesForChainDBAddBlock :: ChainDB.TraceAddBlockEvent blk -> [Text]
namesForChainDBAddBlock (ChainDB.IgnoreBlockOlderThanK _) =
      ["IgnoreBlockOlderThanK"]
namesForChainDBAddBlock (ChainDB.IgnoreBlockAlreadyInVolatileDB _) =
      ["IgnoreBlockAlreadyInVolatileDB"]
namesForChainDBAddBlock (ChainDB.IgnoreInvalidBlock {}) =
      ["IgnoreBlockAlreadyInVolatileDB"]
namesForChainDBAddBlock (ChainDB.AddedBlockToQueue {}) =
      ["AddedBlockToQueue"]
namesForChainDBAddBlock (ChainDB.PoppedBlockFromQueue {}) =
      ["PoppedBlockFromQueue"]
namesForChainDBAddBlock (ChainDB.BlockInTheFuture {}) =
      ["BlockInTheFuture"]
namesForChainDBAddBlock (ChainDB.AddedBlockToVolatileDB {}) =
      ["AddedBlockToVolatileDB"]
namesForChainDBAddBlock (ChainDB.TryAddToCurrentChain {}) =
      ["TryAddToCurrentChain"]
namesForChainDBAddBlock (ChainDB.TrySwitchToAFork {}) =
      ["TrySwitchToAFork"]
namesForChainDBAddBlock (ChainDB.StoreButDontChange {}) =
      ["StoreButDontChange"]
namesForChainDBAddBlock (ChainDB.AddedToCurrentChain {}) =
      ["AddedToCurrentChain"]
namesForChainDBAddBlock (ChainDB.SwitchedToAFork {}) =
      ["SwitchedToAFork"]
namesForChainDBAddBlock (ChainDB.AddBlockValidation ev') =
      "AddBlockValidation" : namesForChainDBAddBlockValidation ev'
namesForChainDBAddBlock (ChainDB.ChainSelectionForFutureBlock {}) =
      ["ChainSelectionForFutureBlock"]
namesForChainDBAddBlock (ChainDB.PipeliningEvent ev) = "PipeliningEvent" : case ev of
      ChainDB.SetTentativeHeader{}      -> ["SetTentativeHeader"]
      ChainDB.TrapTentativeHeader{}     -> ["TrapTentativeHeader"]
      ChainDB.OutdatedTentativeHeader{} -> ["OutdatedTentativeHeader"]
      ChainDB.SettingTentativeHeader{}  -> ["SettingTentativeHeader"]
namesForChainDBAddBlock (ChainDB.AddingBlockToQueue {}) =
      ["AddingBlockToQueue"]
namesForChainDBAddBlock (ChainDB.PoppingBlockFromQueue {}) =
      ["PoppingBlockFromQueue"]
namesForChainDBAddBlock (ChainDB.AddingBlockToVolatileDB {}) =
      ["AddingBlockToVolatileDB"]
namesForChainDBAddBlock (ChainDB.SwitchingSelection {}) =
      ["SwitchingSelection"]

namesForChainDBAddBlockValidation :: ChainDB.TraceValidationEvent blk -> [Text]
namesForChainDBAddBlockValidation (ChainDB.ValidCandidate {}) =
      ["ValidCandidate"]
namesForChainDBAddBlockValidation (ChainDB.CandidateContainsFutureBlocks {}) =
      ["CandidateContainsFutureBlocks"]
namesForChainDBAddBlockValidation (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {}) =
      ["CandidateContainsFutureBlocksExceedingClockSkew"]
namesForChainDBAddBlockValidation (ChainDB.InvalidBlock {}) =
      ["InvalidBlock"]
namesForChainDBAddBlockValidation (ChainDB.UpdateLedgerDbTraceEvent {}) =
      ["UpdateLedgerDb"]

instance ( LogFormatting (Header blk)
         , LogFormatting (LedgerEvent blk)
         , LogFormatting (RealPoint blk)
         , ConvertRawHash blk
         , ConvertRawHash (Header blk)
         , HasHeader (Header blk)
         , LedgerSupportsProtocol blk
         , InspectLedger blk
         ) => LogFormatting (ChainDB.TraceAddBlockEvent blk) where
  forHuman (ChainDB.IgnoreBlockOlderThanK pt) =
    "Ignoring block older than K: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.IgnoreBlockAlreadyInVolatileDB pt) =
      "Ignoring block already in DB: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.IgnoreInvalidBlock pt _reason) =
      "Ignoring previously seen invalid block: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.AddedBlockToQueue pt sz) =
      "Block added to queue: " <> renderRealPointAsPhrase pt <> " queue size " <> condenseT sz
  forHuman (ChainDB.PoppedBlockFromQueue pt) =
      "Block popped from queue: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.BlockInTheFuture pt slot) =
      "Ignoring block from future: " <> renderRealPointAsPhrase pt <> ", slot " <> condenseT slot
  forHuman (ChainDB.StoreButDontChange pt) =
      "Ignoring block: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.TryAddToCurrentChain pt) =
      "Block fits onto the current chain: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.TrySwitchToAFork pt _) =
      "Block fits onto some fork: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.AddedToCurrentChain es _ _ c) =
      "Chain extended, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
        Text.concat [ "\nEvent: " <> showT e | e <- es ]
  forHuman (ChainDB.SwitchedToAFork es _ _ c) =
      "Switched to a fork, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
        Text.concat [ "\nEvent: " <> showT e | e <- es ]
  forHuman (ChainDB.AddBlockValidation ev') = forHuman ev'
  forHuman (ChainDB.AddedBlockToVolatileDB pt _ _) =
      "Chain added block " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.ChainSelectionForFutureBlock pt) =
      "Chain selection run for block previously from future: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.PipeliningEvent ev') = forHuman ev'
  forHuman ChainDB.AddingBlockToQueue{} = "TODO AddingBlockToQueue"   -- TODO
  forHuman ChainDB.PoppingBlockFromQueue{} = "TODO PoppingBlockFromQueue"   -- TODO
  forHuman ChainDB.AddingBlockToVolatileDB{} = "TODO AddingBlockToVolatileDB"   -- TODO
  forHuman ChainDB.SwitchingSelection{} = "TODO SwitchingSelection"   -- TODO

  forMachine dtal (ChainDB.IgnoreBlockOlderThanK pt) =
      mconcat [ "kind" .= String "IgnoreBlockOlderThanK"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.IgnoreBlockAlreadyInVolatileDB pt) =
      mconcat [ "kind" .= String "IgnoreBlockAlreadyInVolatileDB"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.IgnoreInvalidBlock pt reason) =
      mconcat [ "kind" .= String "IgnoreInvalidBlock"
               , "block" .= forMachine dtal pt
               , "reason" .= showT reason ]
  forMachine dtal (ChainDB.AddedBlockToQueue pt sz) =
      mconcat [ "kind" .= String "AddedBlockToQueue"
               , "block" .= forMachine dtal pt
               , "queueSize" .= toJSON sz ]
  forMachine dtal (ChainDB.PoppedBlockFromQueue pt) =
      mconcat [ "kind" .= String "PoppedBlockFromQueue"
               , "block" .= forMachine dtal pt
               ]
  forMachine dtal (ChainDB.BlockInTheFuture pt slot) =
      mconcat [ "kind" .= String "BlockInTheFuture"
               , "block" .= forMachine dtal pt
               , "slot" .= forMachine dtal slot ]
  forMachine dtal (ChainDB.StoreButDontChange pt) =
      mconcat [ "kind" .= String "StoreButDontChange"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.TryAddToCurrentChain pt) =
      mconcat [ "kind" .= String "TryAddToCurrentChain"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.TrySwitchToAFork pt _) =
      mconcat [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.AddedToCurrentChain events _ base extended) =
      mconcat $
               [ "kind" .=  String "AddedToCurrentChain"
               , "newtip" .= renderPointForDetails dtal (AF.headPoint extended)
               ]
            ++ [ "headers" .= toJSON (forMachine dtal `map` addedHdrsNewChain base extended)
               | dtal == DDetailed ]
            ++ [ "events" .= toJSON (map (forMachine dtal) events)
               | not (null events) ]
  forMachine dtal (ChainDB.SwitchedToAFork events _ old new) =
      mconcat $
               [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork"
               , "newtip" .= renderPointForDetails dtal (AF.headPoint new)
               ]
            ++ [ "headers" .= toJSON (forMachine dtal `map` addedHdrsNewChain old new)
               | dtal == DDetailed ]
            ++ [ "events" .= toJSON (map (forMachine dtal) events)
               | not (null events) ]
  forMachine dtal (ChainDB.AddBlockValidation ev') =
    kindContext "AddBlockEvent" $ forMachine dtal ev'
  forMachine dtal (ChainDB.AddedBlockToVolatileDB pt (BlockNo bn) _) =
      mconcat [ "kind" .= String "AddedBlockToVolatileDB"
               , "block" .= forMachine dtal pt
               , "blockNo" .= showT bn ]
  forMachine dtal (ChainDB.ChainSelectionForFutureBlock pt) =
      mconcat [ "kind" .= String "TChainSelectionForFutureBlock"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.PipeliningEvent ev') =
    kindContext "PipeliningEvent" $ forMachine dtal ev'
  forMachine dtal (ChainDB.AddingBlockToQueue pt) =
      mconcat [ "kind" .= String "AddingBlockToQueue"
               , "block" .= forMachine dtal pt
               ]
  forMachine _dtal (ChainDB.PoppingBlockFromQueue) =
      mconcat [ "kind" .= String "PoppingBlockFromQueue"
               ]
  forMachine dtal (ChainDB.AddingBlockToVolatileDB pt (BlockNo bn) _) =
      mconcat [ "kind" .= String "AddingBlockToVolatileDB"
               , "block" .= forMachine dtal pt
               , "blockNo" .= showT bn ]
  forMachine dtal (ChainDB.SwitchingSelection pt) =
      mconcat [ "kind" .= String "SwitchingSelection"
               , "block" .= forMachine dtal pt
               ]

  asMetrics (ChainDB.SwitchedToAFork _warnings newTipInfo _oldChain newChain) =
    let ChainInformation { slots, blocks, density, epoch, slotInEpoch } =
          chainInformation newTipInfo newChain 0
    in  [ DoubleM "cardano.node.density" (fromRational density)
        , IntM    "cardano.node.slotNum" (fromIntegral slots)
        , IntM    "cardano.node.blockNum" (fromIntegral blocks)
        , IntM    "cardano.node.slotInEpoch" (fromIntegral slotInEpoch)
        , IntM    "cardano.node.epoch" (fromIntegral (unEpochNo epoch))
        ]
  asMetrics (ChainDB.AddedToCurrentChain _warnings newTipInfo _oldChain newChain) =
    let ChainInformation { slots, blocks, density, epoch, slotInEpoch } =
          chainInformation newTipInfo newChain 0
    in  [ DoubleM "cardano.node.density" (fromRational density)
        , IntM    "cardano.node.slotNum" (fromIntegral slots)
        , IntM    "cardano.node.blockNum" (fromIntegral blocks)
        , IntM    "cardano.node.slotInEpoch" (fromIntegral slotInEpoch)
        , IntM    "cardano.node.epoch" (fromIntegral (unEpochNo epoch))
        ]
  asMetrics _ = []

instance ( ConvertRawHash (Header blk)
         , HasHeader (Header blk)
         ) => LogFormatting (ChainDB.TracePipeliningEvent blk) where
  forHuman (ChainDB.SetTentativeHeader hdr) =
      "Set tentative header to " <> renderPointAsPhrase (blockPoint hdr)
  forHuman (ChainDB.TrapTentativeHeader hdr) =
      "Discovered trap tentative header " <> renderPointAsPhrase (blockPoint hdr)
  forHuman (ChainDB.OutdatedTentativeHeader hdr) =
      "Tentative header is now outdated " <> renderPointAsPhrase (blockPoint hdr)
  forHuman (ChainDB.SettingTentativeHeader pt) =
      "Setting tentative header to " <> renderPointAsPhrase (castPoint (realPointToPoint pt) :: Point (Header blk))

  forMachine dtals (ChainDB.SetTentativeHeader hdr) =
      mconcat [ "kind" .= String "SetTentativeHeader"
               , "block" .= renderPointForDetails dtals (blockPoint hdr) ]
  forMachine dtals (ChainDB.TrapTentativeHeader hdr) =
      mconcat [ "kind" .= String "TrapTentativeHeader"
               , "block" .= renderPointForDetails dtals (blockPoint hdr) ]
  forMachine dtals (ChainDB.OutdatedTentativeHeader hdr) =
      mconcat [ "kind" .= String "OutdatedTentativeHeader"
               , "block" .= renderPointForDetails dtals (blockPoint hdr)]
  forMachine dtals (ChainDB.SettingTentativeHeader pt) =
      mconcat [ "kind" .= String "SettingTentativeHeader"
               , "block" .= renderPointForDetails dtals (castPoint (realPointToPoint pt) :: Point (Header blk)) ]

addedHdrsNewChain :: HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk)
  -> AF.AnchoredFragment (Header blk)
  -> [Header blk]
addedHdrsNewChain fro to_ =
 case AF.intersect fro to_ of
   Just (_, _, _, s2 :: AF.AnchoredFragment (Header blk)) ->
     AF.toOldestFirst s2
   Nothing -> [] -- No sense to do validation here.

instance ( HasHeader (Header blk)
         , LedgerSupportsProtocol blk
         , ConvertRawHash (Header blk)
         , ConvertRawHash blk
         , LogFormatting (RealPoint blk))
         => LogFormatting (ChainDB.TraceValidationEvent blk) where
    forHuman (ChainDB.InvalidBlock err pt) =
        "Invalid block " <> renderRealPointAsPhrase pt <> ": " <> showT err
    forHuman (ChainDB.ValidCandidate c) =
        "Valid candidate " <> renderPointAsPhrase (AF.headPoint c)
    forHuman (ChainDB.CandidateContainsFutureBlocks c hdrs) =
        "Candidate contains blocks from near future:  " <>
          renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
          Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)
    forHuman (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs) =
        "Candidate contains blocks from future exceeding clock skew limit: " <>
          renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
          Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)
    forHuman (ChainDB.UpdateLedgerDbTraceEvent
                (StartedPushingBlockToTheLedgerDb
                  (LedgerDB.PushStart start)
                  (LedgerDB.PushGoal goal)
                  (LedgerDB.Pushing curr))) =
            let fromSlot = unSlotNo $ realPointSlot start
                atSlot   = unSlotNo $ realPointSlot curr
                atDiff   = atSlot - fromSlot
                toSlot   = unSlotNo $ realPointSlot goal
                toDiff   = toSlot - fromSlot
            in
              "Pushing ledger state for block " <> renderRealPointAsPhrase curr <> ". Progress: " <>
              showProgressT (fromIntegral atDiff) (fromIntegral toDiff) <> "%"

    forMachine dtal  (ChainDB.InvalidBlock err pt) =
            mconcat [ "kind" .= String "InvalidBlock"
                     , "block" .= forMachine dtal pt
                     , "error" .= showT err ]
    forMachine dtal  (ChainDB.ValidCandidate c) =
            mconcat [ "kind" .= String "ValidCandidate"
                     , "block" .= renderPointForDetails dtal (AF.headPoint c) ]
    forMachine dtal  (ChainDB.CandidateContainsFutureBlocks c hdrs) =
            mconcat [ "kind" .= String "CandidateContainsFutureBlocks"
                     , "block"   .= renderPointForDetails dtal (AF.headPoint c)
                     , "headers" .= map (renderPointForDetails dtal . headerPoint) hdrs ]
    forMachine dtal  (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs) =
            mconcat [ "kind" .= String "CandidateContainsFutureBlocksExceedingClockSkew"
                     , "block"   .= renderPointForDetails dtal (AF.headPoint c)
                     , "headers" .= map (renderPointForDetails dtal . headerPoint) hdrs ]
    forMachine _dtal (ChainDB.UpdateLedgerDbTraceEvent
                        (StartedPushingBlockToTheLedgerDb
                          (LedgerDB.PushStart start)
                          (LedgerDB.PushGoal goal)
                          (LedgerDB.Pushing curr))) =
            mconcat [ "kind" .= String "UpdateLedgerDbTraceEvent.StartedPushingBlockToTheLedgerDb"
                     , "startingBlock" .= renderRealPoint start
                     , "currentBlock" .= renderRealPoint curr
                     , "targetBlock" .= renderRealPoint goal
                     ]

showProgressT :: Int -> Int -> Text
showProgressT chunkNo outOf =
  Text.pack (showFFloat
          (Just 2)
          (100 * fromIntegral chunkNo / fromIntegral outOf :: Float)
          mempty)

data ChainInformation = ChainInformation
  { slots                :: Word64
  , blocks               :: Word64
  , density              :: Rational
    -- ^ the actual number of blocks created over the maximum expected number
    -- of blocks that could be created over the span of the last @k@ blocks.
  , epoch                :: EpochNo
    -- ^ In which epoch is the tip of the current chain
  , slotInEpoch          :: Word64
    -- ^ Relative slot number of the tip of the current chain within the
    -- epoch.
  , blocksUncoupledDelta :: Int64
    -- ^ The net change in number of blocks forged since last restart not on the
    -- current chain.
  }

chainInformation
  :: forall blk. HasHeader (Header blk)
  => ChainDB.NewTipInfo blk
  -> AF.AnchoredFragment (Header blk)
  -> Int64
  -> ChainInformation
chainInformation newTipInfo frag blocksUncoupledDelta = ChainInformation
    { slots = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    , blocks = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    , density = fragmentChainDensity frag
    , epoch = ChainDB.newTipEpoch newTipInfo
    , slotInEpoch = ChainDB.newTipSlotInEpoch newTipInfo
    , blocksUncoupledDelta = blocksUncoupledDelta
    }

fragmentChainDensity ::
  HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk) -> Rational
fragmentChainDensity frag = calcDensity blockD slotD
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b


docChainDBAddBlock :: [DocMsg (ChainDB.TraceAddBlockEvent blk)]
docChainDBAddBlock = [
      DocMsg
        (ChainDB.IgnoreBlockOlderThanK anyProto)
        []
        "A block with a 'BlockNo' more than @k@ back than the current tip\
        \ was ignored."
    , DocMsg
        (ChainDB.IgnoreBlockAlreadyInVolatileDB anyProto)
        []
        "A block that is already in the Volatile DB was ignored."
    , DocMsg
        (ChainDB.IgnoreInvalidBlock anyProto anyProto)
        []
        "A block that is already in the Volatile DB was ignored."
    , DocMsg
        (ChainDB.AddedBlockToQueue
            anyProto anyProto)
        []
        "The block was added to the queue and will be added to the ChainDB by\
        \ the background thread. The size of the queue is included.."
    , DocMsg
        (ChainDB.PoppedBlockFromQueue anyProto)
        []
        "The block was popped from the queue by the background thread."
    , DocMsg
        (ChainDB.BlockInTheFuture anyProto anyProto)
        []
        "The block is from the future, i.e., its slot number is greater than\
        \ the current slot (the second argument)."
    , DocMsg
        (ChainDB.AddedBlockToVolatileDB anyProto anyProto anyProto)
        []
        "A block was added to the Volatile DB"
    , DocMsg
        (ChainDB.TryAddToCurrentChain anyProto)
        []
        "The block fits onto the current chain, we'll try to use it to extend\
        \ our chain."
    , DocMsg
        (ChainDB.TrySwitchToAFork anyProto anyProto)
        []
        "The block fits onto some fork, we'll try to switch to that fork (if\
        \ it is preferable to our chain)"
    , DocMsg
        (ChainDB.StoreButDontChange anyProto)
        []
        "The block fits onto some fork, we'll try to switch to that fork (if\
        \ it is preferable to our chain)."
    , DocMsg
        (ChainDB.AddedToCurrentChain [] anyProto anyProto anyProto)
        [("cardano.node.density",
          "The actual number of blocks created over the maximum expected number\
          \ of blocks that could be created over the span of the last @k@ blocks.")
        , ("cardano.node.slots",
          "Number of slots in this chain fragment.")
        , ("cardano.node.blocks",
          "Number of blocks in this chain fragment.")
        , ("cardano.node.slotInEpoch",
          "Relative slot number of the tip of the current chain within the\
          \epoch..")
        , ("cardano.node.epoch",
          "In which epoch is the tip of the current chain.")
        ]
        "The new block fits onto the current chain (first\
        \ fragment) and we have successfully used it to extend our (new) current\
        \ chain (second fragment)."
    , DocMsg
        (ChainDB.SwitchedToAFork [] anyProto anyProto anyProto)
        [ ("cardano.node.density",
          "The actual number of blocks created over the maximum expected number\
          \ of blocks that could be created over the span of the last @k@ blocks.")
        , ("cardano.node.slots",
          "Number of slots in this chain fragment.")
        , ("cardano.node.blocks",
          "Number of blocks in this chain fragment.")
        , ("cardano.node.slotInEpoch",
          "Relative slot number of the tip of the current chain within the\
          \epoch..")
        , ("cardano.node.epoch",
          "In which epoch is the tip of the current chain.")
        ]
        "The new block fits onto some fork and we have switched to that fork\
        \ (second fragment), as it is preferable to our (previous) current chain\
        \ (first fragment)."
    , DocMsg
        (ChainDB.AddBlockValidation
            (ChainDB.InvalidBlock anyProto anyProto))
        []
        "An event traced during validating performed while adding a block.\
        \ A point was found to be invalid."
    , DocMsg
        (ChainDB.AddBlockValidation
            (ChainDB.ValidCandidate anyProto))
        []
        "An event traced during validating performed while adding a block.\
        \ A candidate chain was valid."
    , DocMsg
        (ChainDB.AddBlockValidation
          (ChainDB.CandidateContainsFutureBlocks anyProto anyProto))
        []
        "An event traced during validating performed while adding a block.\
        \ Candidate contains headers from the future which do no exceed the\
        \ clock skew."
    , DocMsg
        (ChainDB.AddBlockValidation
          (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew anyProto anyProto))
        []
        "An event traced during validating performed while adding a block.\
        \ Candidate contains headers from the future which exceed the\
        \ clock skew."
    , DocMsg
        (ChainDB.ChainSelectionForFutureBlock anyProto)
        []
        "Run chain selection for a block that was previously from the future.\
        \ This is done for all blocks from the future each time a new block is\
        \ added."
    , DocMsg
        (ChainDB.PipeliningEvent anyProto)
        []
        "An event trace during block selection whenever something relevant to\
        \ pipelining has happens."
    ]


--------------------------------------------------------------------------------
-- FollowerEvent
--------------------------------------------------------------------------------

sevTraceFollowerEvent :: ChainDB.TraceFollowerEvent blk -> SeverityS
sevTraceFollowerEvent ChainDB.NewFollower {}            = Debug
sevTraceFollowerEvent ChainDB.FollowerNoLongerInMem {}  = Debug
sevTraceFollowerEvent ChainDB.FollowerSwitchToMem {}    = Debug
sevTraceFollowerEvent ChainDB.FollowerNewImmIterator {} = Debug

namesForChainDBFollower :: ChainDB.TraceFollowerEvent blk -> [Text]
namesForChainDBFollower  ChainDB.NewFollower =
      ["NewFollower"]
namesForChainDBFollower (ChainDB.FollowerNoLongerInMem {}) =
      ["FollowerNoLongerInMem"]
namesForChainDBFollower (ChainDB.FollowerSwitchToMem {}) =
      ["FollowerSwitchToMem"]
namesForChainDBFollower (ChainDB.FollowerNewImmIterator {}) =
      ["FollowerNewImmIterator"]

docChainDBFollower :: [DocMsg (ChainDB.TraceFollowerEvent ev)]
docChainDBFollower = [
      DocMsg
        ChainDB.NewFollower
        []
        "A new follower was created."
    , DocMsg
        (ChainDB.FollowerNoLongerInMem anyProto)
        []
        "The follower was in the 'FollowerInImmutableDB' state and is switched to\
        \ the 'FollowerInMem' state."
    , DocMsg
        (ChainDB.FollowerSwitchToMem anyProto anyProto)
        []
        "The follower was in the 'FollowerInImmutableDB' state and is switched to\
        \ the 'FollowerInMem' state."
    , DocMsg
        (ChainDB.FollowerNewImmIterator anyProto anyProto)
        []
        "The follower is in the 'FollowerInImmutableDB' state but the iterator is\
        \ exhausted while the ImmDB has grown, so we open a new iterator to\
        \ stream these blocks too."
  ]

--------------------------------------------------------------------------------
-- CopiedBlockToImmutableDB
--------------------------------------------------------------------------------

sevTraceCopyToImmutableDBEvent :: ChainDB.TraceCopyToImmutableDBEvent blk -> SeverityS
sevTraceCopyToImmutableDBEvent ChainDB.CopiedBlockToImmutableDB {} = Debug
sevTraceCopyToImmutableDBEvent ChainDB.NoBlocksToCopyToImmutableDB = Debug

namesForChainDBCopyToImmutable :: ChainDB.TraceCopyToImmutableDBEvent blk -> [Text]
namesForChainDBCopyToImmutable (ChainDB.CopiedBlockToImmutableDB {}) =
  ["CopiedBlockToImmutableDB"]
namesForChainDBCopyToImmutable ChainDB.NoBlocksToCopyToImmutableDB =
  ["NoBlocksToCopyToImmutableDB"]

instance ConvertRawHash blk
          => LogFormatting (ChainDB.TraceCopyToImmutableDBEvent blk) where
  forHuman (ChainDB.CopiedBlockToImmutableDB pt) =
      "Copied block " <> renderPointAsPhrase pt <> " to the ImmDB"
  forHuman ChainDB.NoBlocksToCopyToImmutableDB  =
      "There are no blocks to copy to the ImmDB"

  forMachine dtals (ChainDB.CopiedBlockToImmutableDB pt) =
      mconcat [ "kind" .= String "CopiedBlockToImmutableDB"
               , "slot" .= forMachine dtals pt ]
  forMachine _dtals ChainDB.NoBlocksToCopyToImmutableDB =
      mconcat [ "kind" .= String "NoBlocksToCopyToImmutableDB" ]

docChainDBImmtable :: [DocMsg (ChainDB.TraceCopyToImmutableDBEvent blk)]
docChainDBImmtable = [
      DocMsg
        (ChainDB.CopiedBlockToImmutableDB anyProto)
        []
        "A block was successfully copied to the ImmDB."
    , DocMsg
        ChainDB.NoBlocksToCopyToImmutableDB
        []
        "There are no block to copy to the ImmDB."
  ]

--------------------------------------------------------------------------------
-- GCEvent
--------------------------------------------------------------------------------

sevTraceGCEvent :: ChainDB.TraceGCEvent blk -> SeverityS
sevTraceGCEvent ChainDB.PerformedGC {} = Debug
sevTraceGCEvent ChainDB.ScheduledGC {} = Debug

namesForChainDBGCEvent :: ChainDB.TraceGCEvent blk -> [Text]
namesForChainDBGCEvent (ChainDB.ScheduledGC {}) =
      ["ScheduledGC"]
namesForChainDBGCEvent (ChainDB.PerformedGC {}) =
      ["PerformedGC"]

instance LogFormatting (ChainDB.TraceGCEvent blk) where
  forHuman (ChainDB.PerformedGC slot) =
      "Performed a garbage collection for " <> condenseT slot
  forHuman (ChainDB.ScheduledGC slot _difft) =
      "Scheduled a garbage collection for " <> condenseT slot

  forMachine dtals (ChainDB.PerformedGC slot) =
      mconcat [ "kind" .= String "PerformedGC"
               , "slot" .= forMachine dtals slot ]
  forMachine dtals (ChainDB.ScheduledGC slot difft) =
      mconcat $ [ "kind" .= String "ScheduledGC"
                 , "slot" .= forMachine dtals slot ] <>
                 [ "difft" .= String ((Text.pack . show) difft) | dtals >= DDetailed]

docChainDBGCEvent :: [DocMsg (ChainDB.TraceGCEvent blk)]
docChainDBGCEvent = [
      DocMsg
        (ChainDB.ScheduledGC anyProto anyProto)
        []
        "There are no block to copy to the ImmDB."
    , DocMsg
        (ChainDB.PerformedGC anyProto)
        []
        "There are no block to copy to the ImmDB."
  ]

--------------------------------------------------------------------------------
-- TraceInitChainSelEvent
--------------------------------------------------------------------------------

sevTraceInitChainSelEvent :: ChainDB.TraceInitChainSelEvent blk -> SeverityS
sevTraceInitChainSelEvent ChainDB.StartedInitChainSelection {} = Info
sevTraceInitChainSelEvent ChainDB.InitalChainSelected {} = Info
sevTraceInitChainSelEvent (ChainDB.InitChainSelValidation ev') =
  case ev' of
      ChainDB.InvalidBlock{}                                     -> Debug
      ChainDB.ValidCandidate {}                                  -> Info
      ChainDB.CandidateContainsFutureBlocks {}                   -> Debug
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {} -> Debug
      ChainDB.UpdateLedgerDbTraceEvent {}                        -> Info

namesForInitChainSel :: ChainDB.TraceInitChainSelEvent blk -> [Text]
namesForInitChainSel (ChainDB.InitChainSelValidation
                              (ChainDB.InvalidBlock {})) =
      ["InvalidBlock"]
namesForInitChainSel (ChainDB.InitChainSelValidation
                              (ChainDB.ValidCandidate {})) =
      ["ValidCandidate"]
namesForInitChainSel (ChainDB.InitChainSelValidation
                              (ChainDB.CandidateContainsFutureBlocks {})) =
      ["CandidateContainsFutureBlocks"]
namesForInitChainSel (ChainDB.InitChainSelValidation
              (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {})) =
      ["CandidateContainsFutureBlocksExceedingClockSkew"]
namesForInitChainSel (ChainDB.InitChainSelValidation
                        (ChainDB.UpdateLedgerDbTraceEvent {})) =
      ["UpdateLedgerDb"]
namesForInitChainSel (ChainDB.StartedInitChainSelection {}) =
      ["StartedInitChainSelection"]
namesForInitChainSel (ChainDB.InitalChainSelected {}) =
      ["InitalChainSelected"]

instance (ConvertRawHash blk, LedgerSupportsProtocol blk)
  => LogFormatting (ChainDB.TraceInitChainSelEvent blk) where
    forHuman (ChainDB.InitChainSelValidation v) = forHuman v
    forHuman (ChainDB.InitalChainSelected {}) =
        "Initial chain selected"
    forHuman (ChainDB.StartedInitChainSelection {}) =
        "Started initial chain selection"

    forMachine dtal (ChainDB.InitChainSelValidation v) = forMachine dtal v
    forMachine _dtal ChainDB.InitalChainSelected =
      mconcat ["kind" .= String "Follower.InitalChainSelected"]
    forMachine _dtal ChainDB.StartedInitChainSelection =
      mconcat ["kind" .= String "Follower.StartedInitChainSelection"]

    asMetrics (ChainDB.InitChainSelValidation v) = asMetrics v
    asMetrics ChainDB.InitalChainSelected        = []
    asMetrics ChainDB.StartedInitChainSelection  = []


docChainDBInitChainSel :: [DocMsg (ChainDB.TraceInitChainSelEvent blk)]
docChainDBInitChainSel = [
      DocMsg
      (ChainDB.InitChainSelValidation
          (ChainDB.InvalidBlock anyProto anyProto))
      []
      "A point was found to be invalid."
    , DocMsg
      (ChainDB.InitChainSelValidation
          (ChainDB.ValidCandidate anyProto))
      []
      "A candidate chain was valid."
    , DocMsg
      (ChainDB.InitChainSelValidation
          (ChainDB.CandidateContainsFutureBlocks anyProto anyProto))
      []
      "Candidate contains headers from the future which do not exceed the\
      \ clock skew."
    , DocMsg
      (ChainDB.InitChainSelValidation
        (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew anyProto anyProto))
      []
      "Candidate contains headers from the future which exceed the\
      \ clock skew, making them invalid."
    , DocMsg
      (ChainDB.InitChainSelValidation
          (ChainDB.UpdateLedgerDbTraceEvent anyProto))
      []
      "UpdateLedgerDb"
   ,  DocMsg
        ChainDB.StartedInitChainSelection
      []
      "StartedInitChainSelection"
   ,  DocMsg
        ChainDB.InitalChainSelected
      []
      "InitalChainSelected"
  ]

--------------------------------------------------------------------------------
-- TraceOpenEvent
--------------------------------------------------------------------------------

sevTraceOpenEvent :: ChainDB.TraceOpenEvent blk -> SeverityS
sevTraceOpenEvent ChainDB.OpenedDB {}               = Info
sevTraceOpenEvent ChainDB.ClosedDB {}               = Info
sevTraceOpenEvent ChainDB.OpenedImmutableDB {}      = Info
sevTraceOpenEvent ChainDB.OpenedVolatileDB          = Info
sevTraceOpenEvent ChainDB.OpenedLgrDB               = Info
sevTraceOpenEvent ChainDB.StartedOpeningDB          = Info
sevTraceOpenEvent ChainDB.StartedOpeningImmutableDB = Info
sevTraceOpenEvent ChainDB.StartedOpeningVolatileDB  = Info
sevTraceOpenEvent ChainDB.StartedOpeningLgrDB       = Info

namesForChainDBOpenEvent :: ChainDB.TraceOpenEvent blk -> [Text]
namesForChainDBOpenEvent (ChainDB.OpenedDB {}) =
      ["OpenedDB"]
namesForChainDBOpenEvent (ChainDB.ClosedDB {}) =
      ["ClosedDB"]
namesForChainDBOpenEvent (ChainDB.OpenedImmutableDB {}) =
      ["OpenedImmutableDB"]
namesForChainDBOpenEvent ChainDB.OpenedVolatileDB =
      ["OpenedVolatileDB"]
namesForChainDBOpenEvent ChainDB.OpenedLgrDB =
      ["OpenedLgrDB"]
namesForChainDBOpenEvent ChainDB.StartedOpeningDB =
      ["StartedOpeningDB"]
namesForChainDBOpenEvent ChainDB.StartedOpeningImmutableDB =
      ["StartedOpeningImmutableDB"]
namesForChainDBOpenEvent ChainDB.StartedOpeningVolatileDB =
      ["StartedOpeningVolatileDB"]
namesForChainDBOpenEvent ChainDB.StartedOpeningLgrDB =
      ["StartedOpeningLgrDB"]


instance ConvertRawHash blk
          => LogFormatting (ChainDB.TraceOpenEvent blk) where
  forHuman (ChainDB.OpenedDB immTip tip') =
          "Opened db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
  forHuman (ChainDB.ClosedDB immTip tip') =
          "Closed db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
  forHuman (ChainDB.OpenedImmutableDB immTip chunk) =
          "Opened imm db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and chunk " <> showT chunk
  forHuman ChainDB.OpenedVolatileDB = "Opened vol db"
  forHuman ChainDB.OpenedLgrDB = "Opened lgr db"
  forHuman ChainDB.StartedOpeningDB = "Started opening Chain DB"
  forHuman ChainDB.StartedOpeningImmutableDB = "Started opening Immutable DB"
  forHuman ChainDB.StartedOpeningVolatileDB = "Started opening Volatile DB"
  forHuman ChainDB.StartedOpeningLgrDB = "Started opening Ledger DB"

  forMachine dtal (ChainDB.OpenedDB immTip tip')=
    mconcat [ "kind" .= String "OpenedDB"
             , "immtip" .= forMachine dtal immTip
             , "tip" .= forMachine dtal tip' ]
  forMachine dtal (ChainDB.ClosedDB immTip tip') =
    mconcat [ "kind" .= String "TraceOpenEvent.ClosedDB"
             , "immtip" .= forMachine dtal immTip
             , "tip" .= forMachine dtal tip' ]
  forMachine dtal (ChainDB.OpenedImmutableDB immTip epoch) =
    mconcat [ "kind" .= String "OpenedImmutableDB"
             , "immtip" .= forMachine dtal immTip
             , "epoch" .= String ((Text.pack . show) epoch) ]
  forMachine _dtal ChainDB.OpenedVolatileDB =
      mconcat [ "kind" .= String "OpenedVolatileDB" ]
  forMachine _dtal ChainDB.OpenedLgrDB =
      mconcat [ "kind" .= String "OpenedLgrDB" ]
  forMachine _dtal ChainDB.StartedOpeningDB =
      mconcat ["kind" .= String "StartedOpeningDB"]
  forMachine _dtal ChainDB.StartedOpeningImmutableDB =
      mconcat ["kind" .= String "StartedOpeningImmutableDB"]
  forMachine _dtal ChainDB.StartedOpeningVolatileDB =
      mconcat ["kind" .= String "StartedOpeningVolatileDB"]
  forMachine _dtal ChainDB.StartedOpeningLgrDB =
      mconcat ["kind" .= String "StartedOpeningLgrDB"]


docChainDBOpenEvent :: [DocMsg (ChainDB.TraceOpenEvent blk)]
docChainDBOpenEvent =
    [ DocMsg
      (ChainDB.OpenedDB anyProto anyProto)
      []
      "The ChainDB was opened."
    , DocMsg
      (ChainDB.ClosedDB anyProto anyProto)
      []
      "The ChainDB was closed."
    , DocMsg
      (ChainDB.OpenedImmutableDB anyProto anyProto)
      []
      "The ImmDB was opened."
    , DocMsg
      ChainDB.OpenedVolatileDB
      []
      "The VolatileDB was opened."
    , DocMsg
      ChainDB.OpenedLgrDB
      []
      "The LedgerDB was opened."
  ]

--------------------------------------------------------------------------------
-- IteratorEvent
--------------------------------------------------------------------------------

sevTraceIteratorEvent :: ChainDB.TraceIteratorEvent blk -> SeverityS
sevTraceIteratorEvent ChainDB.StreamFromVolatileDB {} = Debug
sevTraceIteratorEvent _                               = Debug

namesForChainDBIteratorEvent  :: ChainDB.TraceIteratorEvent blk -> [Text]
namesForChainDBIteratorEvent (ChainDB.UnknownRangeRequested {}) =
      ["UnknownRangeRequested"]
namesForChainDBIteratorEvent (ChainDB.StreamFromVolatileDB {}) =
      ["StreamFromVolatileDB"]
namesForChainDBIteratorEvent (ChainDB.StreamFromImmutableDB {}) =
      ["StreamFromImmutableDB"]
namesForChainDBIteratorEvent (ChainDB.StreamFromBoth {}) =
      ["StreamFromBoth"]
namesForChainDBIteratorEvent (ChainDB.BlockMissingFromVolatileDB {}) =
      ["BlockMissingFromVolatileDB"]
namesForChainDBIteratorEvent (ChainDB.BlockWasCopiedToImmutableDB {}) =
      ["BlockWasCopiedToImmutableDB"]
namesForChainDBIteratorEvent (ChainDB.BlockGCedFromVolatileDB {}) =
      ["BlockGCedFromVolatileDB"]
namesForChainDBIteratorEvent ChainDB.SwitchBackToVolatileDB =
      ["SwitchBackToVolatileDB"]

instance  ( StandardHash blk
          , ConvertRawHash blk
          ) => LogFormatting (ChainDB.TraceIteratorEvent blk) where
  forHuman (ChainDB.UnknownRangeRequested ev') = forHuman ev'
  forHuman (ChainDB.BlockMissingFromVolatileDB realPt) =
      "This block is no longer in the VolatileDB because it has been garbage\
         \ collected. It might now be in the ImmDB if it was part of the\
         \ current chain. Block: " <> renderRealPoint realPt
  forHuman (ChainDB.StreamFromImmutableDB sFrom sTo) =
      "Stream only from the ImmDB. StreamFrom:" <> showT sFrom <>
        " StreamTo: " <> showT sTo
  forHuman (ChainDB.StreamFromBoth sFrom sTo pts) =
      "Stream from both the VolatileDB and the ImmDB."
        <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
        <> " Points: " <> showT (map renderRealPoint pts)
  forHuman (ChainDB.StreamFromVolatileDB sFrom sTo pts) =
      "Stream only from the VolatileDB."
        <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
        <> " Points: " <> showT (map renderRealPoint pts)
  forHuman (ChainDB.BlockWasCopiedToImmutableDB pt) =
      "This block has been garbage collected from the VolatileDB is now\
        \ found and streamed from the ImmDB. Block: " <> renderRealPoint pt
  forHuman (ChainDB.BlockGCedFromVolatileDB pt) =
      "This block no longer in the VolatileDB and isn't in the ImmDB\
        \ either; it wasn't part of the current chain. Block: " <> renderRealPoint pt
  forHuman ChainDB.SwitchBackToVolatileDB = "SwitchBackToVolatileDB"

  forMachine _dtal (ChainDB.UnknownRangeRequested unkRange) =
    mconcat [ "kind" .= String "UnknownRangeRequested"
             , "range" .= String (showT unkRange)
             ]
  forMachine _dtal (ChainDB.StreamFromVolatileDB streamFrom streamTo realPt) =
    mconcat [ "kind" .= String "StreamFromVolatileDB"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.StreamFromImmutableDB streamFrom streamTo) =
    mconcat [ "kind" .= String "StreamFromImmutableDB"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             ]
  forMachine _dtal (ChainDB.StreamFromBoth streamFrom streamTo realPt) =
    mconcat [ "kind" .= String "StreamFromBoth"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockMissingFromVolatileDB realPt) =
    mconcat [ "kind" .= String "BlockMissingFromVolatileDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockWasCopiedToImmutableDB realPt) =
    mconcat [ "kind" .= String "BlockWasCopiedToImmutableDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockGCedFromVolatileDB realPt) =
    mconcat [ "kind" .= String "BlockGCedFromVolatileDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal ChainDB.SwitchBackToVolatileDB =
    mconcat ["kind" .= String "SwitchBackToVolatileDB"
             ]

instance  ( StandardHash blk
          , ConvertRawHash blk
          ) => LogFormatting (ChainDB.UnknownRange blk) where
  forHuman (ChainDB.MissingBlock realPt) =
      "The block at the given point was not found in the ChainDB."
        <> renderRealPoint realPt
  forHuman (ChainDB.ForkTooOld streamFrom) =
      "The requested range forks off too far in the past"
        <> showT streamFrom

  forMachine _dtal (ChainDB.MissingBlock realPt) =
    mconcat [ "kind"  .= String "MissingBlock"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.ForkTooOld streamFrom) =
    mconcat [ "kind" .= String "ForkTooOld"
             , "from" .= String (showT streamFrom)
             ]

docChainDBIteratorEvent :: [DocMsg (ChainDB.TraceIteratorEvent blk)]
docChainDBIteratorEvent = [
      DocMsg
      (ChainDB.UnknownRangeRequested anyProto)
      []
      "An unknown range was requested, see 'UnknownRange'."
    , DocMsg
      (ChainDB.StreamFromVolatileDB anyProto anyProto anyProto)
      []
      "Stream only from the VolatileDB."
    , DocMsg
      (ChainDB.StreamFromImmutableDB anyProto anyProto)
      []
      "Stream only from the ImmDB."
    , DocMsg
      (ChainDB.StreamFromBoth anyProto anyProto anyProto)
      []
      "Stream from both the VolatileDB and the ImmDB."
    , DocMsg
      (ChainDB.BlockMissingFromVolatileDB anyProto)
      []
      "A block is no longer in the VolatileDB because it has been garbage\
      \ collected. It might now be in the ImmDB if it was part of the\
      \ current chain."
    , DocMsg
      (ChainDB.BlockWasCopiedToImmutableDB anyProto)
      []
      "A block that has been garbage collected from the VolatileDB is now\
      \ found and streamed from the ImmDB."
    , DocMsg
      (ChainDB.BlockGCedFromVolatileDB anyProto)
      []
      "A block is no longer in the VolatileDB and isn't in the ImmDB\
      \ either; it wasn't part of the current chain."
    , DocMsg
      ChainDB.SwitchBackToVolatileDB
      []
      "We have streamed one or more blocks from the ImmDB that were part\
      \ of the VolatileDB when initialising the iterator. Now, we have to look\
      \ back in the VolatileDB again because the ImmDB doesn't have the\
      \ next block we're looking for."
  ]


--------------------------------------------------------------------------------
-- LedgerDB.TraceEvent
--------------------------------------------------------------------------------

sevTraceLedgerEvent :: LedgerDB.TraceEvent blk -> SeverityS
sevTraceLedgerEvent LedgerDB.TookSnapshot {}    = Info
sevTraceLedgerEvent LedgerDB.DeletedSnapshot {} = Debug
sevTraceLedgerEvent LedgerDB.InvalidSnapshot {} = Error

namesForChainDBLedgerEvent :: LedgerDB.TraceEvent blk -> [Text]
namesForChainDBLedgerEvent (LedgerDB.InvalidSnapshot {}) =
      ["InvalidSnapshot"]
namesForChainDBLedgerEvent (LedgerDB.TookSnapshot {}) =
      ["TookSnapshot"]
namesForChainDBLedgerEvent (LedgerDB.DeletedSnapshot {}) =
      ["DeletedSnapshot"]

instance ( StandardHash blk
         , ConvertRawHash blk)
         => LogFormatting (LedgerDB.TraceEvent blk) where
  forHuman (LedgerDB.TookSnapshot snap pt) =
      "Took ledger snapshot " <> showT snap <>
        " at " <> renderRealPointAsPhrase pt
  forHuman (LedgerDB.DeletedSnapshot snap) =
      "Deleted old snapshot " <> showT snap
  forHuman (LedgerDB.InvalidSnapshot snap failure) =
      "Invalid snapshot " <> showT snap <> showT failure

  forMachine dtals (LedgerDB.TookSnapshot snap pt) =
    mconcat [ "kind" .= String "TookSnapshot"
             , "snapshot" .= forMachine dtals snap
             , "tip" .= show pt ]
  forMachine dtals (LedgerDB.DeletedSnapshot snap) =
    mconcat [ "kind" .= String "DeletedSnapshot"
             , "snapshot" .= forMachine dtals snap ]
  forMachine dtals (LedgerDB.InvalidSnapshot snap failure) =
    mconcat [ "kind" .= String "TraceLedgerEvent.InvalidSnapshot"
             , "snapshot" .= forMachine dtals snap
             , "failure" .= show failure ]

docChainDBLedgerEvent :: [DocMsg (LedgerDB.TraceEvent blk)]
docChainDBLedgerEvent = [
      DocMsg
      (LedgerDB.InvalidSnapshot anyProto anyProto)
      []
      "An on disk snapshot was skipped because it was invalid."
    , DocMsg
      (LedgerDB.TookSnapshot anyProto anyProto)
      []
      "A snapshot was written to disk."
    , DocMsg
      (LedgerDB.DeletedSnapshot anyProto)
      []
      "An old or invalid on-disk snapshot was deleted."
  ]

--------------------------------------------------------------------------------
-- LedgerReplayEvent
--------------------------------------------------------------------------------

sevTraceLedgerReplayEvent :: LedgerDB.TraceReplayEvent blk -> SeverityS
sevTraceLedgerReplayEvent LedgerDB.ReplayFromGenesis {}  = Info
sevTraceLedgerReplayEvent LedgerDB.ReplayFromSnapshot {} = Info
sevTraceLedgerReplayEvent LedgerDB.ReplayedBlock {}      = Info

namesForChainDBLedgerReplayEvent :: LedgerDB.TraceReplayEvent blk -> [Text]
namesForChainDBLedgerReplayEvent (LedgerDB.ReplayFromGenesis {}) =
    ["ReplayFromGenesis"]
namesForChainDBLedgerReplayEvent (LedgerDB.ReplayFromSnapshot {}) =
    ["ReplayFromSnapshot"]
namesForChainDBLedgerReplayEvent (LedgerDB.ReplayedBlock {}) =
    ["ReplayedBlock"]

instance (StandardHash blk, ConvertRawHash blk)
          => LogFormatting (LedgerDB.TraceReplayEvent blk) where
  forHuman (LedgerDB.ReplayFromGenesis _replayTo) =
      "Replaying ledger from genesis"
  forHuman (LedgerDB.ReplayFromSnapshot snap tip' _ _) =
      "Replaying ledger from snapshot " <> showT snap <> " at " <>
        renderRealPointAsPhrase tip'
  forHuman (LedgerDB.ReplayedBlock
              pt
              _ledgerEvents
              (LedgerDB.ReplayStart replayFrom)
              (LedgerDB.ReplayGoal replayTo)) =
          let fromSlot = withOrigin 0 id $ unSlotNo <$> pointSlot replayFrom
              atSlot   = unSlotNo $ realPointSlot pt
              atDiff   = atSlot - fromSlot
              toSlot   = withOrigin 0 id $ unSlotNo <$> pointSlot replayTo
              toDiff   = toSlot - fromSlot
          in
             "Replayed block: slot "
          <> showT atSlot
          <> " out of "
          <> showT toSlot
          <> ". Progress: "
          <> showProgressT (fromIntegral atDiff) (fromIntegral toDiff)
          <> "%"

  forMachine _dtal (LedgerDB.ReplayFromGenesis _replayTo) =
      mconcat [ "kind" .= String "ReplayFromGenesis" ]
  forMachine dtal (LedgerDB.ReplayFromSnapshot snap tip' _ _) =
      mconcat [ "kind" .= String "ReplayFromSnapshot"
               , "snapshot" .= forMachine dtal snap
               , "tip" .= show tip' ]
  forMachine _dtal (LedgerDB.ReplayedBlock
                      pt
                      _ledgerEvents
                      _
                      (LedgerDB.ReplayGoal replayTo)) =
      mconcat [ "kind" .= String "ReplayedBlock"
               , "slot" .= unSlotNo (realPointSlot pt)
               , "tip"  .= withOrigin 0 unSlotNo (pointSlot replayTo) ]


docChainDBLedgerReplayEvent :: [DocMsg (ChainDB.TraceReplayEvent ev)]
docChainDBLedgerReplayEvent = [
      DocMsg
      (LedgerDB.ReplayFromGenesis anyProto)
      []
      "There were no LedgerDB snapshots on disk, so we're replaying all\
      \ blocks starting from Genesis against the initial ledger.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
    , DocMsg
      (LedgerDB.ReplayFromSnapshot anyProto anyProto anyProto anyProto)
      []
      "There was a LedgerDB snapshot on disk corresponding to the given tip.\
      \ We're replaying more recent blocks against it.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
    , DocMsg
      (LedgerDB.ReplayedBlock anyProto anyProto anyProto anyProto)
      []
      "We replayed the given block (reference) on the genesis snapshot\
      \ during the initialisation of the LedgerDB.\
      \\n\
      \ The @blockInfo@ parameter corresponds replayed block and the @replayTo@\
      \ parameter corresponds to the block at the tip of the ImmDB, i.e.,\
      \ the last block to replay."
    ]

--------------------------------------------------------------------------------
-- TraceImmutableDBEvent
--------------------------------------------------------------------------------

sevTraceImmutableDBEvent :: ImmDB.TraceEvent blk -> SeverityS
sevTraceImmutableDBEvent ImmDB.NoValidLastLocation {} = Info
sevTraceImmutableDBEvent ImmDB.ValidatedLastLocation {} = Info
sevTraceImmutableDBEvent (ImmDB.ChunkValidationEvent ev') =
  case ev' of
      ImmDB.StartedValidatingChunk{} -> Info
      ImmDB.ValidatedChunk{}         -> Info
      ImmDB.MissingChunkFile{}       -> Warning
      ImmDB.InvalidChunkFile {}      -> Warning
      ImmDB.MissingPrimaryIndex{}    -> Warning
      ImmDB.MissingSecondaryIndex{}  -> Warning
      ImmDB.InvalidPrimaryIndex{}    -> Warning
      ImmDB.InvalidSecondaryIndex{}  -> Warning
      ImmDB.RewritePrimaryIndex{}    -> Warning
      ImmDB.RewriteSecondaryIndex{}  -> Warning
sevTraceImmutableDBEvent ImmDB.ChunkFileDoesntFit{} = Warning
sevTraceImmutableDBEvent ImmDB.Migrating{}          = Debug
sevTraceImmutableDBEvent ImmDB.DeletingAfter{}      = Debug
sevTraceImmutableDBEvent ImmDB.DBAlreadyClosed{}    = Error
sevTraceImmutableDBEvent ImmDB.DBClosed{}           = Info
sevTraceImmutableDBEvent ImmDB.TraceCacheEvent{}    = Debug

namesForChainDBImmutableDBEvent :: ImmDB.TraceEvent blk -> [Text]
namesForChainDBImmutableDBEvent ImmDB.NoValidLastLocation =
    ["NoValidLastLocation"]
namesForChainDBImmutableDBEvent (ImmDB.ValidatedLastLocation {}) =
    ["ValidatedLastLocation"]

namesForChainDBImmutableDBEvent (ImmDB.ChunkValidationEvent ev) =
    "ChunkValidation" : namesForChainDBImmutableChunkValidation ev
namesForChainDBImmutableDBEvent (ImmDB.ChunkFileDoesntFit {}) =
    ["ChunkFileDoesntFit"]
namesForChainDBImmutableDBEvent (ImmDB.Migrating {}) =
    ["Migrating"]
namesForChainDBImmutableDBEvent (ImmDB.DeletingAfter {}) =
    ["DeletingAfter"]
namesForChainDBImmutableDBEvent ImmDB.DBAlreadyClosed =
    ["DBAlreadyClosed"]
namesForChainDBImmutableDBEvent ImmDB.DBClosed =
    ["DBClosed"]
namesForChainDBImmutableDBEvent (ImmDB.TraceCacheEvent ev') =
    "CacheEvent" : namesForChainDBImmutableDBCacheEvent ev'

namesForChainDBImmutableChunkValidation ::
     ImmDB.TraceChunkValidation blk ImmDB.ChunkNo
  -> [Text]
namesForChainDBImmutableChunkValidation (ImmDB.StartedValidatingChunk {}) =
    ["StartedValidatingChunk"]
namesForChainDBImmutableChunkValidation (ImmDB.ValidatedChunk {}) =
    ["ValidatedChunk"]
namesForChainDBImmutableChunkValidation (ImmDB.MissingChunkFile {}) =
    ["MissingChunkFile"]
namesForChainDBImmutableChunkValidation (ImmDB.InvalidChunkFile {}) =
    ["InvalidChunkFile"]
namesForChainDBImmutableChunkValidation (ImmDB.MissingPrimaryIndex {}) =
    ["MissingPrimaryIndex"]
namesForChainDBImmutableChunkValidation (ImmDB.MissingSecondaryIndex {}) =
    ["MissingSecondaryIndex"]
namesForChainDBImmutableChunkValidation (ImmDB.InvalidPrimaryIndex {}) =
    ["InvalidPrimaryIndex"]
namesForChainDBImmutableChunkValidation (ImmDB.InvalidSecondaryIndex {}) =
    ["InvalidSecondaryIndex"]
namesForChainDBImmutableChunkValidation (ImmDB.RewritePrimaryIndex {}) =
    ["RewritePrimaryIndex"]
namesForChainDBImmutableChunkValidation (ImmDB.RewriteSecondaryIndex {}) =
    ["RewriteSecondaryIndex"]


namesForChainDBImmutableDBCacheEvent :: ImmDB.TraceCacheEvent -> [Text]
namesForChainDBImmutableDBCacheEvent (ImmDB.TraceCurrentChunkHit {}) =
    ["CurrentChunkHit"]
namesForChainDBImmutableDBCacheEvent (ImmDB.TracePastChunkHit {}) =
    ["PastChunkHit"]
namesForChainDBImmutableDBCacheEvent (ImmDB.TracePastChunkMiss {}) =
    ["PastChunkMiss"]
namesForChainDBImmutableDBCacheEvent (ImmDB.TracePastChunkEvict {}) =
    ["PastChunkEvict"]
namesForChainDBImmutableDBCacheEvent (ImmDB.TracePastChunksExpired {}) =
    ["PastChunkExpired"]

instance (ConvertRawHash blk, StandardHash blk)
  => LogFormatting (ImmDB.TraceEvent blk) where
    forMachine _dtal ImmDB.NoValidLastLocation =
      mconcat [ "kind" .= String "NoValidLastLocation" ]
    forMachine _dtal (ImmDB.ValidatedLastLocation chunkNo immTip) =
      mconcat [ "kind" .= String "ValidatedLastLocation"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "immTip" .= String (renderTipHash immTip)
               , "blockNo" .= String (renderTipBlockNo immTip)
               ]
    forMachine dtal (ImmDB.ChunkValidationEvent traceChunkValidation) =
      forMachine dtal traceChunkValidation
    forMachine _dtal (ImmDB.DeletingAfter immTipWithInfo) =
      mconcat [ "kind" .= String "DeletingAfter"
               , "immTipHash" .= String (renderWithOrigin renderTipHash immTipWithInfo)
               , "immTipBlockNo" .= String (renderWithOrigin renderTipBlockNo immTipWithInfo)
               ]
    forMachine _dtal ImmDB.DBAlreadyClosed =
      mconcat [ "kind" .= String "DBAlreadyClosed" ]
    forMachine _dtal ImmDB.DBClosed =
      mconcat [ "kind" .= String "DBClosed" ]
    forMachine dtal (ImmDB.TraceCacheEvent cacheEv) =
      kindContext "TraceCacheEvent" $ forMachine dtal cacheEv
    forMachine _dtal (ImmDB.ChunkFileDoesntFit expectPrevHash actualPrevHash) =
      mconcat [ "kind" .= String "ChunkFileDoesntFit"
               , "expectedPrevHash" .= String (renderChainHash (Text.decodeLatin1 .
                                              toRawHash (Proxy @blk)) expectPrevHash)
               , "actualPrevHash" .= String (renderChainHash (Text.decodeLatin1 .
                                              toRawHash (Proxy @blk)) actualPrevHash)
               ]
    forMachine _dtal (ImmDB.Migrating txt) =
      mconcat [ "kind" .= String "Migrating"
               , "info" .= String txt
               ]

    forHuman ImmDB.NoValidLastLocation =
          "No valid last location was found. Starting from Genesis."
    forHuman (ImmDB.ValidatedLastLocation cn t) =
            "Found a valid last location at chunk "
          <> showT cn
          <> " with tip "
          <> renderRealPoint (ImmDB.tipToRealPoint t)
          <> "."
    forHuman (ImmDB.ChunkValidationEvent e) = case e of
          ImmDB.StartedValidatingChunk chunkNo outOf ->
               "Validating chunk no. " <> showT chunkNo <> " out of " <> showT outOf
            <> ". Progress: " <> showProgressT (max (chunkNoToInt chunkNo - 1) 0) (chunkNoToInt outOf) <> "%"
          ImmDB.ValidatedChunk chunkNo outOf ->
               "Validated chunk no. " <> showT chunkNo <> " out of " <> showT outOf
            <> ". Progress: " <> showProgressT (chunkNoToInt chunkNo) (chunkNoToInt outOf) <> "%"
          ImmDB.MissingChunkFile cn      ->
            "The chunk file with number " <> showT cn <> " is missing."
          ImmDB.InvalidChunkFile cn er    ->
            "The chunk file with number " <> showT cn <> " is invalid: " <> showT er
          ImmDB.MissingPrimaryIndex cn   ->
            "The primary index of the chunk file with number " <> showT cn <> " is missing."
          ImmDB.MissingSecondaryIndex cn ->
            "The secondary index of the chunk file with number " <> showT cn <> " is missing."
          ImmDB.InvalidPrimaryIndex cn   ->
            "The primary index of the chunk file with number " <> showT cn <> " is invalid."
          ImmDB.InvalidSecondaryIndex cn ->
            "The secondary index of the chunk file with number " <> showT cn <> " is invalid."
          ImmDB.RewritePrimaryIndex cn   ->
            "Rewriting the primary index for the chunk file with number " <> showT cn <> "."
          ImmDB.RewriteSecondaryIndex cn ->
            "Rewriting the secondary index for the chunk file with number " <> showT cn <> "."
    forHuman (ImmDB.ChunkFileDoesntFit ch1 ch2 ) =
          "Chunk file doesn't fit. The hash of the block " <> showT ch2 <> " doesn't match the previous hash of the first block in the current epoch: " <> showT ch1 <> "."
    forHuman (ImmDB.Migrating t) = "Migrating: " <> t
    forHuman (ImmDB.DeletingAfter wot) = "Deleting chunk files after " <> showT wot
    forHuman ImmDB.DBAlreadyClosed {} = "Immutable DB was already closed. Double closing."
    forHuman ImmDB.DBClosed {} = "Closed Immutable DB."
    forHuman (ImmDB.TraceCacheEvent ev') = "Cache event: " <> case ev' of
          ImmDB.TraceCurrentChunkHit   cn   curr -> "Current chunk hit: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunkHit      cn   curr -> "Past chunk hit: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunkMiss     cn   curr -> "Past chunk miss: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunkEvict    cn   curr -> "Past chunk evict: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunksExpired cns  curr -> "Past chunks expired: " <> showT cns <> ", cache size: " <> showT curr

instance ConvertRawHash blk => LogFormatting (ImmDB.TraceChunkValidation blk ImmDB.ChunkNo) where
    forMachine _dtal (ImmDB.RewriteSecondaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.RewriteSecondaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.RewritePrimaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.RewritePrimaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.MissingPrimaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.MissingPrimaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.MissingSecondaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.MissingSecondaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidPrimaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidPrimaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidSecondaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidSecondaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidChunkFile chunkNo
                      (ImmDB.ChunkErrHashMismatch hashPrevBlock prevHashOfBlock)) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrHashMismatch"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 , "hashPrevBlock" .= String (Text.decodeLatin1 . toRawHash (Proxy @blk) $ hashPrevBlock)
                 , "prevHashOfBlock" .= String (renderChainHash (Text.decodeLatin1 . toRawHash (Proxy @blk)) prevHashOfBlock)
                 ]
    forMachine dtal (ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrCorrupt pt)) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrCorrupt"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 , "block" .= String (renderPointForDetails dtal pt)
                 ]
    forMachine _dtal (ImmDB.ValidatedChunk chunkNo _) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.ValidatedChunk"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.MissingChunkFile chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.MissingChunkFile"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrRead readIncErr)) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrRead"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 , "error" .= String (showT readIncErr)
                 ]
    forMachine _dtal (ImmDB.StartedValidatingChunk initialChunk finalChunk) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.StartedValidatingChunk"
                 , "initialChunk" .= renderChunkNo initialChunk
                 , "finalChunk" .= renderChunkNo finalChunk
                 ]

instance LogFormatting ImmDB.TraceCacheEvent where
    forMachine _dtal (ImmDB.TraceCurrentChunkHit chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TraceCurrentChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkHit chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkMiss chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunkMiss"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkEvict chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunkEvict"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunksExpired chunkNos nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunksExpired"
                   , "chunkNos" .= String (Text.pack . show $ map renderChunkNo chunkNos)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]

docChainDBImmutableDBEvent :: [DocMsg (ImmDB.TraceEvent blk)]
docChainDBImmutableDBEvent = [
      DocMsg
      ImmDB.NoValidLastLocation
      []
      "No valid last location was found"
    , DocMsg
      (ImmDB.ValidatedLastLocation anyProto anyProto)
      []
      "The last location was validatet"
    , DocMsg
      (ImmDB.ChunkFileDoesntFit anyProto anyProto)
      []
      "The hash of the last block in the previous epoch doesn't match the\
      \ previous hash of the first block in the current epoch"
    , DocMsg
      (ImmDB.Migrating "")
      []
      "Performing a migration of the on-disk files."
    , DocMsg
      (ImmDB.DeletingAfter anyProto)
      []
      "Delete after"
    , DocMsg
      ImmDB.DBAlreadyClosed
      []
      "The immutable DB is already closed"
    , DocMsg
      ImmDB.DBClosed
      []
      "Closing the immutable DB"
    ]
    <> mapDoc ImmDB.ChunkValidationEvent docChainDBImmutableDBChunkValidation
    <> mapDoc ImmDB.TraceCacheEvent docChainDBImmutableDBCacheEvent

docChainDBImmutableDBChunkValidation ::
     [DocMsg (ImmDB.TraceChunkValidation blk ImmDB.ChunkNo)]
docChainDBImmutableDBChunkValidation = [
      DocMsg
      (ImmDB.MissingChunkFile anyProto)
      []
      "Chunk file is missing"
    , DocMsg
      (ImmDB.InvalidChunkFile anyProto anyProto)
      []
      "Chunk file is invalid"
    , DocMsg
      (ImmDB.MissingPrimaryIndex anyProto)
      []
      "The primary index is missing."
    , DocMsg
      (ImmDB.MissingSecondaryIndex anyProto)
      []
      "The secondary index is missing."
    , DocMsg
      (ImmDB.InvalidPrimaryIndex anyProto)
      []
      "The primary index is invalid."
    , DocMsg
      (ImmDB.InvalidSecondaryIndex anyProto)
      []
      ""
  ]

docChainDBImmutableDBCacheEvent :: [DocMsg ImmDB.TraceCacheEvent]
docChainDBImmutableDBCacheEvent = [
      DocMsg
      (ImmDB.TraceCurrentChunkHit anyProto anyProto)
      []
      "Current chunk found in the cache."
    , DocMsg
      (ImmDB.TracePastChunkHit anyProto anyProto)
      []
      "Past chunk found in the cache"
    , DocMsg
      (ImmDB.TracePastChunkMiss anyProto anyProto)
      []
      "Past chunk was not found in the cache"
    , DocMsg
      (ImmDB.TracePastChunkEvict anyProto anyProto)
      []
      "The least recently used past chunk was evicted because the cache\
      \ was full."
    , DocMsg
      (ImmDB.TracePastChunksExpired anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- VolatileDBEvent
--------------------------------------------------------------------------------

sevTraceVolatileDBEvent :: VolDB.TraceEvent blk -> SeverityS
sevTraceVolatileDBEvent _ = Debug

namesForChainDBVolatileDBEvent :: VolDB.TraceEvent blk -> [Text]
namesForChainDBVolatileDBEvent VolDb.DBAlreadyClosed =
    ["DBAlreadyClosed"]
namesForChainDBVolatileDBEvent (VolDb.Truncate {}) =
    ["Truncate"]
namesForChainDBVolatileDBEvent (VolDb.InvalidFileNames {}) =
    ["InvalidFileNames"]
namesForChainDBVolatileDBEvent (VolDb.BlockAlreadyHere {}) =
    ["BlockAlreadyHere"]


instance StandardHash blk => LogFormatting (VolDB.TraceEvent blk) where
    forMachine _dtal VolDB.DBAlreadyClosed =
      mconcat [ "kind" .= String "DBAlreadyClosed"]
    forMachine _dtal (VolDB.BlockAlreadyHere blockId) =
      mconcat [ "kind" .= String "BlockAlreadyHere"
               , "blockId" .= String (showT blockId)
               ]
    forMachine _dtal (VolDB.Truncate pErr fsPath blockOffset) =
      mconcat [ "kind" .= String "Truncate"
               , "parserError" .= String (showT pErr)
               , "file" .= String (showT fsPath)
               , "blockOffset" .= String (showT blockOffset)
               ]
    forMachine _dtal (VolDB.InvalidFileNames fsPaths) =
      mconcat [ "kind" .= String "InvalidFileNames"
               , "files" .= String (Text.pack . show $ map show fsPaths)
               ]

docChainDBVolatileDBEvent :: [DocMsg (VolDB.TraceEvent blk)]
docChainDBVolatileDBEvent = [
      DocMsg
      VolDB.DBAlreadyClosed
      []
      "When closing the DB it was found itis closed already."
    , DocMsg
      (VolDB.Truncate anyProto anyProto anyProto)
      []
      "Truncates a file up to offset because of the error."
    , DocMsg
      (VolDB.InvalidFileNames anyProto)
      []
      "Reports a list of invalid file paths."
    , DocMsg
      (VolDB.BlockAlreadyHere anyProto)
      []
      "A block was found to be already in the DB."
  ]

--------------------------------------------------------------------------------
-- Other orophans
--------------------------------------------------------------------------------

instance ( StandardHash blk
         , LogFormatting (ValidationErr (BlockProtocol blk))
         , LogFormatting (OtherHeaderEnvelopeError blk)
         )
      => LogFormatting (HeaderError blk) where
  forMachine dtal (HeaderProtocolError err) =
    mconcat
      [ "kind" .= String "HeaderProtocolError"
      , "error" .= forMachine dtal err
      ]
  forMachine dtal (HeaderEnvelopeError err) =
    mconcat
      [ "kind" .= String "HeaderEnvelopeError"
      , "error" .= forMachine dtal err
      ]

instance ( StandardHash blk
         , LogFormatting (OtherHeaderEnvelopeError blk)
         )
      => LogFormatting (HeaderEnvelopeError blk) where
  forMachine _dtal (UnexpectedBlockNo expect act) =
    mconcat
      [ "kind" .= String "UnexpectedBlockNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  forMachine _dtal (UnexpectedSlotNo expect act) =
    mconcat
      [ "kind" .= String "UnexpectedSlotNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  forMachine _dtal (UnexpectedPrevHash expect act) =
    mconcat
      [ "kind" .= String "UnexpectedPrevHash"
      , "expected" .= String (Text.pack $ show expect)
      , "actual" .= String (Text.pack $ show act)
      ]
  forMachine dtal (OtherHeaderEnvelopeError err) =
    forMachine dtal err


instance (   LogFormatting (LedgerError blk)
           , LogFormatting (HeaderError blk))
        => LogFormatting (ExtValidationError blk) where
    forMachine dtal (ExtValidationErrorLedger err) = forMachine dtal err
    forMachine dtal (ExtValidationErrorHeader err) = forMachine dtal err

    forHuman (ExtValidationErrorLedger err) =  forHuman err
    forHuman (ExtValidationErrorHeader err) =  forHuman err

    asMetrics (ExtValidationErrorLedger err) =  asMetrics err
    asMetrics (ExtValidationErrorHeader err) =  asMetrics err

instance LogFormatting LedgerDB.DiskSnapshot where
  forMachine DDetailed snap =
    mconcat [ "kind" .= String "snapshot"
             , "snapshot" .= String (Text.pack $ show snap) ]
  forMachine _ _snap = mconcat [ "kind" .= String "snapshot" ]



instance (Show (PBFT.PBftVerKeyHash c))
      => LogFormatting (PBFT.PBftValidationErr c) where
  forMachine _dtal (PBFT.PBftInvalidSignature text) =
    mconcat
      [ "kind" .= String "PBftInvalidSignature"
      , "error" .= String text
      ]
  forMachine _dtal (PBFT.PBftNotGenesisDelegate vkhash _ledgerView) =
    mconcat
      [ "kind" .= String "PBftNotGenesisDelegate"
      , "vk" .= String (Text.pack $ show vkhash)
      ]
  forMachine _dtal (PBFT.PBftExceededSignThreshold vkhash numForged) =
    mconcat
      [ "kind" .= String "PBftExceededSignThreshold"
      , "vk" .= String (Text.pack $ show vkhash)
      , "numForged" .= String (Text.pack (show numForged))
      ]
  forMachine _dtal PBFT.PBftInvalidSlot =
    mconcat
      [ "kind" .= String "PBftInvalidSlot"
      ]

instance (Show (PBFT.PBftVerKeyHash c))
      => LogFormatting (PBFT.PBftCannotForge c) where
  forMachine _dtal (PBFT.PBftCannotForgeInvalidDelegation vkhash) =
    mconcat
      [ "kind" .= String "PBftCannotForgeInvalidDelegation"
      , "vk" .= String (Text.pack $ show vkhash)
      ]
  forMachine _dtal (PBFT.PBftCannotForgeThresholdExceeded numForged) =
    mconcat
      [ "kind" .= String "PBftCannotForgeThresholdExceeded"
      , "numForged" .= numForged
      ]

instance (ConvertRawHash blk, StandardHash blk) => LogFormatting (ChainDB.TraceFollowerEvent blk) where
  forHuman ChainDB.NewFollower = "A new Follower was created"
  forHuman (ChainDB.FollowerNoLongerInMem _rrs) =
    "The follower was in the 'FollowerInMem' state but its point is no longer on\
    \ the in-memory chain fragment, so it has to switch to the\
    \ 'FollowerInImmutableDB' state"
  forHuman (ChainDB.FollowerSwitchToMem point slot) =
    "The follower was in the 'FollowerInImmutableDB' state and is switched to\
    \ the 'FollowerInMem' state. Point: " <> showT point <> " slot: " <> showT slot
  forHuman (ChainDB.FollowerNewImmIterator point slot) =
    "The follower is in the 'FollowerInImmutableDB' state but the iterator is\
    \ exhausted while the ImmDB has grown, so we open a new iterator to\
    \ stream these blocks too. Point: " <> showT point <> " slot: " <> showT slot

  forMachine _dtal ChainDB.NewFollower =
      mconcat [ "kind" .= String "NewFollower" ]
  forMachine _dtal (ChainDB.FollowerNoLongerInMem _) =
      mconcat [ "kind" .= String "FollowerNoLongerInMem" ]
  forMachine _dtal (ChainDB.FollowerSwitchToMem _ _) =
      mconcat [ "kind" .= String "FollowerSwitchToMem" ]
  forMachine _dtal (ChainDB.FollowerNewImmIterator _ _) =
      mconcat [ "kind" .= String "FollowerNewImmIterator" ]

instance ( ConvertRawHash blk
         , StandardHash blk
         , LogFormatting (LedgerError blk)
         , LogFormatting (RealPoint blk)
         , LogFormatting (OtherHeaderEnvelopeError blk)
         , LogFormatting (ExtValidationError blk)
         , LogFormatting (ValidationErr (BlockProtocol blk))
         )
      => LogFormatting (ChainDB.InvalidBlockReason blk) where
  forMachine dtal (ChainDB.ValidationError extvalerr) =
    mconcat
      [ "kind" .= String "ValidationError"
      , "error" .= forMachine dtal extvalerr
      ]
  forMachine dtal (ChainDB.InFutureExceedsClockSkew point) =
    mconcat
      [ "kind" .= String "InFutureExceedsClockSkew"
      , "point" .= forMachine dtal point
      ]
