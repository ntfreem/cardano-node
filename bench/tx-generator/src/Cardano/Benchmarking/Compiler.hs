{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.Benchmarking.Compiler
where

import           Prelude

import           Control.Applicative (liftA2)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.CPS

import           Data.Dependent.Sum ( (==>) )
import           Data.DList (DList)
import qualified Data.DList as DL

import           Cardano.Api
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.NixOptions
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store (Name(..), WalletName)
import           Cardano.Benchmarking.Script.Types

data CompileError where
  SomeCompilerError :: CompileError
  deriving (Show)
type Compiler a = RWST NixServiceOptions (DList Action) Int (Except CompileError) a

type SrcWallet = WalletName
type DstWallet = WalletName

compileOptions :: NixServiceOptions -> Either CompileError [Action]
compileOptions opts = runCompiler opts compileToScript

runCompiler ::NixServiceOptions -> Compiler () -> Either CompileError [Action]
runCompiler o c = case runExcept $ runRWST c o 0 of
  Left err -> Left err
  Right ((), _ , l) -> Right $ DL.toList l

testCompiler :: NixServiceOptions -> Compiler a -> Either CompileError (a, Int, [Action])
testCompiler o c = case runExcept $ runRWST c o 0 of
  Left err -> Left err
  Right (a, s , l) -> Right (a, s, DL.toList l)

compileToScript :: Compiler ()
compileToScript = do
  initConstants
  emit . StartProtocol =<< askNixOption getNodeConfigFile
  genesisWallet <- newWallet "genesis_wallet"
  importGenesisFunds genesisWallet
  collateralWallet <- addCollaterals genesisWallet
  splitWallet <- splittingPhase genesisWallet
  benchmarkingPhase splitWallet collateralWallet

initConstants :: Compiler ()
initConstants = do
  setN TTxAdditionalSize     _nix_add_tx_size
  setN TFee                  _nix_tx_fee
  setN TLocalSocket          _nix_localNodeSocketPath
  setConst  TTTL             1000000
  where
    setConst :: Tag v -> v -> Compiler ()
    setConst key val = emit $ Set $ key ==> val 

    setN :: Tag v -> (NixServiceOptions -> v) -> Compiler ()
    setN key s = askNixOption s >>= setConst key

importGenesisFunds :: DstWallet -> Compiler ()
importGenesisFunds wallet = do
  era <- askNixOption _nix_era
  cmd1 (ReadSigningKey $ KeyName "pass-partout") _nix_sigKey
  emit $ ImportGenesisFund era wallet LocalSocket (KeyName "pass-partout") (KeyName "pass-partout")
  delay

addCollaterals :: SrcWallet -> Compiler (Maybe WalletName)
addCollaterals src = do
  era <- askNixOption _nix_era
  isAnyPlutusMode >>= \case
    False -> return Nothing
    True -> do      
      safeCollateral <- _safeCollateral <$> evilFeeMagic
      collateralWallet <- newWallet "collateral_wallet"
      emit $ CreateChange era src LocalSocket
               (PayToAddr (KeyName "pass-partout") collateralWallet)
               (PayToAddr (KeyName "pass-partout") src)
               safeCollateral
               1
      return $ Just collateralWallet

splittingPhase :: SrcWallet -> Compiler DstWallet
splittingPhase srcWallet = do
  (NumberOfTxs tx_count) <- askNixOption _nix_tx_count
  (NumberOfInputsPerTx inputs_per_tx) <- askNixOption _nix_inputs_per_tx
  tx_fee <- askNixOption _nix_tx_fee
  era <- askNixOption _nix_era  
  minValuePerInput <- _minValuePerInput <$> evilFeeMagic
  splitSteps <- splitSequenceWalletNames srcWallet srcWallet $ unfoldSplitSequence tx_fee minValuePerInput (tx_count * inputs_per_tx)
  forM_ (init splitSteps) $ createChange era
  plutus <- isAnyPlutusMode
  (if plutus then createChangePlutus era else createChange era) $ last splitSteps
 where
  createChange :: AnyCardanoEra -> SplitStep -> Compiler DstWallet
  createChange era (src, dst, value, count) = do
     emit $ CreateChange era src LocalSocket (PayToAddr (KeyName "pass-partout") dst ) (PayToAddr  (KeyName "pass-partout") src) value count
     delay
     return dst

  createChangePlutus :: AnyCardanoEra -> SplitStep -> Compiler DstWallet
  createChangePlutus era (src, dst, value, count) = do
     autoMode <- isPlutusAutoMode
     scriptSpec <- if autoMode
       then ScriptSpec <$> askNixOption _nix_plutusLoopScript <*> pure AutoScript
       else do
         executionUnits <- ExecutionUnits <$> askNixOption _nix_executionMemory <*> askNixOption _nix_executionSteps
         debugMode <- askNixOption _nix_debugMode
         budget <- (if debugMode then CheckScriptBudget else StaticScriptBudget)
                     <$> (ScriptDataNumber <$> askNixOption _nix_plutusData)
                     <*> (ScriptDataNumber <$> askNixOption _nix_plutusRedeemer)
                     <*> pure executionUnits
         ScriptSpec <$> askNixOption _nix_plutusScript <*> pure budget
     emit $ CreateChange era src LocalSocket (PayToScript scriptSpec dst) (PayToScript scriptSpec src) value count
     delay
     return dst

-- Generate src and dst wallet names for a splitSequence.
-- testCompiler (error "opts") $ splitSequenceWalletNames (WalletName "w1") (WalletName "w2") (unfoldSplitSequence 1 1000 10000)
type SplitStep = (SrcWallet, DstWallet, Lovelace, Int)

splitSequenceWalletNames :: SrcWallet -> DstWallet -> [(Lovelace, Int)] -> Compiler [ SplitStep ]
splitSequenceWalletNames _src _dst [] = return []
splitSequenceWalletNames src dst [ (val,count) ] = return [( src, dst, val, count)]
splitSequenceWalletNames src dst ((val, count):rest) = do
  nextDst <- newWallet "change_wallet"
  l <- splitSequenceWalletNames dst nextDst rest
  return $ ( src, dst, val, count) : l

-- Return a list of splitSteps.
unfoldSplitSequence :: Lovelace -> Lovelace -> Int -> [(Lovelace, Int)]
unfoldSplitSequence fee value count
  = if count < maxOutputs
    then [
           -- Add an extra transaction that just contains the desired output and possible fees.
           (value * fromIntegral count + fee, 1)
         , (value, count )
         ]
    else unfoldSplitSequence fee (value * fromIntegral maxOutputs + fee) (count `div` maxOutputs + 1) ++ [ (value, count) ]
  where
    -- maximal number of outputs in a TX.
    -- todo: this must be in sync with Scipt/Core.hs
    maxOutputs = 30
    
benchmarkingPhase :: WalletName -> Maybe WalletName -> Compiler ()
benchmarkingPhase wallet collateralWallet = do
  debugMode <- askNixOption _nix_debugMode
  targetNodes <- askNixOption _nix_targetNodes
  extraArgs <- evilValueMagic
  tps <- askNixOption _nix_tps
  era <- askNixOption _nix_era
  let target = if debugMode then LocalSocket else NodeToNode targetNodes
  emit $ RunBenchmark era wallet target (ThreadName "tx-submit-benchmark") extraArgs collateralWallet tps
  unless debugMode $ do
    emit $ WaitBenchmark $ ThreadName "tx-submit-benchmark"

data Fees = Fees {
    _safeCollateral :: Lovelace
  , _minValuePerInput :: Lovelace
  }
  
evilFeeMagic :: Compiler Fees
evilFeeMagic = do
  (Quantity tx_fee) <- lovelaceToQuantity <$> askNixOption _nix_tx_fee
  plutusMode <- askNixOption _nix_plutusMode  
  (NumberOfInputsPerTx inputs_per_tx) <- askNixOption _nix_inputs_per_tx
  (NumberOfOutputsPerTx outputs_per_tx) <- askNixOption _nix_outputs_per_tx  
  (Quantity min_utxo_value)  <- lovelaceToQuantity <$> askNixOption _nix_min_utxo_value
  let
    scriptFees = 5000000;
    collateralPercentage = 200;

    totalFee = if plutusMode
               then tx_fee + scriptFees * fromIntegral inputs_per_tx
               else tx_fee;
    safeCollateral = max ((scriptFees + tx_fee) * collateralPercentage `div` 100) min_utxo_value;
    minTotalValue = min_utxo_value * fromIntegral outputs_per_tx + totalFee;
    minValuePerInput = minTotalValue `div` fromIntegral inputs_per_tx + 1;
  return $ Fees {
      _safeCollateral = fromIntegral safeCollateral
    , _minValuePerInput = fromIntegral minValuePerInput
    }

emit :: Action -> Compiler ()
emit = tell . DL.singleton

cmd1 :: (v -> Action) -> (NixServiceOptions -> v) -> Compiler ()
cmd1 cmd arg = emit . cmd =<< askNixOption arg
  
askNixOption :: (NixServiceOptions -> v) -> Compiler v
askNixOption = asks

delay :: Compiler ()
delay = cmd1 Delay _nix_init_cooldown

isPlutusMode :: Compiler Bool
isPlutusMode = askNixOption _nix_plutusMode

isPlutusAutoMode :: Compiler Bool
isPlutusAutoMode = askNixOption _nix_plutusAutoMode

isAnyPlutusMode :: Compiler Bool
isAnyPlutusMode = liftA2 (||) isPlutusMode isPlutusAutoMode

newIdentifier :: String -> Compiler String
newIdentifier prefix = do
  n <- get
  put $ succ n
  return $ prefix ++ "_" ++ show n

newWallet :: String -> Compiler WalletName
newWallet n = do
  name <- WalletName <$> newIdentifier n
  emit $ InitWallet name
  return name

-- Approximate the ada values for inputs of the benchmarking Phase
evilValueMagic :: Compiler RunBenchmarkAux
evilValueMagic = do
  (NumberOfInputsPerTx inputsPerTx) <- askNixOption _nix_inputs_per_tx
  (NumberOfOutputsPerTx outputsPerTx) <- askNixOption _nix_outputs_per_tx
  (NumberOfTxs txCount) <- askNixOption _nix_tx_count
  fee <- askNixOption _nix_tx_fee
  minValuePerUTxO <- askNixOption _nix_min_utxo_value
  let
    (Quantity minValue) = lovelaceToQuantity $ fromIntegral outputsPerTx * minValuePerUTxO + fee

  -- this is not totally correct:
  -- beware of rounding errors !
    minValuePerInput = quantityToLovelace $ fromIntegral (if m==0 then d else d+1)
      where
        (d, m) = minValue `divMod` fromIntegral inputsPerTx
  return $  RunBenchmarkAux {
      auxTxCount = txCount
    , auxFee = fee
    , auxOutputsPerTx = outputsPerTx
    , auxInputsPerTx = inputsPerTx
    , auxInputs = inputsPerTx * txCount
    , auxOutputs = inputsPerTx * txCount
    , auxMinValuePerUTxO = minValuePerInput
    }

