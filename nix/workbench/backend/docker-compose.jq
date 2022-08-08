{

  "services": (
    (
        .
      |
        with_entries ( {
            key: .key
          , value: {
                pull_policy: "never"
              , image: "${WB_NODE_IMAGE_NAME:-\($nodeImageName)}:${WB_NODE_IMAGE_TAG:-\($nodeImageTag)}"
              , profiles: [ "node" ]
              , restart: "no"
              , networks: {
                  "cardano-cluster": {
                    ipv4_address: "172.22.\(.value.i / 254 | floor).\(.value.i % 254 + 1)"
                  }
                }
              , ports: [ "\(.value.port):\(.value.port)" ]
              , volumes: [
                    "NODE-\(.value.name):/var/cardano-node:rw"
                  , "GENESIS:/var/cardano-node/genesis:ro"
                ]
              , environment: [
                  "HOST_ADDR=172.22.\(.value.i / 254 | floor).\(.value.i % 254 + 1)"
                , "PORT=\(.value.port)"
                , "DATA_DIR=/var/cardano-node"
                , "NODE_CONFIG=/var/cardano-node/config.json"
                , "NODE_TOPOLOGY=/var/cardano-node/topology.json"
                , "SOCKET_PATH=/var/cardano-node/node.socket"
                , "TRACER_SOCKET_PATH=/var/cardano-tracer/tracer.socket"
                , "RTS_FLAGS=+RTS -N2 -I0 -A16m -qg -qb --disable-delayed-os-memory-return -RTS"
                , "SHELLEY_KES_KEY=/var/cardano-node/genesis/node-keys/node-kes\(.value.i).skey"
                , "SHELLEY_VRF_KEY=/var/cardano-node/genesis/node-keys/node-vrf\(.value.i).skey"
                , "SHELLEY_OPCERT=/var/cardano-node/genesis/node-keys/node\(.value.i).opcert"
              ]
              , logging: {driver: "json-file"}
            }
        } )
    )
  )

  , "networks": {
    "cardano-cluster": {
      # Networks and volumes defined as `external` are never removed.
        external: false
      , attachable: true
      , driver: "bridge"
      , driver_opts: {}
      , enable_ipv6: false
      , ipam: {
          driver: "default"
        , config: [{
          # Network Address:	    172.20.0.0
          # Subnet Mask:	        255.252.0.0
          # Wildcard Mask:	      0.3.255.255
          # Broadcast Address:	  172.23.255.255
          # Usable Host IP Range:	172.20.0.1 - 172.23.255.254
            subnet: "172.20.0.0/14"
          , ip_range: "172.20.0.0/14"
          , gateway: "172.20.255.254"
          , aux_addresses: {}
        }]
      }
    }
  }

  , volumes: (
      (
          .
        |
          with_entries (
            {
                key: "NODE-\(.value.name)"
              , value: {
                  # Networks and volumes defined as `external` are never removed.
                    external: false
                  , driver: "local"
                  , driver_opts: {
                        type: "none"
                      , o: "bind"
                      , device: "${WB_RUNDIR:-./run/current}/\(.value.name)"
                  }
              }
            }
          )
      )
    +
      {GENESIS:
        {
          # Networks and volumes defined as `external` are never removed.
            external: false
          , driver: "local"
          , driver_opts: {
                type: "none"
              , o: "bind"
              , device: "${WB_RUNDIR:-./run/current}/genesis"
          }
        }
      }
  )

}
