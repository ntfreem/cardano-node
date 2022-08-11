let
  basePort              = 30000;
  cacheDirDefault       = "${__getEnv "HOME"}/.cache/cardano-workbench";
  stateDir              = "run/current";
in
{ pkgs
, lib
, cardano-world
, workbench
##
, cacheDir              ? cacheDirDefault
, extraBackendConfig    ? {}
, useCabalRun           ? false
, enableEKG             ? true
##
, ...
}:
with lib;
let
  backend =
    rec
    { name = "docker";

      services-config = import ./services-config.nix {inherit lib workbench basePort stateDir useCabalRun enableEKG;};

      materialise-profile =
        { profileNix }:
          pkgs.runCommand "workbench-backend-output-${profileNix.name}-${name}d"
            { buildInputs = [ workbench.workbench ];
              cardanoNodeImageName = cardano-world.x86_64-linux.cardano.oci-images.cardano-node.imageName;
              cardanoNodeImageTag = cardano-world.x86_64-linux.cardano.oci-images.cardano-node.imageTag;
            }
            ''
            mkdir $out
            echo $cardanoNodeImageName              > $out/cardanoNodeImageName
            echo $cardanoNodeImageTag               > $out/cardanoNodeImageTag
            wb app compose ${profileNix.JSON} ${profileNix.node-specs.JSON} $cardanoNodeImageName $cardanoNodeImageTag > $out/docker-compose.yaml
            '';
    };
in
{
  inherit cacheDir stateDir basePort;
  inherit workbench;
  inherit backend;
}