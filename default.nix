{ nixpkgsSrc ? <nixpkgs>
}:

with builtins;
let
  haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/2f3ec7bbf2e790122ca276a4ab038bf2749c6c93.tar.gz") {};
  pkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;
  project = pkgs.haskell-nix.cabalProject {
    name = "refurb";
    src = ./.;
    cabalProject = readFile ./cabal.project;
    compiler-nix-name = "ghc925";
    index-state = "2022-12-01T00:00:00Z";
    modules = [
      {
        packages.postgresql-libpq = {
          preConfigure = ''
            export PATH="${pkgs.pkg-config}/bin:$PATH"
          '';
          flags."use-pkg-config" = true;
        };
      }
    ];
  };
  shell = project.shellFor {
    tools = {
      cabal = "3.6.2.0";
    };
    nativeBuildInputs = [
      pkgs.pkg-config
    ];
    exactDeps = true;
  };
in { inherit project shell; }
