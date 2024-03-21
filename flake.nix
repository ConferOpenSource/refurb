{
  inputs = {
    composite-base = {
      url = "github:composite-hs/composite-base?rev=d946fb096f777cb99482b0f8899d6aff13811932";
      flake = false;
    };
    composite = {
      url = "github:composite-hs/composite?rev=e90f04c67bd273d5d52daa55594b6c753dadfdaf";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "nixpkgs/release-23.11";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["aarch64-linux" "x86_64-linux"];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {
        self',
        system,
        lib,
        config,
        pkgs,
        ...
      }: {
        _module.args.pkgs = inputs.nixpkgs.legacyPackages.${system};
        devShells.default = pkgs.mkShell {
          name = "refurb-devshell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
        };
        haskellProjects.default = {
          autoWire = [];
          devShell.tools = hp: {inherit (hp) fourmolu;};
          packages = {
            composite-base.source = inputs.composite-base;
            composite-opaleye.source = inputs.composite + "/composite-opaleye";
          };
          settings = {
            composite-base.jailbreak = true;
            composite-opaleye.jailbreak = true;
          };
        };
        packages = rec {
          default = refurb;
          refurb = config.haskellProjects.default.outputs.packages.refurb.package;
          refurb-example = config.haskellProjects.default.outputs.packages.refurb-example.package;
        };
      };
    };
}
