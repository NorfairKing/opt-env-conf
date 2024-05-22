{
  description = "opt-env-conf";
  nixConfig = {
    extra-substituters = "https://foobar.cachix.org";
    extra-trusted-public-keys = "foobar.cachix.org-1:srabhQPgZR0EO+bOppsCWbesHOgk8ABakPL8D1h5wOU=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , weeder-nix
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (dekking + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.optEnvConfRelease;
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [ "opt-env-conf" ];
        };
        # weeder-check = pkgs.weeder-nix.makeWeederCheck {
        #   weederToml = ./weeder.toml;
        #   packages = [ "opt-env-conf" ];
        # };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "opt-env-conf-shell";
        packages = p: builtins.attrValues p.optEnvConfPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
