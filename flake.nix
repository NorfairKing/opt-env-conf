{
  description = "opt-env-conf";
  nixConfig = {
    extra-substituters = "https://foobar.cachix.org";
    extra-trusted-public-keys = "foobar.cachix.org-1:srabhQPgZR0EO+bOppsCWbesHOgk8ABakPL8D1h5wOU=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.11";
    nixpkgs-24_11.url = "github:NixOS/nixpkgs?ref=nixos-24.11";
    nixpkgs-24_05.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    nixpkgs-23_11.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    horizon-advance.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-advance";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-24_11
    , nixpkgs-24_05
    , nixpkgs-23_11
    , horizon-advance
    , pre-commit-hooks
    , validity
    , autodocodec
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , weeder-nix
    , dekking
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgs = nixpkgsFor nixpkgs;
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (dekking + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      horizonPkgs = horizon-advance.legacyPackages.${system}.extend allOverrides;
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend allOverrides;
      haskellPackages = haskellPackagesFor nixpkgs;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = haskellPackages.optEnvConfRelease;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs: (haskellPackagesFor nixpkgs).optEnvConfRelease;
          allNixpkgs = {
            inherit
              nixpkgs-24_11
              nixpkgs-24_05
              nixpkgs-23_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          forwardCompatibility = horizonPkgs.optEnvConfRelease;
          release = self.packages.${system}.default;
          shell = self.devShells.${system}.default;
          coverage-report = haskellPackages.dekking.makeCoverageReport {
            name = "test-coverage-report";
            packages = [
              "opt-env-conf"
            ];
            coverage = [
              "opt-env-conf-test"
              "opt-env-conf-example"
            ];
            needToBeLinkedAgainstDekkingValue = [
              "sydtest"
              "genvalidity-sydtest"
            ];
          };
          # weeder-check = pkgs.weeder-nix.makeWeederCheck {
          #   weederToml = ./weeder.toml;
          #   packages = [
          #     "opt-env-conf"
          #     "opt-env-conf-test"
          #     "opt-env-conf-example"
          #   ];
          #   inherit haskellPackages;
          # };
          example-settings-check =
            haskellPackages.opt-env-conf.makeSettingsCheck
              "example-settings-check"
              "${haskellPackages.opt-env-conf-example}/bin/opt-env-conf-example"
              [ "read" ]
              { };
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [
                ".*/default.nix"
                ".*/options.nix"
                "opt-env-conf-test/test_resources.*.nix"
              ];
              deadnix.enable = true;
              deadnix.excludes = [
                ".*/default.nix"
                ".*/options.nix"
                "opt-env-conf-test/test_resources.*.nix"
              ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = haskellPackages.shellFor {
        name = "opt-env-conf-shell";
        packages = p: builtins.attrValues p.optEnvConfPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          mandoc
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nix-ci.cachix = {
        name = "opt-env-conf";
        public-key = "opt-env-conf.cachix.org-1:gkENPxoLqJMYgYsFOCCbA3wr3MkNfN5bdDQPjs4QHlU=";
      };
    };
}
