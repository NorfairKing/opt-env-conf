{ lib
, haskell
, symlinkJoin
, gzip
, runCommand
, ...
}:
with lib;
with haskell.lib;
self: super:
let
  optEnvConfPkg = name:
    buildFromSdist (overrideCabal (self.callPackage (../${name}/default.nix) { }) (old: {
      configureFlags = (old.configureFlags or [ ]) ++ [
        # Optimisations
        "--ghc-options=-O2"
        # Extra warnings
        "--ghc-options=-Wall"
        "--ghc-options=-Wincomplete-uni-patterns"
        "--ghc-options=-Wincomplete-record-updates"
        "--ghc-options=-Wpartial-fields"
        "--ghc-options=-Widentities"
        "--ghc-options=-Wredundant-constraints"
        "--ghc-options=-Wcpp-undef"
        "--ghc-options=-Werror"
      ];
      doBenchmark = true;
      doCoverage = false;
      doCheck = false; # Only for coverage
      # Ugly hack because we can't just add flags to the 'test' invocation.
      # Show test output as we go, instead of all at once afterwards.
      testTarget = (old.testTarget or "") + " --show-details=direct";
    }));

  installManpage = exeName: drv: overrideCabal drv (old: {
    postInstall = (drv.postInstall or "") + ''
      mkdir -p $out/share/man/man1/
      export NO_COLOR=1
      ''${!outputBin}/bin/${exeName} --render-man-page > ${exeName}.1
      ${gzip}/bin/gzip -9 -c ${exeName}.1 > $out/share/man/man1/${exeName}.1.gz
    '';
  });
  installManpages = exeNames: drv:
    foldr installManpage drv exeNames;
  installCompletion = exeName: drv:
    overrideCabal drv (old: { });
  installCompletions = exeNames: drv:
    foldr installCompletion drv exeNames;
  installManpagesAndCompletions = exeNames: drv:
    installManpages exeNames (installCompletions exeNames drv);

  mkSettingsCheck = name: exe: args: env: runCommand name env ''
    ${exe} --run-settings-check ${concatStringsSep " " args} > $out
  '';

  # Note to reader: If you find code while debugging a build failure, please
  # contribute a more accurate version of this function:
  addSettingsCheckToService = service: service // {
    documentation =
      let
        check = mkSettingsCheck
          (service.name or "settings-check")
          (last (init (splitString "\n" service.script)))
          (service.scriptArgs or [ ]) # TODO is this right?
          (service.environment or { });
      in
      (service.documentation or [ ]) ++ [ "${check}" ];
  };

  opt-env-conf = overrideCabal (optEnvConfPkg "opt-env-conf") (old: {
    passthru = {
      inherit
        installManpage
        installManpages
        installCompletion
        installCompletions
        installManpagesAndCompletions
        mkSettingsCheck
        addSettingsCheckToService;
    } // (old.passthru or { });
  });

  opt-env-conf-test =
    installManpagesAndCompletions [ "opt-env-conf-example" ]
      (optEnvConfPkg "opt-env-conf-test");

  optEnvConfPackages = {
    inherit
      opt-env-conf
      opt-env-conf-test;
  };
in
{
  inherit optEnvConfPackages;

  optEnvConfRelease =
    symlinkJoin {
      name = "opt-env-conf-release";
      paths = attrValues self.optEnvConfPackages;
      passthru = self.optEnvConfPackages;
    };
} // optEnvConfPackages
