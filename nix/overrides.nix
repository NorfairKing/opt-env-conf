{ lib
, haskell
, symlinkJoin
, gzip
, runCommand
, writeShellApplication
, ...
}:
with lib;
with haskell.lib;
self: _:
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
    }));

  installManpage = exeName: drv: overrideCabal drv (old: {
    postInstall = (old.postInstall or "") + ''
      mkdir -p $out/share/man/man1/
      export NO_COLOR=1
      ''${!outputBin}/bin/${exeName} --render-man-page > ${exeName}.1
      ${gzip}/bin/gzip -9 -c ${exeName}.1 > $out/share/man/man1/${exeName}.1.gz
    '';
  });
  installManpages = exeNames: drv:
    foldr installManpage drv exeNames;
  installCompletion = exeName: drv:
    overrideCabal drv (old: {
      postInstall = (old.postInstall or "") + ''
        bashCompDir="''${!outputBin}/share/bash-completion/completions"
        zshCompDir="''${!outputBin}/share/zsh/vendor-completions"
        fishCompDir="''${!outputBin}/share/fish/vendor_completions.d"
        mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
        "''${!outputBin}/bin/${exeName}" --bash-completion-script "''${!outputBin}/bin/${exeName}" >"$bashCompDir/${exeName}"
        "''${!outputBin}/bin/${exeName}" --zsh-completion-script "''${!outputBin}/bin/${exeName}" >"$zshCompDir/_${exeName}"
        "''${!outputBin}/bin/${exeName}" --fish-completion-script "''${!outputBin}/bin/${exeName}" >"$fishCompDir/${exeName}.fish"

        # Sanity check
        grep -F ${exeName} <$bashCompDir/${exeName} >/dev/null || {
          echo 'Could not find ${exeName} in completion script.'
          exit 1
        }
      '';
    });
  installCompletions = exeNames: drv:
    foldr installCompletion drv exeNames;
  installManpagesAndCompletions = exeNames: drv:
    installManpages exeNames (installCompletions exeNames drv);

  makeSettingsCheckScript = name: capabilities: exe: args: env: writeShellApplication {
    inherit name;
    runtimeEnv = env;
    # Because we set PATH sometimes
    excludeShellChecks = [ "SC2123" ];
    text =
      let
        capabilityArgs = lib.mapAttrsToList
          (cap: bool:
            if bool
            then ""
            else "--settings-capabilities-disable-" + cap)
          capabilities;
        capabilityArgsStr = concatStringsSep " " capabilityArgs;
      in
      ''
        ${exe} --run-settings-check ${capabilityArgsStr} ${concatStringsSep " " args}
      '';
  };

  # A nix build that does a settings check.
  # Note that this will only work if the option parsing does not rely on any
  # files not available in the nix build sandbox.
  # If it does, you'll need to use 'makeSettingsCheckScript' and run it outside
  # of the build sandbox, for example in an activation script.
  makeSettingsCheck = name: capabilities: exe: args: env: runCommand name env ''
    ${makeSettingsCheckScript name capabilities exe args env}/bin/${name} > "$out"
  '';

  makeSettingsCheckHomeManagerActivationScript = name: capabilities: exe: args: env: {
    after = [ "writeBoundary" ];
    before = [ ];
    data = ''
      run ${makeSettingsCheckScript name capabilities exe args env}/bin/${name}
    '';
  };

  # Note to reader: If you find code while debugging a build failure, please
  # contribute a more accurate version of this function:
  addSettingsCheckToService = capabilities: service: lib.recursiveUpdate service {
    documentation =
      let
        path = service.path or [ ];
        pathVar = makeSearchPath "bin" path;
        environment = ((service.environment or { }) // { PATH = pathVar; });
        check = makeSettingsCheck
          (service.name or "settings-check")
          capabilities
          (last (init (splitString "\n" service.script)))
          (service.scriptArgs or [ ]) # TODO is this right?
          environment;
      in
      (service.documentation or [ ]) ++ [ "${check}" ];
  };
  # For home-manager:
  addSettingsCheckToUserService = capabilities: service: lib.recursiveUpdate service
    {
      Unit.Documentation =
        let
          path = service.path or [ ];
          pathVar = makeSearchPath "bin" path;
          environment = ((service.environment or { }) // { PATH = pathVar; });
          check = makeSettingsCheck
            (service.name or "settings-check")
            capabilities
            service.Service.ExecStart
            [ ] # TODO is this right?
            environment;
        in
        (service.Unit.Documentation or [ ]) ++ [ "${check}" ];
    };
  opt-env-conf = overrideCabal (optEnvConfPkg "opt-env-conf") (old: {
    passthru = {
      inherit
        installManpage
        installManpages
        installCompletion
        installCompletions
        installManpagesAndCompletions
        makeSettingsCheckScript
        makeSettingsCheck
        makeSettingsCheckHomeManagerActivationScript
        addSettingsCheckToService
        addSettingsCheckToUserService;
    } // (old.passthru or { });
  });


  optEnvConfPackages = {
    inherit opt-env-conf;
    opt-env-conf-test = optEnvConfPkg "opt-env-conf-test";
    opt-env-conf-example =
      self.opt-env-conf.installManpagesAndCompletions
        [ "opt-env-conf-example" ]
        (optEnvConfPkg "opt-env-conf-example");
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
