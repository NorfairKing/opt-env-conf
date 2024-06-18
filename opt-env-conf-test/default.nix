{ mkDerivation, aeson, base, containers, envparse, genvalidity
, genvalidity-aeson, genvalidity-containers, genvalidity-sydtest
, genvalidity-text, lib, opt-env-conf, optparse-applicative
, pretty-show, QuickCheck, safe-coloured-text
, safe-coloured-text-terminfo, sydtest, sydtest-discover, text
, yaml
}:
mkDerivation {
  pname = "opt-env-conf-test";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers opt-env-conf
    safe-coloured-text sydtest text
  ];
  executableHaskellDepends = [ base opt-env-conf ];
  testHaskellDepends = [
    aeson base containers envparse genvalidity genvalidity-aeson
    genvalidity-containers genvalidity-sydtest genvalidity-text
    opt-env-conf optparse-applicative pretty-show QuickCheck
    safe-coloured-text safe-coloured-text-terminfo sydtest text yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "opt-env-conf-example";
}
