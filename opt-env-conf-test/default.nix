{ mkDerivation, aeson, base, containers, genvalidity
, genvalidity-aeson, genvalidity-containers, genvalidity-sydtest
, genvalidity-text, lib, mtl, opt-env-conf, path, pretty-show
, QuickCheck, safe-coloured-text, sydtest, sydtest-discover, text
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
    aeson base containers genvalidity-aeson genvalidity-sydtest
    genvalidity-text mtl opt-env-conf path pretty-show QuickCheck
    safe-coloured-text sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "opt-env-conf-example";
}
