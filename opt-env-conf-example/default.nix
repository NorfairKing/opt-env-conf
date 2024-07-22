{ mkDerivation, base, lib, opt-env-conf, opt-env-conf-test, sydtest
, sydtest-discover, text
}:
mkDerivation {
  pname = "opt-env-conf-example";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base opt-env-conf text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "opt-env-conf-example";
}
