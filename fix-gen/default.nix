{ mkDerivation, base, fix, genvalidity, genvalidity-bytestring
, genvalidity-sydtest, lib, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "fix-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base fix genvalidity genvalidity-bytestring
  ];
  testHaskellDepends = [ base fix genvalidity-sydtest sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
