{ mkDerivation, base, bytestring, fix, genvalidity
, genvalidity-bytestring, genvalidity-sydtest, lib, QuickCheck
, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "fix-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fix genvalidity genvalidity-bytestring QuickCheck
  ];
  testHaskellDepends = [
    base bytestring fix genvalidity-sydtest sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
