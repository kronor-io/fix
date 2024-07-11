{ mkDerivation, base, bytestring, fix, genvalidity
, genvalidity-bytestring, genvalidity-sydtest, genvalidity-text
, genvalidity-time, lib, path, path-io, QuickCheck, sydtest
, sydtest-discover
}:
mkDerivation {
  pname = "fix-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fix genvalidity genvalidity-bytestring
    genvalidity-text genvalidity-time QuickCheck
  ];
  testHaskellDepends = [
    base bytestring fix genvalidity-sydtest path path-io sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
