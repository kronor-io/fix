{ mkDerivation, base, bytestring, fix-core, genvalidity
, genvalidity-bytestring, genvalidity-sydtest, genvalidity-text
, genvalidity-time, lib, path, path-io, QuickCheck, sydtest
, sydtest-discover, time
}:
mkDerivation {
  pname = "fix-core-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fix-core genvalidity genvalidity-bytestring
    genvalidity-text genvalidity-time QuickCheck
  ];
  testHaskellDepends = [
    base bytestring fix-core genvalidity-sydtest path path-io sydtest
    time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
