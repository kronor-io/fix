{ mkDerivation, base, bytestring, fix-core, genvalidity
, genvalidity-bytestring, genvalidity-sydtest, genvalidity-text
, genvalidity-time, lib, QuickCheck, sydtest, sydtest-discover
, time
}:
mkDerivation {
  pname = "fix-core-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fix-core genvalidity genvalidity-bytestring
    genvalidity-sydtest genvalidity-text genvalidity-time QuickCheck
    sydtest
  ];
  testHaskellDepends = [
    base bytestring fix-core genvalidity-sydtest sydtest time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
