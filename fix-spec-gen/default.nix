{ mkDerivation, base, fix-core-gen, fix-spec, genvalidity
, genvalidity-bytestring, lib, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "fix-spec-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base fix-spec genvalidity genvalidity-bytestring
  ];
  testHaskellDepends = [ base fix-core-gen fix-spec sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
