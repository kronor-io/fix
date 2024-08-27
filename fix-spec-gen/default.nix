{ mkDerivation, base, bytestring, fix-core, fix-core-gen, fix-spec
, genvalidity, genvalidity-bytestring, genvalidity-sydtest, lib
, path, path-io, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "fix-spec-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fix-core fix-core-gen fix-spec genvalidity
    genvalidity-bytestring genvalidity-sydtest path path-io sydtest
  ];
  testHaskellDepends = [ base fix-core-gen fix-spec sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
