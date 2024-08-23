{ mkDerivation, base, fix-spec, genvalidity, genvalidity-bytestring
, lib
}:
mkDerivation {
  pname = "fix-spec-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base fix-spec genvalidity genvalidity-bytestring
  ];
  license = "unknown";
}
