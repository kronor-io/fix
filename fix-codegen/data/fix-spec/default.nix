{ mkDerivation, base, bytestring, fix-core, lib, mtl, text
, validity
}:
mkDerivation {
  pname = "fix-spec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fix-core mtl text validity
  ];
  license = "unknown";
}
