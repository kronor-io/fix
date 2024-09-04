{ mkDerivation, base, bytestring, conduit, dlist, fix-core, lib
, megaparsec, mtl, text, validity
}:
mkDerivation {
  pname = "fix-spec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit dlist fix-core megaparsec mtl text validity
  ];
  license = "unknown";
}
