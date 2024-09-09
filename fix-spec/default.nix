{ mkDerivation, base, bytestring, conduit, dlist, fix-core, lib
, megaparsec, mtl, stm, stm-chans, stm-conduit, text, validity
}:
mkDerivation {
  pname = "fix-spec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit dlist fix-core megaparsec mtl stm stm-chans
    stm-conduit text validity
  ];
  license = "unknown";
}
