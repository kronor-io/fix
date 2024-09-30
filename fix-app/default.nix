{ mkDerivation, base, bytestring, conduit, conduit-extra, fix-spec
, lib, megaparsec, network, pretty-show, stm-chans, stm-conduit
, text, unliftio
}:
mkDerivation {
  pname = "fix-app";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit conduit-extra fix-spec megaparsec network
    pretty-show stm-chans stm-conduit text unliftio
  ];
  license = "unknown";
}
