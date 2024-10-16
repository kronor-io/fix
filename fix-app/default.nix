{ mkDerivation, base, bytestring, conduit, conduit-extra
, containers, fix-core, fix-spec, lib, megaparsec, network
, pretty-show, stm-chans, stm-conduit, text, time, tls, unliftio
}:
mkDerivation {
  pname = "fix-app";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit conduit-extra containers fix-core fix-spec
    megaparsec network pretty-show stm-chans stm-conduit text time tls
    unliftio
  ];
  license = "unknown";
}
