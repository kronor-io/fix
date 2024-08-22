{ mkDerivation, base, bytestring, lib, megaparsec, mtl, text, time
, validity, validity-bytestring, validity-text, validity-time
}:
mkDerivation {
  pname = "fix-core";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring megaparsec mtl text time validity
    validity-bytestring validity-text validity-time
  ];
  license = "unknown";
}