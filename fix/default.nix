{ mkDerivation, base, bytestring, lib, megaparsec, validity
, validity-bytestring
}:
mkDerivation {
  pname = "fix";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring megaparsec validity validity-bytestring
  ];
  license = "unknown";
}
