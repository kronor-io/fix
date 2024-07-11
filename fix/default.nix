{ mkDerivation, base, bytestring, lib, megaparsec, text, validity
, validity-bytestring, validity-text
}:
mkDerivation {
  pname = "fix";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring megaparsec text validity validity-bytestring
    validity-text
  ];
  license = "unknown";
}
