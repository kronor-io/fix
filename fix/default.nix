{ mkDerivation, base, bytestring, lib, validity
, validity-bytestring
}:
mkDerivation {
  pname = "fix";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring validity validity-bytestring
  ];
  license = "unknown";
}
