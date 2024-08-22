{ mkDerivation, base, bytestring, fix-core, lib, text, time
, validity, validity-bytestring, validity-text, validity-time
}:
mkDerivation {
  pname = "fix-spec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring fix-core text time validity validity-bytestring
    validity-text validity-time
  ];
  license = "unknown";
}
