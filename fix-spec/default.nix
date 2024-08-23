{ mkDerivation, base, bytestring, fix-core, lib, validity }:
mkDerivation {
  pname = "fix-spec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring fix-core validity ];
  license = "unknown";
}
