{ mkDerivation, base, lib, opt-env-conf, path, path-io, xml-conduit
}:
mkDerivation {
  pname = "fix-codegen";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base opt-env-conf path path-io xml-conduit
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "fix-codegen";
}
