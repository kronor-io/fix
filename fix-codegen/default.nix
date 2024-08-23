{ mkDerivation, base, containers, lib, opt-env-conf, path, path-io
, process, template-haskell, text, xml-conduit
}:
mkDerivation {
  pname = "fix-codegen";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers opt-env-conf path path-io process template-haskell
    text xml-conduit
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "fix-codegen";
}
