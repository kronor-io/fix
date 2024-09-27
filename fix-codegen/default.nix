{ mkDerivation, base, bytestring, containers, lib, opt-env-conf
, path, path-io, pretty-show, process, template-haskell, text
, unliftio, xml-conduit
}:
mkDerivation {
  pname = "fix-codegen";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring containers opt-env-conf path path-io pretty-show
    process template-haskell text unliftio xml-conduit
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "fix-codegen";
}
