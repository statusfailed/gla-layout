{ mkDerivation, base, miso, stdenv }:
mkDerivation {
  pname = "gla-layout";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso ];
  description = "Graphical Linear Algebra diagram layout algorithm";
  license = stdenv.lib.licenses.bsd3;
}
