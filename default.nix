{ mkDerivation, base, clay, hakyll, stdenv }:
mkDerivation {
  pname = "imagined-saviors";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base clay hakyll ];
  license = stdenv.lib.licenses.mit;
}
