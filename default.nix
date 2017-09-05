{ mkDerivation, base, clay, filepath, hakyll, lucid, stdenv, text
}:
mkDerivation {
  pname = "imagined-saviors";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base clay filepath hakyll lucid text
  ];
  license = stdenv.lib.licenses.mit;
}
