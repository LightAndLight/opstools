{ mkDerivation, base, deepseq, filepath, lib, optparse-applicative
, process, temporary, unix
}:
mkDerivation {
  pname = "sshgen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base deepseq filepath optparse-applicative process temporary unix
  ];
  license = lib.licenses.mit;
  mainProgram = "sshgen";
}
