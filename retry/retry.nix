{ mkDerivation, base, lib, optparse-applicative, process }:
mkDerivation {
  pname = "retry";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base optparse-applicative process ];
  license = lib.licenses.mit;
  mainProgram = "retry";
}
