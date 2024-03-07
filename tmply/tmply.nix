{ mkDerivation, attoparsec, base, bytestring, directory, lib
, optparse-applicative, process, temporary, vector
}:
mkDerivation {
  pname = "tmply";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring directory optparse-applicative process
    temporary vector
  ];
  license = lib.licenses.mit;
  mainProgram = "tmply";
}
