{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server
            cabal2nix
          ];
        };

        packages.sshgen = pkgs.haskellPackages.callPackage ./sshgen/sshgen.nix {};

        apps.sshgen = {
          type = "app";
          program = "${pkgs.haskell.lib.justStaticExecutables packages.sshgen}/bin/sshgen";
        };
      }
    );
}
