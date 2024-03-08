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
        packages.tmply = pkgs.haskellPackages.callPackage ./tmply/tmply.nix {};
        packages.retry= pkgs.haskellPackages.callPackage ./retry/retry.nix {};

        apps.sshgen = {
          type = "app";
          program = "${pkgs.haskell.lib.justStaticExecutables packages.sshgen}/bin/sshgen";
        };
        apps.tmply = {
          type = "app";
          program = "${pkgs.haskell.lib.justStaticExecutables packages.tmply}/bin/tmply";
        };
        apps.retry = {
          type = "app";
          program = "${pkgs.haskell.lib.justStaticExecutables packages.retry}/bin/retry";
        };
      }
    );
}
