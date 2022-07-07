{
  description = "bi-directional tangle daemon for literate programming";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "entangled";
      in with pkgs; {
        packages.${packageName} =
          haskell.lib.doJailbreak (haskellPackages.callCabal2nix packageName self { });

        defaultPackage = self.packages.${system}.${packageName};

        devShell = mkShell {
	  packages = [
	    zlib
	  ];
          buildInputs = [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
