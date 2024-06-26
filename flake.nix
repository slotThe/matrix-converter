{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      package = "matrix";
      system  = "x86_64-linux";
      pkgs    = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        main = prev.callCabal2nix package ./. { };
      };
      haskellPackages = pkgs.haskellPackages.extend overlay;
    in {
      # nix build
      packages.${system}.default = haskellPackages.main;

      # nix develop
      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ p.main ];
        nativeBuildInputs = with pkgs.xorg; [
          libX11
          libXScrnSaver
          libXext
          libXrandr
          libXinerama
        ];
        buildInputs = with haskellPackages; [
          cabal-install
          haskell-language-server
          hpack
        ];
      };
    };
}
