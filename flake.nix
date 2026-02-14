{
  description = "Balancing transactions for the Cardano blockchain";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs =
    inputs@{ self, nixpkgs, flake-utils, haskellNix, iohkNix, mkdocs, CHaP, ... }:
    let version = self.dirtyShortRev or self.shortRev;
    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [
            iohkNix.overlays.crypto
            haskellNix.overlay
            iohkNix.overlays.haskell-nix-extra
            iohkNix.overlays.haskell-nix-crypto
          ];
          inherit system;
        };
        project = import ./nix/project.nix {
          indexState = "2025-10-01T00:00:00Z";
          inherit pkgs CHaP;
          mkdocs = mkdocs.packages.${system};
        };
      in {
        packages = project.packages // { default = project.packages.lib; };
        inherit (project) devShells;
      });
}
