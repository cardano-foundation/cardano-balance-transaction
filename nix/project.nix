{ CHaP, indexState, pkgs, mkdocs, ... }:

let
  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
      hlint = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.shellcheck
      pkgs.mkdocs
      mkdocs.from-nixpkgs
      pkgs.haskellPackages.cabal-fmt
    ];
  };

  mkProject = { lib, pkgs, ... }: {
    name = "cardano-balance-tx";
    src = ./..;
    compiler-nix-name = "ghc9122";
    shell = shell { inherit pkgs; };
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
    modules = [
      ({ lib, pkgs, ... }: {
        packages.cardano-crypto-praos.components.library.pkgconfig =
          lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
        packages.cardano-crypto-class.components.library.pkgconfig =
          lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
      })
    ];
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.lib = project.hsPkgs.cardano-balance-tx.components.library;
  # TODO: Re-enable after test suite is restored
  # packages.unit-tests =
  #   project.hsPkgs.cardano-balance-tx.components.tests.unit;
}
