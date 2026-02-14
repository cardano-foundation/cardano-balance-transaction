{ indexState, pkgs, mkdocs, ... }:

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
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.lib = project.hsPkgs.cardano-balance-tx.components.library;
  packages.unit-tests =
    project.hsPkgs.cardano-balance-tx.components.tests.unit;
}
