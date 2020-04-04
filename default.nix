let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          hasql-notifications = haskellPackagesNew.callPackage ./hasql-notifications.nix { };
          schloss = haskellPackagesNew.callPackage ./schloss.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie = (all-hies.selection { selector = p: { inherit (p) ghc882; }; });
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
          hlint
          hie
        ]);
  }
