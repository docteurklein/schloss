{ compiler ? "ghc882" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              schloss = haskell.packages."${compiler}".callCabal2nix "schloss" ./schloss.cabal {
                zlib = pkgs.zlib;
                openssl = pkgs.openssl;
              };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie = (all-hies.selection { selector = p: { inherit (p) ghc882; }; });
in
  pkgs.haskell.packages."${compiler}".developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskell.packages."${compiler}";
        [ cabal-install
          ghcid
          hlint
          hie
        ]);
  }
