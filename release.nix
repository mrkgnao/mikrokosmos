let
  compiler = "ghc822";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              mikrokosmos = haskellPackagesNew.callPackage ./default.nix { };
            };
          };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

in
  { mikrokosmos = pkgs.haskellPackages.mikrokosmos;
  }
