let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = 
          haskellPackagesNew: 
          haskellPackagesOld: 
          rec {
            imagined-saviors =
              haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { imagined-saviors = pkgs.haskellPackages.imagined-saviors;
  }
