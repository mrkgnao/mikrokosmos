let 
  rien = import /home/frob/code/rien/rien.nix {
    packageName = "mikrokosmos";
    packagePath = ./.;

    # Instead of using <nixpkgs>, use a lock-file to stick to
    # a particular `nixpkgs` commit.
    nixpkgsLock = ./nixpkgs.json;

    ghcVersion = "ghc822";
  };

in
  (rien.shell {
    # Generate Hoogle documentation?
    wantHoogle = false;

    # Haskell dependencies
    deps = hsPkgs: with hsPkgs; [
      brittany
      hakyll
      clay
      filepath
      lucid
      type-of-html
      text
      binary
      latex-formulae-hakyll
      latex-formulae-image
      latex-formulae-pandoc
      pandoc-types
      unordered-containers

      bound
      deriving-compat
    ];

    # Optionally, also add sets of related packages that are
    # commonly used together.
    depSets = hsPkgs: with (rien.package-sets hsPkgs); [
      development-servers
    ];

    # Native dependencies
    nativeDeps = pkgs: with pkgs; [ 
      z3 # for liquidhaskell
    ];
  }) // { shellHook = "source setup-ghcmod.sh"; } 
