let
  f =
    build-or-shell:
    { chan ? "5272327b81ed355bbed5659b8d303cf2979b6953"
    , compiler ? "ghc865"
    , withHoogle ? false
    , doHoogle ? false
    , doHaddock ? false
    , enableLibraryProfiling ? false
    , enableExecutableProfiling ? false
    , strictDeps ? false
    , isJS ? false
    , system ? builtins.currentSystem
    , optimize ? true
    , shpadoinkle-path ? null
    }:
    let


      # It's a shpadoinkle day
      shpadoinkle = if shpadoinkle-path != null then shpadoinkle-path else builtins.fetchGit {
        url    = https://gitlab.com/platonic/shpadoinkle.git;
        ref    = "master";
        rev    = "1b3b0cedd7e360243ab07ccc2d20553b0711e93c";
      };


      # Get some utilities
      inherit (import (shpadoinkle + "/nix/util.nix") { inherit compiler isJS pkgs; }) compilerjs doCannibalize;


      # Build faster by doing less
      chill = p: (pkgs.haskell.lib.overrideCabal p {
        inherit enableLibraryProfiling enableExecutableProfiling;
      }).overrideAttrs (_: {
        inherit doHoogle doHaddock strictDeps;
      });


      # Overlay containing Shpadoinkle packages, and needed alterations for those packages
      # as well as optimizations from Reflex Platform
      shpadoinkle-overlay =
        import (shpadoinkle + "/nix/overlay.nix") { inherit compiler chan isJS enableLibraryProfiling enableExecutableProfiling; };


      # Haskell specific overlay (for you to extend)
      haskell-overlay = hself: hsuper: {
        "happy" = pkgs.haskell.lib.dontCheck hsuper.happy;
      };


      # Top level overlay (for you to extend)
      budgetable-overlay = self: super: {
        haskell = super.haskell //
          { packages = super.haskell.packages //
            { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
                overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
              });
            };
          };
        };


      # Complete package set with overlays applied
      pkgs = import
        (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
        }) {
        inherit system;
        overlays = [
          shpadoinkle-overlay
          budgetable-overlay
        ];
      };


      ghcTools = with pkgs.haskell.packages.${compiler};
        [ cabal-install
          ghcid
        ] ++ (if isJS then [] else [ stylish-haskell ]);


      # We can name him George
      budgetable =
        with builtins;
        let l = pkgs.lib; in
        pkgs.haskell.packages.${compilerjs}.callCabal2nix "budgetable"
          (filterSource
             (path: _:
               let pathP = replaceStrings [((getEnv "PWD") + "/")] [""] path; in
               let first3 = substring 0 3 pathP; in
               trace first3
               ((first3 == "src" || first3 == "app") || l.hasSuffix ".cabal" path)
             #   seq pathP (trace pathP
             # (
             #    baseNameOf path == "src"
             #    # || baseNameOf path == "View"
             #    || baseNameOf path == "app"
             #    || l.hasSuffix ".hs" path
             #    || l.hasSuffix ".cabal" path
             # )
             # )
             )
             ../.
          )
          {};


    in with pkgs; with lib;

      { build =
          (if isJS && optimize then doCannibalize else x: x) (chill budgetable);

        shell =
          pkgs.haskell.packages.${compilerjs}.shellFor {
            inherit withHoogle;
            packages    = _: [ budgetable ];
            COMPILER    = compilerjs;
            buildInputs = ghcTools;
            shellHook   = ''
              ${lolcat}/bin/lolcat ${../figlet}
              cat ${../intro}
            '';
          };
      }.${build-or-shell};
in
  { build = f "build";
    shell = f "shell";
  }
