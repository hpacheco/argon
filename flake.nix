{
  description = "argon";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem = {
        pkgs,
        system,
        ...
      }: let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        pkgsStatic = inputs.nixpkgs.legacyPackages.${system}.pkgsMusl;
        myOverlay = pkgs.haskell.lib.compose.packageSourceOverrides {
          argon = ./.;
        };
        hsPkgs = pkgs.haskell.packages.ghc910.extend myOverlay;

        fixGhc = pkg:
          pkg.override {
            enableRelocatedStaticLibs = true;
            enableShared = false;
            enableDwarf = false;
          };

        hsPkgsStatic =
          (pkgsStatic.haskell.packages.ghc910.override (old: {
            ghc = fixGhc old.ghc;
            buildHaskellPackages = old.buildHaskellPackages.override (oldBHP: {
              ghc = fixGhc oldBHP.ghc;
            });
          }))
          .extend
          myOverlay;
      in {
        devShells.default = hsPkgs.shellFor {
          packages = p: [p.argon];
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.fourmolu
            hsPkgs.cabal-fmt
          ];
        };

        packages.default = pkgs.haskell.lib.dontCheck hsPkgs.argon;

        packages.static = pkgs.haskell.lib.overrideCabal hsPkgsStatic.argon (old: {
          configureFlags =
            (old.configureFlags or [])
            ++ [
              "--ghc-option=-optl=-static"
              "--extra-lib-dirs=${pkgsStatic.gmp6.override {withStatic = true;}}/lib"
              "--extra-lib-dirs=${pkgsStatic.libffi.overrideAttrs (old: {dontDisableStatic = true;})}/lib"
              "--extra-lib-dirs=${pkgsStatic.zlib.static}/lib"
            ];
          enableSharedExecutables = false;
          enableSharedLibraries = false;
          doCheck = false;
        });

        formatter = pkgs.alejandra;
      };
    };
}
