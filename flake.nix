{
  description = "angelagesteira.com website";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs"; };
    nixpkgs-1803 = { url = "github:NixOS/nixpkgs/release-18.03"; flake = false; };
    nixpkgs-2003 = { url = "github:NixOS/nixpkgs/release-20.03"; };
    nixpkgs-2211 = { url = "github:NixOS/nixpkgs/release-22.11"; };
    nixpkgs-2305 = { url = "github:NixOS/nixpkgs/release-23.05"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    nix-filter.url = "github:numtide/nix-filter";
    # needed for systest
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, flake-utils, ... }@inputs:

    let
      pkgsFor = nixpkgs: system:
        import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
            inputs.nix-filter.overlays.default
            inputs.safe-coloured-text.overlays.${system}
          ];
        };
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "assets"
          "package.yaml"
        ];
      };
    in
    {
      overlays.default = final: prev: with final.haskell.lib; {
        haskellPackages = prev.haskellPackages.override
          (old: {
            overrides = final.lib.composeExtensions
              (old.overrides or (_: _: { }))
              (self: super: {
                autodocodec-yaml = unmarkBroken (doCheck super.autodocodec-yaml);
                sydtest = unmarkBroken (dontCheck super.sydtest);
                ghcaniuse = dontCheck (self.callCabal2nix "ghcaniuse" src { });
              });
          });
      };
    }
    //
    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = pkgsFor inputs.nixpkgs system;

          pkgs1803 = pkgsFor inputs.nixpkgs-1803 system;
          pkgs2003 = pkgsFor inputs.nixpkgs-2003 system;
          pkgs2211 = pkgsFor inputs.nixpkgs-2211 system;
          pkgs2305 = pkgsFor inputs.nixpkgs-2305 system;
          mkDerivationLanguageExtensions = name: ghc:
            pkgs.stdenv.mkDerivation {
              name = "language-extensions-for-${name}";
              phases = [ "buildPhase" "installPhase" ];
              buildInputs = [ ghc ];
              buildPhase = "ghc --supported-languages > ${name}-languages.txt";
              installPhase = "mkdir -p $out; cp ${name}-languages.txt $out";
            };
          ghcs = {
            "ghc-7.0.4" = pkgs1803.haskell.compiler.ghc704Binary;
            "ghc-7.4.2" = pkgs1803.haskell.compiler.ghc742Binary;
            "ghc-7.6.3" = pkgs1803.haskell.compiler.ghc763;
            "ghc-7.8.4" = pkgs1803.haskell.compiler.ghc784Binary;
            "ghc-7.10.3" = pkgs1803.haskell.compiler.ghc7103Binary;
            "ghc-8.0.2" = pkgs1803.haskell.compiler.ghc802;
            "ghc-8.2.2" = pkgs2003.haskell.compiler.ghc822Binary;
            "ghc-8.4.4" = pkgs2003.haskell.compiler.ghc844;
            "ghc-8.6.5" = pkgs2211.haskell.compiler.ghc865Binary;
            "ghc-8.8.4" = pkgs2305.haskell.compiler.ghc884;
            "ghc-8.10.7" = pkgs2305.haskell.compiler.ghc8107;
            "ghc-9.0.2" = pkgs2305.haskell.compiler.ghc902;
            "ghc-9.2.6" = pkgs2305.haskell.compiler.ghc926;
            "ghc-9.4.5" = pkgs2305.haskell.compiler.ghc945;
            "ghc-9.6.2" = pkgs2305.haskell.compiler.ghc962;
          };
          precommitCheck = inputs.pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              actionlint.enable = true;
              hlint.enable = true;
              hpack.enable = true;
              markdownlint.enable = true;
              nil.enable = true;
              nixpkgs-fmt.enable = true;
              ormolu.enable = true;
              statix.enable = true;
            };
          };
        in
        rec {
          packages = {
            inherit (pkgs.haskellPackages) ghcaniuse;
            ghc-language-extensions = pkgs.symlinkJoin {
              name = "ghc-language-extensions";
              paths = builtins.attrValues (
                (builtins.mapAttrs mkDerivationLanguageExtensions)
                  ghcs
              );
            };
            ghcaniuse-web = pkgs.stdenv.mkDerivation {
              name = "ghcaniuse-web";
              phases = [ "buildPhase" "installPhase" ];
              buildPhase = apps.ghcaniuse-gen.program;
              installPhase = ''
                mkdir -p $out
                cp index.html $out
              '';
            };
            default = packages.ghcaniuse-web;
          };
          apps = {
            ghcaniuse = flake-utils.lib.mkApp {
              drv = pkgs.haskell.lib.justStaticExecutables (
                packages.ghcaniuse.overrideAttrs (oldAttrs: {
                  configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
                })
              );
            };
            ghcaniuse-gen = flake-utils.lib.mkApp {
              drv = pkgs.writeShellScriptBin "ghcaniuse-gen" ''
                ${apps.ghcaniuse.program} \
                  --directory ${packages.ghc-language-extensions} $@
              '';
            };
            default = apps.ghcaniuse-gen;
          };

          devShells = {
            default = pkgs.haskellPackages.shellFor {
              packages = p: [ (pkgs.haskell.lib.doCheck packages.ghcaniuse) ];
              buildInputs = with pkgs; [
                nixpkgs-fmt
                cabal-install
                hpack
                ghcid
                haskellPackages.implicit-hie
                haskell-language-server
              ];
              inherit (precommitCheck) shellHook;
            };
          };
        });
}
