{
  description = "angelagesteira.com website";

  inputs = {
    nixpkgs-1803 = { url = "github:NixOS/nixpkgs/release-18.03"; flake = false; };
    nixpkgs-2003 = { url = "github:NixOS/nixpkgs/release-20.03"; };
    nixpkgs-2305 = { url = "github:NixOS/nixpkgs/release-23.05"; };
    nixpkgs-2405 = { url = "github:NixOS/nixpkgs/release-24.05"; };
    nixpkgs = { url = "github:NixOS/nixpkgs"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs-2405";
  };

  outputs = { self, flake-utils, ... }@inputs:

    let
      pkgsFor = nixpkgs: system:
        import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
            inputs.nix-filter.overlays.default
          ];
        };
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "test"
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
          pkgs1803 = pkgsFor inputs.nixpkgs-1803 system;
          pkgs2003 = pkgsFor inputs.nixpkgs-2003 system;
          pkgs2305 = pkgsFor inputs.nixpkgs-2305 system;
          pkgs2405 = pkgsFor inputs.nixpkgs-2405 system;
          pkgs = pkgsFor inputs.nixpkgs system;
          mkDerivationLanguageExtensions = name: ghc:
            pkgs.stdenv.mkDerivation {
              name = "language-extensions-for-${name}";
              phases = [ "buildPhase" "installPhase" ];
              buildInputs = [ ghc ];
              buildPhase = "ghc --supported-languages > ${name}-languages.txt";
              installPhase = "mkdir -p $out; cp ${name}-languages.txt $out";
            };
          ghcs = {
            "ghc-7.0.4" = pkgs1803.haskell.compiler.ghc704;
            "ghc-7.4.2" = pkgs1803.haskell.compiler.ghc742;
            "ghc-7.6.3" = pkgs1803.haskell.compiler.ghc763;
            "ghc-7.8.4" = pkgs1803.haskell.compiler.ghc784;
            "ghc-7.10.3" = pkgs1803.haskell.compiler.ghc7103;
            "ghc-8.0.2" = pkgs1803.haskell.compiler.ghc802;
            "ghc-8.2.2" = pkgs1803.haskell.compiler.ghc822;
            "ghc-8.4.4" = pkgs2003.haskell.compiler.ghc844;
            "ghc-8.6.5" = pkgs2003.haskell.compiler.ghc865;
            "ghc-8.8.4" = pkgs2305.haskell.compiler.ghc884;
            "ghc-8.10.7" = pkgs2305.haskell.compiler.ghc8107;
            "ghc-9.0.2" = pkgs2305.haskell.compiler.ghc902;
            "ghc-9.2.8" = pkgs2405.haskell.compiler.ghc928;
            "ghc-9.4.6" = pkgs2405.haskell.compiler.ghc946;
            "ghc-9.6.5" = pkgs2405.haskell.compiler.ghc965;
            "ghc-9.8.2" = pkgs2405.haskell.compiler.ghc982;
            "ghc-9.10.1" = pkgs2405.haskell.compiler.ghc9101;
            "ghc-9.12.1" = pkgs.haskell.compiler.ghc9121;
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
                fh
                hpack
                ghcid
                haskellPackages.implicit-hie
                haskellPackages.calligraphy
                haskell-language-server
              ];
              inherit (precommitCheck) shellHook;
            };
          };
        });

  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}
