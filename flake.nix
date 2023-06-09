{
  description = "angelagesteira.com website";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/release-21.11"; };
    nixpkgs-2003 = { url = "github:NixOS/nixpkgs/release-20.03"; };
    nixops-plugged = { url = "github:lukebfox/nixops-plugged"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }@inputs:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ ];
      };

    in
    flake-utils.lib.eachDefaultSystem
      (system:

      let
        pkgs = pkgsFor system;
        haskellPackages = pkgs.haskellPackages.extend
          (pkgs.haskell.lib.compose.packageSourceOverrides {
            ghcaniuse = ./.;
          });
      in
      rec {
        packages = {
          ghcaniuse = haskellPackages.ghcaniuse;
        };

        defaultPackage = packages.ghcaniuse;
        devShells.default =
          haskellPackages.shellFor
            {
              packages = p: [ packages.ghcaniuse ];
              buildInputs = with pkgs; [ haskell-language-server cabal-install ghcid haskellPackages.implicit-hie ];
            };
        deploy-shell = pkgs.mkShell {
          buildInputs = [ inputs.nixops-plugged.defaultPackage.${system} ];
          shellHook = ''
            export NIX_PATH="nixpkgs=${nixpkgs}"
            export NIXOPS_STATE="./state.nixops"
          '';
        };
      }
      );
}
