{
  description = "slides for an overview of the CS backend";

  inputs =
    {
      nixpkgs = { url = "github:NixOS/nixpkgs"; };
      flake-utils = { url = "github:numtide/flake-utils"; };
    };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          mk-ghcaniuse = returnShellEnv:
            pkgs.haskellPackages.developPackage {
              root = ./.;
              source-overrides = { };
              overrides = new: old: { };
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (
                    with pkgs; with haskellPackages; [
                      cabal-install
                      ghcid
                      haskell-language-server
                      hlint
                      hpack
                      ormolu
                    ]
                  );
              inherit returnShellEnv;
            };

        in
        {
          packages = rec {
            devShell = mk-ghcaniuse true;
            ghcaniuse = mk-ghcaniuse false;
            default = devShell;
          };
        }
      );
}
