{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, basic-prelude, bytestring, lens
      , lucid, parsec, scalpel, stdenv, text, time, transformers, turtle
      , unordered-containers
      }:
      mkDerivation {
        pname = "ghcaniuse";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base basic-prelude bytestring lens lucid parsec scalpel text
          time transformers turtle unordered-containers
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
