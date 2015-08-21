{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lens, stdenv, wreq }:
      mkDerivation {
        pname = "first-haskell-nix";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base lens wreq ];
        description = "This is the simplest haskell & nix project to help people understand how it works";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
