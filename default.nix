{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "first-haskell-nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base ];
  description = "This is the simplest haskell & nix project to help people understand how it works";
  license = stdenv.lib.licenses.publicDomain;
}
