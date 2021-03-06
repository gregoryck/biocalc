# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "biocalc";
  version = "0.1.0.0";
  src = "./.";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    blazeHtml monadPar mtl scotty text waiExtra warp
  ];
  testDepends = [ monadPar QuickCheck scotty text waiExtra warp ];
  meta = {
    description = "A REST \"calculator\" for biological sequences. A Haskell learning exercise.";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
