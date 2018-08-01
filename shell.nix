{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "Battleship";
  buildInputs = [ zlib ];
  inherit ghc;
}
