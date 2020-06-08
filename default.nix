{ sources ? import ./nix/sources.nix, nixpkgs ? "nixpkgs"
, pkgs ? import sources.${nixpkgs} { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, ghc ? "default" }:

nix-hs {
  cabal = ./themoviedb.cabal;
  compiler = ghc;
}
