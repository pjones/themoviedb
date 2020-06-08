{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, compiler ? "default" }:

nix-hs {
  inherit compiler;
  cabal = ./themoviedb.cabal;
}
