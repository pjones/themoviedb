# Load an interactive environment:
{ ghc ? "default"
, nixpkgs ? "nixpkgs"
}:

(import ./. {
  inherit ghc nixpkgs;
}).interactive
