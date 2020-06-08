# Load an interactive environment:
{ ghc ? "default"
}:

(import ./. {
  inherit ghc;
}).interactive
