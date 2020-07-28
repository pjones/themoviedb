# Build vimeta with nixpkgs-unstable.
{ sources ? import ./sources.nix
, ghc ? "default"
}:
let
  pkgs = import sources.nixpkgs-unstable { };
  drv = import ../. { inherit sources ghc pkgs; };
in
if pkgs.lib.inNixShell
then drv.interactive
else drv
