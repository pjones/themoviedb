{ pkgs ? import <nixpkgs> { }
}:

let
  # Helpful if you want to override any Haskell packages:
  overrides = self: super: {
    http-client = if super ? http-client_0_6_2
      then super.http-client_0_6_2
      else super.http-client;
  };

  # Apply the overrides from above:
  haskell = pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: {})) overrides; });
in

# Load the local nix file and use the overrides from above:
haskell.callPackage ./themoviedb.nix { }
