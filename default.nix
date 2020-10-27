{ sources ? import ./nix/sources.nix
, nixpkgs ? "nixpkgs"
, pkgs ? import sources.${nixpkgs} { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./themoviedb.cabal;
  compiler = ghc;

  overrides = lib: self: super: {
    relude =
      if super ? relude_0_6_0_0
      then super.relude_0_6_0_0 # NixOS 20.03
      else super.relude;
  };
}
