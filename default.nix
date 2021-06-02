{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./themoviedb.cabal;
  compiler = ghc;

  overrides = lib: self: super: {
    cryptonite = super.cryptonite_0_29;
    memory = super.memory_0_16_0;
    relude = super.relude_1_0_0_1;
  };
}
