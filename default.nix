{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./themoviedb.cabal;
  compiler = ghc;

  overrides = lib: self: super: {
    aeson =
      if super ? aeson_1_5_2_0
      then super.aeson_1_5_2_0
      else super.aeson;

    http-client =
      if super ? http-client_0_7_1
      then super.http-client_0_7_1
      else super.http-client;

    # ERROR: infinite recursion encountered, at undefined position
    # time =
    #   if super ? time_1_10
    #   then super.time_1_10
    #   else super.time;
  };
}
