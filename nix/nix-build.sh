#!/usr/bin/env bash

set -eu
set -o pipefail

nixpkgs=(
  "nixpkgs/nixos-20.03"
  "nixpkgs" # NixOS 20.09
  "nixpkgs-unstable"
)

for pkgs in "${nixpkgs[@]}"; do
  nix-build --no-out-link --argstr nixpkgs "$pkgs"
done
