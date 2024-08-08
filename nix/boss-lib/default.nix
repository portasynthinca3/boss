{ callPackage }:
let
  etfify = callPackage ./etfify.nix { };
in
{
  buildBossApp = callPackage ./build-boss-app.nix { inherit etfify; };
}
