{ callPackage }:
{
  boss-lib = callPackage ./boss-lib { };
  bosbaima = callPackage ./bosbaima.nix { };
  buildEmulator = callPackage ./emulator.nix { };
  buildIso = callPackage ./iso.nix;
}
