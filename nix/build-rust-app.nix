{ craneLib, lib }:
{
  src,
  doCheck ? true,
  target ? null,
  extraArgs ? { },
  fixBuildStd ? false,
}:
let
  inherit (builtins) mapAttrs;
  commonArgs =
    {
      inherit src;
      inherit doCheck;
      env.CARGO_BUILD_TARGET = lib.mapNullable toString target;
      passthru = {
        inherit craneLib;
      };
    }
    // lib.optionalAttrs fixBuildStd {
      # https://github.com/ipetkov/crane/issues/285
      cargoVendorDir = craneLib.vendorMultipleCargoDeps {
        inherit (craneLib.findCargoFiles src) cargoConfigs;
        cargoLockList = [
          "${src}/Cargo.lock"
          "${craneLib.rustc.passthru.availableComponents.rust-src}/lib/rustlib/src/rust/Cargo.lock"
        ];
      };
    }
    // extraArgs;
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
  totalArgs = commonArgs // {
    inherit cargoArtifacts;
  };
in
{
  deps = cargoArtifacts;
}
// mapAttrs (_: f: f totalArgs) {
  package = craneLib.buildPackage;
  clippy = craneLib.cargoClippy;
  rustfmt = craneLib.cargoFmt;
  rustdoc = craneLib.cargoDoc;
  audit = craneLib.cargoAudit;
}
