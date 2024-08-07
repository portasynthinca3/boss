{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs = {
      nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      nixpkgs,
      flake-parts,
      crane,
      rust-overlay,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = nixpkgs.lib.platforms.all;
      perSystem =
        {
          pkgs,
          system,
          lib,
          self',
          ...
        }:
        let
          platform = lib.systems.elaborate system;
          inherit (if platform.isx86_64 -> platform.isDarwin then pkgs.pkgsCross.x86_64-embedded else pkgs)
            OVMF
            ;
          toolchain = p: p.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
          craneLib = (crane.mkLib pkgs).overrideToolchain toolchain;
          buildCargoPackage = import ./nix/build-rust-app.nix { inherit craneLib lib; };
        in
        {
          packages =
            let
              extraExclude = [
                ./rust-toolchain.toml
                ./nix
                ./apps
                ./demo
                ./.github
              ];
              extraInclude = [ ./src/vm/genop.tab ];
              src = lib.cleanSourceWith {
                filter = (
                  path: type:
                  (!builtins.elem path (map toString extraExclude))
                  && (craneLib.filterCargoSources path type || builtins.elem path (map toString extraInclude))
                );
                src = ./.;
                name = "source";
              };
              boss = buildCargoPackage {
                doCheck = false;
                fixBuildStd = true;
                inherit src;
                target = ./x86_64-boss-uefi.json;
                extraArgs = {
                  # https://github.com/ipetkov/crane/issues/262
                  dummyrs = ./nix/dummy.rs;
                };
              };
              inherit (pkgs.callPackage ./nix { }) buildEmulator bosbaima buildIso;
            in
            {
              kernel = boss.package;
              inherit (boss)
                audit
                clippy
                rustdoc
                rustfmt
                deps
                ;
              emulator = buildEmulator {
                src = lib.getExe' self'.packages.kernel "boss.efi";
                magic-offset = "0x141000000";
                reloc-offset = "0x141001000";
              };
              bosbaima = bosbaima.override {
                bossApps =
                  let
                    apps = self'.legacyPackages.bossApps;
                  in
                  [ apps.base ];
              };
              iso = buildIso {
                inherit (self'.packages) bosbaima;
                boss-emulator = self'.packages.emulator;
              };
              qemu = pkgs.writeShellApplication {
                name = "qemu-boss";
                runtimeInputs = [ pkgs.qemu ];
                text = ''
                  qemu-system-x86_64 \
                    -drive if=pflash,format=raw,readonly=on,file=${lib.escapeShellArg OVMF.fd}/FV/OVMF.fd \
                    -device ahci,id=ahci \
                    -device ide-hd,drive=disk,bus=ahci.0 \
                    -drive if=none,id=disk,format=raw,snapshot=on,file=${lib.escapeShellArg self'.packages.iso} \
                    -m 128 \
                    -smp 1,sockets=1,cores=1,threads=1 \
                    -boot menu=off,splash-time=0 \
                    -serial stdio
                '';
              };
            };
          legacyPackages = {
            inherit (pkgs.callPackage ./nix/boss-lib { }) buildBossApp;
            bossApps = {
              base = self'.legacyPackages.buildBossApp {
                src = ./apps/base;
                pname = "base";
                version = "0.1.0";
              };
            };
          };

          apps.default.program = self'.packages.qemu;

          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import rust-overlay) ];
          };
        };
    };
}
