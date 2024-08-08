{
  bosbaima,
  boss-emulator,
  mtools,
  runCommand,
  lib,
  gnutar,
}:
let
  size-mb = 32;
  size = 65536;
  bosbaima-tar = runCommand "bossbaima.tar" { } ''
    find ${lib.escapeShellArg bosbaima} -maxdepth 1 -printf '%P\0' \
      | ${lib.getExe gnutar} \
          --create \
          --file="$out" \
          --directory=${lib.escapeShellArg bosbaima} \
          --null \
          --files-from=- \
  '';
in
runCommand "boss.iso"
  {
    nativeBuildInputs = [ mtools ];
    passthru = {
      inherit bosbaima-tar;
    };
  }
  ''
    dd if=/dev/zero of="$out" bs=1M count=${toString size-mb}
    mformat -i "$out" -T ${toString size}
    mmd -i "$out" ::/EFI
    mmd -i "$out" ::/EFI/BOOT
    mmd -i "$out" ::/BOSS
    mcopy -i "$out" ${lib.escapeShellArg boss-emulator} ::/EFI/BOOT/BOOTX64.EFI
    mcopy -i "$out" ${lib.escapeShellArg bosbaima-tar} ::/BOSS/BOSBAIMA.TAR
  ''
