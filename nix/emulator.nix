{
  runCommand,
  lib,
  writeText,
  openssl,
  binutils-unwrapped-all-targets,
}:
{
  src,
  magic-offset,
  reloc-offset,
}:
let
  relocMagic = runCommand "boss-reloc-magic.bin" { } ''
    ${lib.getExe openssl} shake256 -xoflen 1024 -binary <${lib.escapeShellArg src} > rand
    {
      cat rand rand
      ${lib.getExe' binutils-unwrapped-all-targets "objdump"} -hj.data ${lib.escapeShellArg src} \
        | tail -n+6 | head -n1
      printf '\0'
    } > "$out"
  '';
in
runCommand "boss-emulator"
  {
    passthru = {
      inherit
        src
        magic-offset
        reloc-offset
        relocMagic
        ;
    };
  }
  ''
    ${lib.getExe' binutils-unwrapped-all-targets "objcopy"} ${lib.escapeShellArg src} \
      --add-section .reloc-magic=${lib.escapeShellArg relocMagic} \
      --change-section-address .reloc-magic=${toString magic-offset} \
      --change-section-address .reloc=${toString reloc-offset} \
      "$out"
  ''
