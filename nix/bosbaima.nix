{
  stdenvNoCC,
  lib,
  bossApps ? [ ],
}:
stdenvNoCC.mkDerivation {
  pname = "bosbaima";
  version = "0.1.0";

  dontUnpack = true;
  dontBuild = true;

  installPhase =
    ''
      mkdir -p "$out"
    ''
    + lib.concatLines (
      map (
        app:
        # sh
        ''
          cp ${lib.escapeShellArg app.bop} "$out"/${lib.escapeShellArg app.app-name}.bop
        '') bossApps
    );
  passthru = {
    apps = bossApps;
  };
}
