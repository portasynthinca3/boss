{ erlang_27, stdenvNoCC }:
stdenvNoCC.mkDerivation {
  pname = "etfify";
  version = "0.1.0";
  src = ../../apps/etfify;
  dontUnpack = true;
  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;

  buildInputs = [ erlang_27 ];
  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    install -m 0555 "$src" "$out/bin/etfify"

    runHook postInstall
  '';

  meta.mainProgram = "etfify";
}
