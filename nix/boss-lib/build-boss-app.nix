{
  erlang_27,
  stdenv,
  etfify,
  lib,
}:
{
  pname,
  version,
  src,
  app-name ? pname,
  ...
}@args:
stdenv.mkDerivation (
  {
    inherit pname version src;
    outputs = [
      "out"
      "bop"
    ];
    nativeBuildInputs = [
      erlang_27
      etfify
    ];
    buildPhase = ''
      runHook preBuild

      mkdir build
      find src/ -name '*.erl' -exec erlc -b beam -o build/ '{}' +
      etfify src/${lib.escapeShellArg app-name}.app.src app

      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall

      mv build "$out"
      mv app "$out"/

      tar --create --file="$bop" --directory="$out" --transform='s%\.%ebin%' .

      runHook postInstall
    '';
    passthru = {
      inherit app-name;
    };
  }
  // builtins.removeAttrs args [
    "pname"
    "version"
    "src"
    "app-name"
  ]
)
