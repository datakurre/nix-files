{
  lib,
  stdenv,
}:

stdenv.mkDerivation {
  pname = "evdev-debounce";
  version = "1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = ./debounce.c;
  };

  buildPhase = ''
    runHook preBuild
    $CC -O2 -Wall -Wextra -Werror -o evdev-debounce debounce.c
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 evdev-debounce $out/bin/evdev-debounce
    runHook postInstall
  '';

  meta = {
    description = "interception-tools filter suppressing phantom button events from a worn microswitch";
    mainProgram = "evdev-debounce";
    platforms = lib.platforms.linux;
  };
}
