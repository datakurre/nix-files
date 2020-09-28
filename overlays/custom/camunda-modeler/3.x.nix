{ stdenv, fetchurl, makeWrapper, electron }:

stdenv.mkDerivation rec {
  name = "camunda-modeler-${version}";
  version = "3.7.3";
  src = fetchurl {
    url = "https://github.com/camunda/camunda-modeler/releases/download/v${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "10b2khcxjyaa5syh4slianq4vyxglzqlvjl3hicgs8rwhxjvms21";
  };

  nativeBuildInputs = [ electron makeWrapper ];

  installPhase = ''
    mkdir -p $out/var/lib/camunda $out/bin
    cp resources/app.asar $out/var/lib/camunda
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar"
  '';
}
