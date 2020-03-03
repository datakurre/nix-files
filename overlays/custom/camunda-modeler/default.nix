{ stdenv, fetchurl, makeWrapper, electron }:

stdenv.mkDerivation rec {
  name = "camunda-modeler-${version}";
  version = "3.5.0";
  src = fetchurl {
    url = "https://github.com/camunda/camunda-modeler/releases/download/v${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "1qmgvb08b26iizizqglda0vyrxf8v3dxz2fxpbga1q7z76grmmij";
  };

  nativeBuildInputs = [ electron makeWrapper ];

  installPhase = ''
    mkdir -p $out/var/lib/camunda $out/bin
    cp resources/app.asar $out/var/lib/camunda
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar"
  '';
}
