{ stdenv, fetchurl, makeWrapper, electron }:

stdenv.mkDerivation rec {
  name = "camunda-modeller-${version}";
  version = "1.11.3";

  src = fetchurl {
    url = "https://camunda.org/release/camunda-modeler/${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "fea117012fa32c77c322b31e47fe39da69e06a14b0f1f100999f70863e1e18fc";
  };

  nativeBuildInputs = [ electron makeWrapper ];

  installPhase = ''
    mkdir -p $out/var/lib/camunda $out/bin
    cp resources/app.asar $out/var/lib/camunda
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar"
  '';
}
