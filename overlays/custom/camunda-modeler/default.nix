{ stdenv, fetchurl, makeWrapper, electron }:

stdenv.mkDerivation rec {
  name = "camunda-modeler-${version}";
  version = "1.12.0";

  src = fetchurl {
    url = "https://camunda.org/release/camunda-modeler/${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "0k7jcmf3132d5rk3jikqcnqzv89sjb2a3h9k4hwlapq6w6vyydqq";
  };

  nativeBuildInputs = [ electron makeWrapper ];

  installPhase = ''
    mkdir -p $out/var/lib/camunda $out/bin
    cp resources/app.asar $out/var/lib/camunda
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar"
  '';
}
