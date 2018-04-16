{ stdenv, fetchurl, makeWrapper, electron }:

stdenv.mkDerivation rec {
  name = "camunda-modeler-${version}";
  version = "1.13.0";

  src = fetchurl {
    url = "https://camunda.org/release/camunda-modeler/${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "0hrcraql443s2jbmrhm0ra3qiy2j5fqizjm2bx74df46mkmpfqbh";
  };

  nativeBuildInputs = [ electron makeWrapper ];

  installPhase = ''
    mkdir -p $out/var/lib/camunda $out/bin
    cp resources/app.asar $out/var/lib/camunda
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar"
  '';
}
