{ stdenv, fetchurl, makeWrapper, electron }:

stdenv.mkDerivation rec {
  name = "zeebe-modeler-${version}";
  version = "0.6.2";
  src = fetchurl {
    url = "https://github.com/zeebe-io/zeebe-modeler/releases/download/v${version}/zeebe-modeler-${version}-linux-x64.tar.gz";
    sha256 = "04qvrzyfik0h59g3my9h9gpmlhqqw36wp33cwqkpn1vr6dz95mm6";
  };

  nativeBuildInputs = [ electron makeWrapper ];

  installPhase = ''
    mkdir -p $out/var/lib/zeebe $out/bin
    cp resources/app.asar $out/var/lib/zeebe
    makeWrapper ${electron}/bin/electron $out/bin/zeebe-modeler \
      --add-flags "$out/var/lib/zeebe/app.asar"
  '';
}
