{ stdenv, cmake }:

stdenv.mkDerivation {
  name = "jfrog-cli-1.15.1";
  src = builtins.fetchurl {
    url = "https://dl.bintray.com/jfrog/jfrog-cli-go/1.15.1/jfrog-cli-linux-386/jfrog";
    sha256 = "9a83d6441a7db255e864d0496d2746d657fa9be100fec83c6e554c7d0f8ba694";
  };
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/jfrog-cli
    chmod u+x $out/bin/jfrog-cli
  '';
}
