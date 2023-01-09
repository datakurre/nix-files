{ stdenv, lib, fetchurl, dpkg, glibc, gcc-unwrapped, autoPatchelfHook, pcsclite, qtbase, wrapQtAppsHook
}:
let

  # Please keep the version x.y.0.z and do not update to x.y.76.z because the
  # source of the latter disappears much faster.
  version = "4.2.2b-8136";

  src = fetchurl {
    url = "https://dvv.fi/documents/16079645/118383788/mpollux-digisign-client-for-dvv_${version}_amd64.deb/49f1d5d3-3d23-6e9d-866b-71cd25e61177";
    sha256 = "2157ec95827a0d0170c53bfeddeb2c0c90265aa034c4c1cecbf936df4e895b91";
  };

in stdenv.mkDerivation {
  name = "mpollux-digisign-client${version}";

  system = "x86_64-linux";

  inherit src;

  # Required for compilation
  nativeBuildInputs = [
    wrapQtAppsHook
    autoPatchelfHook # Automatically setup the loader, and do the magic
    dpkg
  ];

  # Required at running time
  buildInputs = [
    glibc
    gcc-unwrapped
    qtbase
    pcsclite
  ];

  unpackPhase = "true";

  # Extract and copy executable in $out/bin
  installPhase = ''
    mkdir -p $out
    dpkg -x $src $out
    mv $out/usr/* $out
    rmdir $out/usr
  '';
}
