{ stdenv, boost, cmake, ffmpeg, fuzzylite, innoextract, minizip, pkgconfig
, qt5, SDL2, SDL2_image, SDL2_mixer, SDL2_ttf, unshield, unzip, zlib
, makeWrapper }:

stdenv.mkDerivation {
  name = "vcmi-2018-04-03";
  src = builtins.fetchTarball {
    url = "https://github.com/vcmi/vcmi/archive/1fb25c46d38d7f687b9d3872cbc76978171b78e3.tar.gz";
    sha256 = "039qyzyl2y0zsaj0qckv8bv0yyb11akgr282720vwg2hz26721ml";
  };
  cmakeFlags = [
    "-DCMAKE_INSTALL_LIBDIR='lib'"
    "-DCMAKE_SKIP_RPATH='FALSE'"
    "-DENABLE_TEST=OFF"
    "-DFORCE_BUNDLED_FL=OFF"
    "-DCMAKE_BUILD_TYPE=OFF"
  ];
  buildInputs = [
    boost
    cmake
    ffmpeg
    fuzzylite
    minizip
    pkgconfig
    qt5.qtbase
    SDL2
    SDL2_image
    SDL2_mixer
    SDL2_ttf
    zlib
    makeWrapper
  ];
  propagatedBuildInputs = [
    innoextract
    unshield
    unzip
  ];
  postInstall = ''
    wrapProgram $out/bin/vcmilauncher\
      --set LD_LIBRARY_PATH $out/lib/vcmi:$out/lib/vcmi/AI/lib
  '';
}
