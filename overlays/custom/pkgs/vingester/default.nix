{ nixpkgs, stdenv, libXScrnSaver, makeWrapper, fetchurl, wrapGAppsHook, glib, gtk3, unzip, atomEnv, libuuid, at-spi2-atk, at-spi2-core, nodePackages, autoPatchelfHook, gcc-unwrapped, libdrm, mesa, libxkbcommon, adoptopenjdk-jre-hotspot-bin-11, libxshmfence, lib, libappindicator-gtk3
, appimage-run, ndi, ffmpeg, avahi }:

let

  mkElectron = import "${nixpkgs}/pkgs/development/tools/electron/generic.nix" { inherit stdenv libXScrnSaver makeWrapper fetchurl wrapGAppsHook glib gtk3 unzip atomEnv libuuid at-spi2-atk at-spi2-core libdrm mesa libxkbcommon libxshmfence lib libappindicator-gtk3; };

  electron = mkElectron "16.0.8" {
    x86_64-linux = "0my8k3b92pq46hdnf2aj033p7c2vssf8h22hxlb7d4mhsja7lxi5";
  };

  vingester_asar = stdenv.mkDerivation rec {
    name = "vingester-${version}-asar";
    version = "2.7.1";
    src = fetchurl {
      url = "https://github.com/rse/vingester/releases/download/${version}/Vingester-lnx-x64.zip";
      sha256 = "01gpcp8m76a9y2xa4v833y9frdc3np7c0kk61cxn88i4dywjbgvj";
    };
    nativeBuildInputs = [ nodePackages.asar autoPatchelfHook appimage-run unzip ];
    buildInputs = [ ffmpeg ndi avahi ];
    unpackPhase = ''
      unzip $src
      appimage-run -x src Vingester
    '';
    installPhase = ''
      asar extract src/resources/app.asar $out
      substituteInPlace $out/vingester-main.js \
        --replace "const Update " \
                  "// const Update " \
        --replace "const update = " \
                  "const update = { updateable: async () => false, check: async () => null, update: async () => null, cleanup: async () => null }; //"
      mv $out/node_modules/@discordjs/opus/prebuild/electron-v16.0-napi-v3-linux-x64-glibc-2.31 \
         $out/node_modules/@discordjs/opus/prebuild/electron-v16.0-napi-v3-linux-x64-glibc-2.33
      sed -i "s/update-versions/update-versions-disabled/g" $out/*.js
    '';
  };

in

stdenv.mkDerivation rec {
   name = "vingester-${version}";
   version = "2.3.2";
    src = fetchurl {
      url = "https://github.com/rse/vingester/releases/download/${version}/Vingester-lnx-x64.zip";
      sha256 = "01gpcp8m76a9y2xa4v833y9frdc3np7c0kk61cxn88i4dywjbgvj";
    };

   nativeBuildInputs = [ makeWrapper nodePackages.asar autoPatchelfHook appimage-run unzip ];

   buildInputs = [ electron ffmpeg ndi avahi ];
   propagatedBuildInputs = [ ffmpeg ];

   unpackPhase = ''
     unzip $src
     appimage-run -x src Vingester
   '';

   installPhase = ''
     mkdir -p $out/var/lib/vingester $out/bin
     asar pack ${vingester_asar} $out/var/lib/vingester/app.asar
     ls -l src/resources
     cp -a src/resources/app.asar.unpacked $out/var/lib/vingester
     makeWrapper ${electron}/bin/electron $out/bin/vingester \
       --add-flags "$out/var/lib/vingester/app.asar" \
       --prefix LD_LIBRARY_PATH : "$out/var/lib/vingester/app.asar.unpacked/node_modules/grandiose/ndi/lib/lnx-x86/"
   '';
 }
