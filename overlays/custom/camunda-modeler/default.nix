{ stdenv, libXScrnSaver, makeWrapper, fetchurl, wrapGAppsHook, glib, gtk3, unzip, atomEnv, libuuid, at-spi2-atk, at-spi2-core, nodePackages, autoPatchelfHook, gcc-unwrapped }:

let

  mkElectron = import (<nixpkgs> + "/pkgs/development/tools/electron/generic.nix") { inherit stdenv libXScrnSaver makeWrapper fetchurl wrapGAppsHook glib gtk3 unzip atomEnv libuuid at-spi2-atk at-spi2-core; };
  electron = mkElectron "7.2.4" {
    x86_64-linux = "aa809819aa353f0dabd40a80124f5e433ccba445ec4dfa9668e04ae47fcb6057";
    x86_64-darwin = "11d05813c2b7c923a2b58f4ca4460869619350419a5cd962d0ce3e4639146f45";
  };

in

stdenv.mkDerivation rec {
  name = "camunda-modeler-${version}";
  version = "4.3.0";
  src = fetchurl {
    url = "https://github.com/camunda/camunda-modeler/releases/download/v${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "03qgby5szqxy2zvq39cpv0jrqb47gzn3ghr2zqw7hlak9g8s38kh";
  };

  nativeBuildInputs = [ electron makeWrapper nodePackages.asar autoPatchelfHook gcc-unwrapped ];

  installPhase = ''
    mkdir build
    cd build
    asar extract ../resources/app.asar .
    if [ -f node_modules/grpc/src/node/extension_binary/node-v72-linux-x64-glibc/grpc_node.node ]; then
      autoPatchelf node_modules/grpc/src/node/extension_binary/node-v72-linux-x64-glibc/grpc_node.node;
    fi
    if [ -f node_modules/grpc/src/node/extension_binary/electron-v7.2-linux-x64-glibc/grpc_node.node ]; then
      autoPatchelf node_modules/grpc/src/node/extension_binary/electron-v7.2-linux-x64-glibc/grpc_node.node;
    fi
    asar pack . ../app.asar
    cd ..
    mkdir -p $out/var/lib/camunda $out/bin
    cp app.asar $out/var/lib/camunda
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar"
  '';

# installPhase = ''
#   mkdir -p $out/var/lib/camunda $out/bin
#   cp resources/app.asar $out/var/lib/camunda
#   makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
#     --add-flags "$out/var/lib/camunda/app.asar"
# '';
}
