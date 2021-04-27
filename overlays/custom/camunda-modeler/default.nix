{ stdenv, libXScrnSaver, makeWrapper, fetchurl, wrapGAppsHook, glib, gtk3, unzip, atomEnv, libuuid, at-spi2-atk, at-spi2-core, nodePackages, autoPatchelfHook, gcc-unwrapped, libdrm, mesa, libxkbcommon, adoptopenjdk-jre-hotspot-bin-11 }:

let

  mkElectron = import (<nixpkgs> + "/pkgs/development/tools/electron/generic.nix") { inherit stdenv libXScrnSaver makeWrapper fetchurl wrapGAppsHook glib gtk3 unzip atomEnv libuuid at-spi2-atk at-spi2-core libdrm mesa libxkbcommon; };
  electron = mkElectron "7.3.3" {
    x86_64-linux = "a947228a859149bec5bd937f9f3c03eb0aa4d78cfe4dfa9aead60d3646a357f9";
    x86_64-darwin = "e081436abef52212065f560ea6add1c0cd13d287a1b3cc76b28d2762f7651a4e";
  };

  camunda-modeler-plugins = fetchurl {
    url = "https://github.com/camunda/camunda-modeler-plugins/archive/3659fb8dac82098fd8de98d50d03f0d33eea7556.tar.gz";
    sha256 = "0l64a7zq7z1vx2bjbrff9xn1g1c798zxf41pjnjggjwm55kfqvh0";
  };

  camunda-modeler-property-info-plugin = fetchurl {
    url = "https://github.com/umb/camunda-modeler-property-info-plugin/archive/b50e34e99ef87f5cac30a074acbd2aae29fe53b2.tar.gz";
    sha256 = "0ac6klmd788fams5ikrl47dr2adhxzbmay778chnrp7zgh1h9w5d";
#   url = "https://github.com/umb/camunda-modeler-property-info-plugin/archive/f06c33c45ef57302a910c30d9497a46dc79bd93d.tar.gz";
#   sha256 = "02mccdi8bqwp92gfv1w7xch72yjx3pvrvqzyd82nxfb519sdbwk1";
  };

  camunda-modeler-linter-plugin = fetchurl {
    url = "https://github.com/camunda/camunda-modeler-linter-plugin/archive/c506ee6b250871c9b7785e9a1ff9fa9fe4334c85.tar.gz";
    sha256 = "080x1yjic2lhs5mqsbw3n2vxsgyadjgrflqjx20grq1vv3vjf5s1";
  };

  camunda-modeler-robot-plugin = fetchurl { 
    url = "https://github.com/datakurre/camunda-modeler-robot-plugin/archive/d9cf521699cfac82ad892c8c9951705438cb0fb5.tar.gz";
    sha256 = "0gin2icbr74a6ppcs7yj0dxl6na8m98b5whvgxglvm8r0qwzfbgj";
  };

  bpmn-js-token-simulation-plugin = fetchurl { 
    url = "https://github.com/bpmn-io/bpmn-js-token-simulation-plugin/archive/b856b708362a2022180316a8b4c84ae7122bcf63.tar.gz";
    sha256 = "0b005201c5ec346f542c6f556c2d7579a021707fd7f0701f3f7819b58a8aeb7b";
  };

  dmn-testing-plugin = fetchurl { 
    url = "https://github.com/bpmn-io/dmn-testing-plugin/archive/ca25586d607e3bd04357ee1546424f859591e0b8.tar.gz";
    sha256 = "4a48bfeeac46baaa5731ae2472c564d6c154bf5ca7716b059d0ce1b47ecaa26d";
  };

in

stdenv.mkDerivation rec {
  name = "camunda-modeler-${version}";
  version = "4.7.0";
  src = fetchurl {
    url = "https://github.com/camunda/camunda-modeler/releases/download/v${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "1fq4bbfzlybgab4rwsqzdrva5rf0v4wv65y5kylj5gai8j9d20in";
  };

  nativeBuildInputs = [ electron makeWrapper nodePackages.asar autoPatchelfHook gcc-unwrapped ];

  installPhase = ''
    mkdir build
    cd build
    asar extract ../resources/app.asar .
    substituteInPlace ./lib/index.js \
      --replace "let resourcesPaths = [" \
                "let resourcesPaths = [\"$out/var/lib/camunda/resources\","
    find . -name "grpc_node.node"
    if [ -f node_modules/grpc/src/node/extension_binary/electron-v7.3-linux-x64-glibc/grpc_node.node ]; then
      autoPatchelf node_modules/grpc/src/node/extension_binary/electron-v7.3-linux-x64-glibc/grpc_node.node;
    fi
    asar pack . ../app.asar
    cd ..
    mkdir -p $out/var/lib/camunda/resources/plugins $out/bin
    cp app.asar $out/var/lib/camunda
    cd $out/var/lib/camunda/resources/plugins
    tar xzvf ${dmn-testing-plugin}
    tar xzvf ${camunda-modeler-robot-plugin}
    tar xzvf ${bpmn-js-token-simulation-plugin}
    tar xzvf ${camunda-modeler-linter-plugin}
    tar xzvf ${camunda-modeler-plugins}
    tar xzvf ${camunda-modeler-property-info-plugin}
    mv camunda-modeler-plugins*/camunda-transaction-boundaries-plugin .
    rm -r camunda-modeler-plugins*
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar" \
      --prefix PATH : "${adoptopenjdk-jre-hotspot-bin-11}/bin"
  '';
}
