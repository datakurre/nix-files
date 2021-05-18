{ stdenv, libXScrnSaver, makeWrapper, fetchurl, wrapGAppsHook, glib, gtk3, unzip, atomEnv, libuuid, at-spi2-atk, at-spi2-core, nodePackages, autoPatchelfHook, gcc-unwrapped, libdrm, mesa, libxkbcommon, adoptopenjdk-jre-hotspot-bin-11, libxshmfence, lib, libappindicator-gtk3}:

let

  mkElectron = import (<unstable> + "/pkgs/development/tools/electron/generic.nix") { inherit stdenv libXScrnSaver makeWrapper fetchurl wrapGAppsHook glib gtk3 unzip atomEnv libuuid at-spi2-atk at-spi2-core libdrm mesa libxkbcommon libxshmfence lib libappindicator-gtk3; };
  electron = mkElectron "12.0.7" {
    x86_64-linux = "335b77b35361fac4e2df1b7e8de5cf055e0a1a2065759cb2dd4508e8a77949fa";
    x86_64-darwin = "c3238c9962c5ad0f9de23c9314f07e03410d096d7e9f9d91016dab2856606a9e";
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

  camunda-modeler-tooltip-plugin = fetchurl { 
    url = "https://github.com/viadee/camunda-modeler-tooltip-plugin/archive/7255229c1abbf1cb71b3758b3939d5b9ff05d0e8.tar.gz";
    sha256 = "0dq679dr2p44mazqvkiiryv69hqc4kgag2507crm0f2qmywbj8c9";
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
  version = "4.8.1";
  src = fetchurl {
    url = "https://github.com/camunda/camunda-modeler/releases/download/v${version}/camunda-modeler-${version}-linux-x64.tar.gz";
    sha256 = "11ymcqf1cs3x982vg6n1ndi5i5zz9l0jbpz973wx9g3cfdmgw54l";
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
    autoPatchelf node_modules/grpc/src/node/extension_binary/electron-v12.0-linux-x64-glibc/grpc_node.node;
    asar pack . ../app.asar
    cd ..
    mkdir -p $out/var/lib/camunda/resources/plugins $out/bin
    cp app.asar $out/var/lib/camunda
    cd $out/var/lib/camunda/resources/plugins
    tar xzvf ${dmn-testing-plugin}
    tar xzvf ${camunda-modeler-robot-plugin}
    tar xzvf ${bpmn-js-token-simulation-plugin}
    tar xzvf ${camunda-modeler-linter-plugin}

    # Fix camunda/camunda-modeler-linter-plugin to allow inclusive gateways
    substituteInPlace camunda-modeler-linter-plugin-*/dist/client.js \
      --replace '"no-inclusive-gateway": "error",' "" \
      --replace '"camunda/avoid-lanes": "warn",' ""

    tar xzvf ${camunda-modeler-tooltip-plugin}
    tar xzvf ${camunda-modeler-property-info-plugin}
    tar xzvf ${camunda-modeler-plugins}
    mv camunda-modeler-plugins*/camunda-transaction-boundaries-plugin .
    rm -r camunda-modeler-plugins*
    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar" \
      --prefix PATH : "${adoptopenjdk-jre-hotspot-bin-11}/bin"
  '';
}
