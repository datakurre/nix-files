{ pkgs, stdenv, libXScrnSaver, makeWrapper, fetchurl, wrapGAppsHook, glib, gtk3, unzip, atomEnv, libuuid, at-spi2-atk, at-spi2-core, nodePackages, autoPatchelfHook, gcc-unwrapped, libdrm, mesa, libxkbcommon, libxshmfence, lib, libappindicator-gtk3, libglvnd }:

let

  mkElectron = import "${pkgs.path}/pkgs/development/tools/electron/generic.nix" { inherit stdenv libXScrnSaver makeWrapper fetchurl wrapGAppsHook glib gtk3 unzip atomEnv libuuid at-spi2-atk at-spi2-core libdrm mesa libxkbcommon libxshmfence lib libappindicator-gtk3 libglvnd; };
  electron = mkElectron "20.2.0" {
    x86_64-linux = "sha256-wONSLeNIGbg49KNd3zDGKDxhvhu43/Agic2j9kGTiq0=";
  };

  bpmn-js-token-simulation-plugin = fetchurl {
    url = "https://github.com/bpmn-io/bpmn-js-token-simulation-plugin/archive/b433c1454e0edf415372b58fe897569b75cf2c40.tar.gz";
    sha256 = "sha256-5D1wElJrwZeKoiWhVRv2UCaLTs5jebZCTaKrOr9ZgF4=";
  };

  camunda-modeler-robot-plugin = fetchurl {
    url = "https://github.com/datakurre/camunda-modeler-robot-plugin/archive/4c5805fbbde8e28c9c123cdeb4069d66054be402.tar.gz";
    sha256 = "sha256-dnvzBgJtiIqJUYCZc0QuXG1HIcl7AdudprTgRonrjEM=
";
  };

  bpmn-driven-testing-plugin = fetchurl {
    url = "https://github.com/camunda-community-hub/bpmn-driven-testing/releases/download/0.6.0/bpmn-driven-testing-plugin.zip";
    sha256 = "65c84a356b68232c4a438a63b19b24c4d6e2c0282c5aa6ad6d7077bc2e756e86";
  };

  asar = stdenv.mkDerivation rec {
    name = "camunda-modeler-${version}-asar";
    version = "5.6.0";
    src = fetchurl {
      url = "https://github.com/camunda/camunda-modeler/releases/download/v${version}/camunda-modeler-${version}-linux-x64.tar.gz";
      sha256 = "sha256-HJky0jnzpSNoqjzKwBwpw5mpYtzFKicdRU0dZbOQ3CY=";
    };
    nativeBuildInputs = [ nodePackages.asar autoPatchelfHook gcc-unwrapped ];
    installPhase = ''
      asar extract ./resources/app.asar $out
    '';
  };

in

stdenv.mkDerivation rec {
  name = "camunda-modeler-${version}";
  version = "5.6.0";
  src = asar;
  unpackPhase = "";
  nativeBuildInputs = [ electron makeWrapper nodePackages.asar autoPatchelfHook gcc-unwrapped unzip ];
  installPhase = ''
    mkdir -p $out/var/lib/camunda/resources/plugins $out/bin

    cp -a $src build
    chmod u+w -R build
    substituteInPlace build/lib/index.js \
      --replace "let resourcesPaths = [" \
                "let resourcesPaths = [\"$out/var/lib/camunda/resources\","
    asar pack build $out/var/lib/camunda/app.asar

    cd $out/var/lib/camunda/resources/plugins

    tar xzvf ${bpmn-js-token-simulation-plugin}
    tar xzvf ${camunda-modeler-robot-plugin}

    mkdir bpmndt
    cd bpmndt
    unzip ${bpmn-driven-testing-plugin}
    cd ..

    makeWrapper ${electron}/bin/electron $out/bin/camunda-modeler \
      --add-flags "$out/var/lib/camunda/app.asar"
  '';
}
