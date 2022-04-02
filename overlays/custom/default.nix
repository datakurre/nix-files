let

  sources = import ./nix/sources.nix;
  nixpkgs = sources."nixpkgs-21.11";

  config = let pkgs = import sources."nixpkgs-21.11" {}; in {
    allowBroken = true;
    allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
      "code"
      "corefonts"
      "factorio-alpha"
      "font-bh-100dpi"
      "font-bh-lucidatypewriter-100dpi"
      "font-bh-lucidatypewriter-75dpi"
      "ndi"
      "pycharm-professional"
      "teams"
      "vscode"
      "vscode-extension-ms-vsliveshare-vsliveshare"
    ];
  };

  unstable = import sources."nixpkgs-unstable" { inherit config; };

in

self: super:

{

  afew = super.afew.overridePythonAttrs(old: {
    doCheck = false;
    postPatch = ''
      sed -i "s|'notmuch', 'new'|'test', '1'|g" afew/MailMover.py
    '';
  });

  alot = (super.python3Packages.alot.overridePythonAttrs(old: {
    postPatch = ''
      find alot -type f -print0|xargs -0 sed -i "s|payload.encode('raw-unicode-escape')|payload.encode('utf-8')|g"
    '';
  }));

  gitlog = super.stdenv.mkDerivation {
    name = "gitlog";
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out/bin
      cat > $out/bin/gitlog << EOF
      #!/usr/bin/env bash
      git log \
      --pretty=format:"- %s%n  [%an]" "\`git describe --tags|grep -o '^[^-]*'\`"..HEAD
      EOF
      chmod u+x $out/bin/gitlog
    '';
  };

  gitclog = super.stdenv.mkDerivation {
    name = "gitclog";
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out/bin
      cat > $out/bin/gitclog << EOF
      #!/usr/bin/env bash
      head -n 6 \$1 \
      >> \$1.new && gitlog \
      >> \$1.new && tail -n +8 \$1 \
      >> \$1.new && mv \$1.new \$1
      EOF
      chmod u+x $out/bin/gitclog
    '';
  };

  inkscapeFull = let
    myPython2Env = super.python2.withPackages(ps: with ps; [
      numpy
      lxml
      (buildPythonPackage rec {
        name = "${pname}-${version}";
        pname = "scour";
        version = "0.36";
        src = super.fetchurl {
          url = "https://pypi.python.org/packages/1a/9a/7e9f7a40241c1d2659655a5f10ef3d9a84b18365c845f030825d709d59b1/scour-0.36.tar.gz";
          sha256 = "0aizn6yk1nqqz0gqj70hkynf9zgqnab552aix4svy0wygcwlksjb";
        };
        propagatedBuildInputs = [
          six
        ];
      })
    ]);
  in super.inkscape.overrideAttrs(old: {
    postPatch = ''
      patchShebangs share/extensions
      patchShebangs fix-roff-punct
      # Python is used at run-time to execute scripts, e.g., those from
      # the "Effects" menu.
        substituteInPlace src/extension/implementation/script.cpp \
          --replace '"python-interpreter", "python"' '"python-interpreter", "${myPython2Env}/bin/python"'
    '';
    buildInputs = old.buildInputs ++ [ myPython2Env ];
  });

  xterm = (super.xterm.overrideDerivation(old: {
    # fixes issue where locales were broken on non NixOS host
    postInstall = ''
      for prog in $out/bin/*; do
        wrapProgram $prog --set LOCALE_ARCHIVE ${super.glibcLocales}/lib/locale/locale-archive
      done
    '';
  }));

  aspellDicts = super.recurseIntoAttrs (super.callPackages ./super.aspell/dictionaries.nix {});
  camunda-modeler = super.callPackage ./pkgs/camunda-modeler { inherit nixpkgs; };
  findimagedupes = super.callPackage ./pkgs/findimagedupes {};
  fuzzylite = super.callPackage ./pkgs/fuzzylite {};
  jfrog-cli = super.callPackage ./pkgs/jfrog-cli {};
  jupyter-env = super.callPackage ./pkgs/jupyter-env {};
  micromamba = (import sources."nixpkgs-21.11" {}).micromamba;
  mvn2nix = (super.callPackage ./pkgs/mvn2nix { inherit nixpkgs; }).mvn2nix;
  node2nix = super.callPackage ./pkgs/node2nix { inherit nixpkgs; };
  onnxruntime = super.callPackage ./pkgs/onnxruntime {};
  obs-backgroundremoval = super.libsForQt5.callPackage ./pkgs/obs-backgroundremoval {};
  plone-env = super.callPackage ./pkgs/plone-env {};
  plone6-env = super.callPackage ./pkgs/plone6-env {};
  plonetheme-upload = (super.callPackage ./pkgs/plonetheme-upload {}).package;
  poetry2nix = super.callPackage ./pkgs/poetry2nix { inherit nixpkgs; };
  rcc = super.callPackage ./pkgs/rcc {};
  robotframework-sikulilibrary = super.callPackage ./pkgs/sikulilibrary { pythonPackages = super.python3Packages; };
  sikulix = super.callPackage ./pkgs/sikulix {};
  vingester = super.callPackage ./pkgs/vingester { inherit nixpkgs; };
  vcmi = super.callPackage ./pkgs/vcmi {};
  zeebe-modeler = super.callPackage ./pkgs/zeebe-modeler { inherit nixpkgs; };
  zest-releaser-python2 = (import ./pkgs/zest-releaser/release.nix { pkgs = import sources."nixpkgs-20.09" {}; python = "python27"; }).targetPython.pkgs."zest.releaser";
  zest-releaser-python3 = (import ./pkgs/zest-releaser/release.nix { pkgs = import sources."nixpkgs-20.09" {}; python = "python37"; }).targetPython.pkgs."zest.releaser";

  inherit (unstable)
  factorio
  jetbrains
  obs-studio
  obs-studio-plugins
  novnc
  ndi
  teams
  vscode
  vscode-extensions
  vscode-fhsWithPackages
  vscode-utils
  ;
}
