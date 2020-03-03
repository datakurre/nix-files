self: super:

{

  afew = super.afew.overrideAttrs(old: {
    postPatch = ''
      sed -i "s|'notmuch', 'new'|'test', '1'|g" afew/MailMover.py
    '';
  });

  aspellDicts = super.recurseIntoAttrs (super.callPackages ./aspell/dictionaries.nix {});

  camunda-modeler = super.callPackage ./camunda-modeler {};

  zeebe-modeler = super.callPackage ./zeebe-modeler {};

  subversion_1_7_14 = super.callPackage ./subversion {
    sqlite = super.callPackage ./subversion/sqlite.nix {};
  };

  fuzzylite = super.callPackage ./fuzzylite {};
  vcmi = super.callPackage ./vcmi {};

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

  jfrog-cli = super.callPackage ./jfrog-cli {};

  jupyter-env = super.callPackage ./jupyter-env {};

  sikulix = super.callPackage ./sikulix {};

  robotframework-sikulilibrary = super.callPackage ./sikulilibrary {
    pythonPackages = self.python3Packages;
  };

  findimagedupes = super.callPackage ./findimagedupes {};

  zest-releaser-python2 = (import ./zest-releaser/release.nix {
    pkgs = self;
    python = "python27";
  }).targetPython.pkgs."zest.releaser";

  zest-releaser-python3 = (import ./zest-releaser/release.nix {
    pkgs = self;
    python = "python37";
  }).targetPython.pkgs."zest.releaser";

  gmime = super.gmime.overrideAttrs(old: {
    propagatedBuildInputs = old.propagatedBuildInputs ++ [ self.gpgme.dev ];
  });

  jetbrains = (super.recurseIntoAttrs (super.callPackages ./jetbrains {
    # jdk = self.oraclejdk8;
  }));

  pidgin-with-plugins = super.pidgin-with-plugins.override {
    plugins = [ self.pidginsipe ];
  };

  plone-env = super.callPackage ./plone-env/setup.nix {};
  plone6-env = super.callPackage ./plone6-env {};

  plonetheme-upload = (super.callPackage ./plonetheme-upload {}).package;

  powerline-fonts = super.callPackage ./powerline-fonts {};

  inkscapeFull = let
    myPython2Env = self.python2.withPackages(ps: with ps; [
      numpy
      lxml
      (buildPythonPackage rec {
        name = "${pname}-${version}";
        pname = "scour";
        version = "0.36";
        src = super.fetchurl {
          url = "https://pypi.python.org/packages/1a/9a/7e9f7a40241c1d2659655a5f10ef3d9a84b18365c845f030825d709d59b1/scour-0.36.tar.gz";
          # url = "mirror://pypi/r/${pname}/${name}.tar.gz";
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
}
