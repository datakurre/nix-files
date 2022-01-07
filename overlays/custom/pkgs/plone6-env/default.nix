{ pkgs, python3Packages, ... }:

let self = rec {
  robotframework-seleniumlibrary = python3Packages.robotframework-selenium2library.overridePythonAttrs(old: rec {
    pname = "robotframework-seleniumlibrary";
    version = "3.1.1";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0k42g313mqdgmj80hfayks4qshknps0wadizk37m48x273zi74nj";
    };
    propagatedBuildInputs = [
      python3Packages.decorator
      python3Packages.docutils
      python3Packages.robotframework
      python3Packages.selenium
    ];
  });
  robotframework-selenium2library = python3Packages.robotframework-selenium2library.overridePythonAttrs(old: rec {
    pname = "robotframework-selenium2library";
    version = "3.0.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "19zxf2f5f6ply2ab4q9l52cn3d6j6j5h0f9h4pnnvcc80wmr93ia";
    };
    propagatedBuildInputs = [
      robotframework-seleniumlibrary
    ];
  });
  buildout = python3Packages.zc_buildout_nix.overridePythonAttrs (old: rec {
    pname = "zc.buildout";
    version = "2.12.2";
    name = "${pname}-nix-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${pname}-${version}.tar.gz";
      sha256 = "0hx0a24r8b6gl48silhqhpfjksamrvv5xlr523z8vnk12f57wpgz";
    };
    postInstall = ''
      sed -i "s|import sys|import sys\nimport os\nsys.executable = os.path.join(sys.prefix, 'bin', os.path.basename(sys.executable))|" $out/bin/buildout
      cat $out/bin/buildout
    '';
    propagatedBuildInputs = [
      python3Packages.pycrypto
      python3Packages.cryptography
      python3Packages.ipdb
      python3Packages.kerberos
      python3Packages.ldap
      python3Packages.lxml
      python3Packages.pillow
      python3Packages.python_magic
      python3Packages.pyscss
      python3Packages.watchdog
      python3Packages.gnureadline
      robotframework-selenium2library
    ];
  });
};

in pkgs.stdenv.mkDerivation rec {
  name = "plone-env";
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';
  buildInputs = with self; [
    buildout
    pkgs.firefox
    pkgs.geckodriver
    (pkgs.pythonFull.buildEnv.override {
      extraLibs = [ buildout ] ++ buildout.propagatedNativeBuildInputs;
    })
  ];
}
