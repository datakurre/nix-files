{ pkgs, python2Packages, ... }:

let self = rec {
  selenium = python2Packages.selenium.overridePythonAttrs (old: rec {
    pname = "selenium";
    version = "3.9.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "a34a833d89bcfb463bfba5e5515a9276bb94221787b409f0ad28d2f91903e31d";
    };
    propagatedBuildInputs = old.propagatedBuildInputs ++ [
      pkgs.geckodriver
    ];
  });
  robotframework-seleniumlibrary = python2Packages.robotframework-selenium2library.overridePythonAttrs(old: rec {
    pname = "robotframework-seleniumlibrary";
    version = "3.1.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0zql4dvnbv2z7lf055nk6cwagrmp0yhxb3pm17irw0flxj2b87pc";
    };
    propagatedBuildInputs = [
      python2Packages.decorator
      python2Packages.docutils
      python2Packages.robotframework
      selenium
    ];
  });
  robotframework-selenium2library = python2Packages.robotframework-selenium2library.overridePythonAttrs(old: rec {
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
  dataflake-fakeldap = python2Packages.buildPythonPackage rec {
    pname = "dataflake.fakeldap";
    version = "1.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "521e1f302f2cebd8da39e06b429ec9630cc40622d06ff095606adb06091c85d2";
    };
    buildInputs = [
      python2Packages.setuptools-git
    ];
    propagatedBuildInputs = [
      python2Packages.ldap
    ];
  };
  buildout = python2Packages.zc_buildout_nix.overridePythonAttrs (old: rec {
    pname = "zc.buildout";
    version = "2.11.0";
    name = "${pname}-nix-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${pname}-${version}.tar.gz";
      sha256 = "092b0a147d5fb4e79ee0afde665570f85738e714463854f9e4f7f38d0b27ea82";
    };
    postInstall = "";
    propagatedBuildInputs = [
      dataflake-fakeldap
      python2Packages.pycrypto
      python2Packages.cryptography
      python2Packages.ipdb
      python2Packages.kerberos
      python2Packages.ldap
      python2Packages.lxml
      python2Packages.pillow
      python2Packages.python_magic
      python2Packages.watchdog
      python2Packages.gnureadline
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
    pkgs.geckodriver
    (pkgs.pythonFull.buildEnv.override {
      extraLibs = buildout.propagatedNativeBuildInputs;
    })
  ];
}
