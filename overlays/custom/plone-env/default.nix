{ pkgs, python2Packages, ... }:

let self = rec {
  selenium = python2Packages.selenium.overridePythonAttrs (old: rec {
    pname = "selenium";
    version = "3.14.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0h3w592r40bidq40fhjp99yg79z34dwiglic0630ljjnkf8j3jpr";
    };
    propagatedBuildInputs = old.propagatedBuildInputs ++ [
      pkgs.geckodriver
      python2Packages.urllib3
    ];
  });
  robotframework-seleniumlibrary = python2Packages.robotframework-selenium2library.overridePythonAttrs(old: rec {
    pname = "robotframework-seleniumlibrary";
    version = "3.1.1";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0k42g313mqdgmj80hfayks4qshknps0wadizk37m48x273zi74nj";
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
    version = "2.11.3";
    name = "${pname}-nix-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${pname}-${version}.tar.gz";
      sha256 = "0k2l6pvz6y37x4q88sdvx70p6jr9z5v4psd3a9zgcdxrwz6y5zgp";
    };
    postInstall = ''
      sed -i "s|import sys|import sys\nimport os\nsys.executable = os.path.join(sys.prefix, 'bin', os.path.basename(sys.executable))|" $out/bin/buildout
      cat $out/bin/buildout
    '';
    propagatedBuildInputs = [
      dataflake-fakeldap
      python2Packages.pycrypto
      python2Packages.cryptography
      python2Packages.ipdb
      python2Packages.kerberos
      python2Packages.ldap
      (python2Packages.lxml.overridePythonAttrs(old: {
        name = "lxml-3.5.0";
        src = pkgs.fetchurl {
          url = "https://files.pythonhosted.org/packages/8b/be/ed850baac891aca25c832fb8d7b9c0e7a5077a30e336d95ffc7d649aaa06/lxml-3.5.0.tar.gz";
          sha256 = "349f93e3a4b09cc59418854ab8013d027d246757c51744bf20069bc89016f578";
        };
      }))
      python2Packages.pillow
      python2Packages.python_magic
      python2Packages.pyscss
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
    pkgs.firefox
    pkgs.geckodriver
    (pkgs.pythonFull.buildEnv.override {
      extraLibs = [ buildout ] ++ buildout.propagatedNativeBuildInputs;
    })
  ];
}
