{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/b2b5c1d5af2a3e743a388ffb807db64d6b776a5d.tar.gz";
    sha256 = "0zg58h2j55aq8pr5b16h2jlns1215rnc2q9ap4bmdcxcqidm62ra";
  }) {}
, setup ? import (fetchTarball {
    url = "https://github.com/datakurre/setup.nix/archive/a05ef605ae476a07ba1f8b0c2e1ce95d0eca8355.tar.gz";
    sha256 = "0ih9ccy54hcij7z49mfxpyvl1wdsh00kr9714scza9b101s4gpap";
 })
, pythonPackages ? pkgs.python2Packages
}:

let overrides = self: super: {
  "dataflake-fakeldap" = super."dataflake-fakeldap".overridePythonAttrs(old: {
    buildInputs = [ self."setuptools-git" ];
  });
  "fancycompleter" = super."fancycompleter".overridePythonAttrs(old: {
    buildInputs = [ self."setuptools-scm" ];
  });
  "pdbpp" = super."pdbpp".overridePythonAttrs(old: {
    buildInputs = [ self."setuptools-scm" ];
  });
  "pycrypto" = super."pycrypto".overridePythonAttrs(old: {
    buildInputs = [ self."pycryptodome" ];
  });
  "python-magic" = pythonPackages."magic";
  "robotframework" = super."robotframework".overridePythonAttrs(old: {
    nativeBuildInputs = [ pkgs."unzip" ];
  });
  "zc.buildout" = pythonPackages.zc_buildout_nix.overridePythonAttrs (old: {
    name = super."zc.buildout".name;
    src = super."zc.buildout".src;
    postInstall = ''
      sed -i "s|import sys|import sys\nimport os\nsys.executable = os.path.join(sys.prefix, 'bin', os.path.basename(sys.executable))|" $out/bin/buildout
    '';
#   propagatedBuildInputs = [
#     self."dataflake-fakeldap"
#     self."pycrypto"
#     self."cryptography"
#     self."kerberos"
#     self."python-ldap"
#     self."lxml"
#     self."pillow"
#     self."python_magic"
#     self."pyscss"
#     self."watchdog"
#     self."gnureadline"
#     self."robotframework-selenium2library"
#   ];
  });
};

in pkgs.stdenv.mkDerivation rec {
  name = "plone-env";
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';
  buildInputs = [
    pkgs.firefox
    pkgs.geckodriver
    (setup {
      inherit pkgs pythonPackages overrides;
      src = ./requirements.nix;
    }).env
  ];
}
