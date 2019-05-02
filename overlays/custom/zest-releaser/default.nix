{ pkgs, pythonPackages }:

let

  setup = import (fetchTarball {
    url = "https://github.com/datakurre/setup.nix/archive/a05ef605ae476a07ba1f8b0c2e1ce95d0eca8355.tar.gz";
    sha256 = "0ih9ccy54hcij7z49mfxpyvl1wdsh00kr9714scza9b101s4gpap";
  });

  manifest_python = pythonPackages.python.withPackages(ps: [
    ps.setuptools ps.wheel
  ]);

  overrides = self: super: {

  # check-manifest requires Python interpreter able to import setup.py
  "check-manifest" = super."check-manifest".overridePythonAttrs(old: {
    postPatch = ''
      substituteInPlace check_manifest.py \
        --replace "os.path.abspath(python)" \
                  "\"${manifest_python.interpreter}\""
    '';
    propagatedBuildInputs = [ manifest_python ];
  });

  # building wheels require SOURCE_DATE_EPOCH
  "zest.releaser" = super."zest.releaser".overridePythonAttrs(old: {
    postInstall = ''
      for prog in $out/bin/*; do
        mv $prog $prog-python${pythonPackages.python.pythonVersion}
        wrapProgram $prog-python${pythonPackages.python.pythonVersion} \
          --set SOURCE_DATE_EPOCH 315532800
      done
    '';
  });

  "wheel" = null;  # wheel is implicit in nixpkgs and cannot be installed
};

in setup {
  inherit pkgs pythonPackages overrides;
  src = ./requirements.nix;
  force = true;
}
