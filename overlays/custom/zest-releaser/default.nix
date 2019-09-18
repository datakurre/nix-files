{ pkgs ? import <nixpkgs> {}
, pythonPackages ? pkgs.pythonPackages
}:

let

  setup = import (fetchTarball {
    url = "https://github.com/nix-community/setup.nix/archive/v3.1.0.tar.gz";
    sha256 = "1dp9bzivqaqqc2d9bnfy6jh37rfz6mvqaqbxy34l998y0khv5fpv";
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
    propagatedBuildInputs = old.propagatedBuildInputs ++ [ manifest_python ];
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
}
