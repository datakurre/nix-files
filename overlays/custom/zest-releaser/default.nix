{ pkgs, pythonPackages }:

let

  setup = import (fetchTarball {
    url = "https://github.com/datakurre/setup.nix/archive/d3025ac35cc348d7bb233ee171629630bb4d6864.tar.gz";
    sha256 = "09czivsv81y1qydl7jnqa634bili8z9zvzsj0h3snbr8pk5dzwkj";
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
        mv $prog $prog-python${pythonPackages.python.majorVersion}
        wrapProgram $prog-python${pythonPackages.python.majorVersion} \
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
