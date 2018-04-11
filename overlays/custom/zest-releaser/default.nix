{ pkgs, pythonPackages }:

let

  setup = import (pkgs.fetchFromGitHub {
    owner = "datakurre";
    repo = "setup.nix";
    rev = "4ca906f296ab62345f6b4afa2b5e9acc2d417302";
    sha256 = "0fkvazlmps6isy5j328xp9pd7gh9r0nj4sm4xb2rcwywmwkb9pc6";
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
};

in setup {
  inherit pkgs pythonPackages overrides;
  src = ./requirements.nix;
  force = true;
}
