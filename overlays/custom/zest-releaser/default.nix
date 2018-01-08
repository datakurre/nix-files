{ pkgs, pythonPackages }:

let

  setup = import (pkgs.fetchFromGitHub {
    owner = "datakurre";
    repo = "setup.nix";
    rev = "506afc61cd923d90ef1910337ccf87ee9786d736";
    sha256 = "057g8vbz9knvlbs94dlxsb5jii9x3p2cx5xzrx09cicq2227fqnz";
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
