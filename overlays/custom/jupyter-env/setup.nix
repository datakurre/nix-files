{ pkgs ? import <nixpkgs> {}
, pythonPackages ? pkgs.python3Packages
, setup ? import (pkgs.fetchFromGitHub {
    owner = "datakurre";
    repo = "setup.nix";
    rev = "3660563b961d2cfb81e197288ae9df2d0d9dc648";
    sha256 = "173r6800bds7ignmcai1a5v264awl7y5smy0a3ccip6g4myb1759";
  })
}:

setup {
  inherit pkgs pythonPackages;
  overrides = self: super: {
    "robotframework-SikuliLibrary" = pkgs.robotframework-sikulilibrary;
    "robotframework-seleniumlibrary" = super."robotframework-seleniumlibrary".overridePythonAttrs(old: {
      src = pkgs.fetchFromGitHub {
        owner = "robotframework";
        repo = "seleniumlibrary";
        rev = "34cea295575c5f69e3325ec12b295a6167092acd";
        sha256 = "07x6krwzvlba3087v38q53ww3spn1lmzva5kn9163l1m6kafpvii";
      };
    });
    "backports.shutil-get-terminal-size" = pythonPackages.backports_shutil_get_terminal_size;
    certifi = pythonPackages.certifi;
    decorator = pythonPackages.decorator;
    docutils = pythonPackages.docutils;
    enum34 = pythonPackages.enum34;
    idna = pythonPackages.idna;
    ipykernel = pythonPackages.ipykernel;
    ipython-genutils = pythonPackages.ipython_genutils;
    ipython = pythonPackages.ipython;
    jedi = pythonPackages.jedi;
    jupyter-client = pythonPackages.jupyter_client;
    jupyter-core = pythonPackages.jupyter_core;
    jsonschema = pythonPackages.jsonschema;
    parso = pythonPackages.parso;
    pexpect = pythonPackages.pexpect;
    pillow = pythonPackages.pillow;
    prompt-toolkit = pythonPackages.prompt_toolkit;
    ptyprocess = pythonPackages.ptyprocess;
    pygments = pythonPackages.pygments;
    py = pythonPackages.py;
    python-dateutil = pythonPackages.dateutil;
    pyzmq = pythonPackages.pyzmq;
    requests = pythonPackages.requests;
    singledispatch = pythonPackages.singledispatch;
    six = pythonPackages.six;
    tornado = pythonPackages.tornado;
    traitlets = pythonPackages.traitlets;
    urllib3 = pythonPackages.urllib3;
    wcwidth = pythonPackages.wcwidth;
    chardet = pythonPackages.chardet;
  };
  src = ./.;
  propagatedBuildInputs = with pkgs; [ geckodriver pythonPackages.opencv3 ];
}
