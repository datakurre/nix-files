{ pkgs ? import (builtins.fetchTarball {
    # branches nixos-19.03
    url = "https://github.com/NixOS/nixpkgs-channels/archive/96151a48dd6662fb3f84bd16bbfe8a34f59d717a.tar.gz";
    sha256 = "06cqc37yj23g3jbwvlf9704bl5dg8vrzqvs5y2q18ayg9sw61i6z";
  }) {}
}:

let self = rec {

  # python packages

  pythonPackages = (import ./setup.nix {
    inherit pkgs;
    pythonPackages = pkgs.python3Packages;
  }).pythonPackages;

  # other packages

  vim_binding = pkgs.fetchFromGitHub {
    owner = "lambdalisue";
    repo = "jupyter-vim-binding";
    rev = "c9822c753b6acad8b1084086d218eb4ce69950e9";
    sha256 = "1951wnf0k91h07nfsz8rr0c9nw68dbyflkjvw5pbx9dmmzsa065j";
  };

  # jupyter

  jupyter = pythonPackages.jupyter.overridePythonAttrs (old: {
    propagatedBuildInputs =
    with pythonPackages; old.propagatedBuildInputs ++ [
      graphviz
      ipywidgets
      jupyter-contrib-nbextensions
      jupyter-nbextensions-configurator
      jupyterlab
      nbimporter
      opencv3
      rise
      tkinter
      widgetsnbextension
    ];
  });

  jupyter_nbconfig = pkgs.stdenv.mkDerivation rec {
    name = "jupyter";
    json = builtins.toJSON {
      load_extensions = {
        "jupyter-js-widgets/extension" = true;
        "rise/main" = true;
        "vim_binding/vim_binding" = true;
      };
      keys = {
        command = {
          bind = {
            "Ctrl-7" = "RISE:toggle-slide";
            "Ctrl-8" = "RISE:toggle-subslide";
            "Ctrl-9" = "RISE:toggle-fragment";
          };
        };
      };
    };
    builder = with pkgs; builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out
      cat > $out/notebook.json << EOF
      $json
      EOF
    '';
  };

  jupyter_config_dir = pkgs.stdenv.mkDerivation {
    name = "jupyter";
    builder = with pythonPackages; with pkgs; writeText "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out/etc/jupyter/migrated
      mkdir -p $out/share/jupyter/nbextensions
      mkdir -p $out/share/jupyter/migrated

      ln -s ${jupyter_nbconfig} $out/share/jupyter/nbconfig
      ln -s ${jupyter-contrib-nbextensions}/${pythonPackages.python.sitePackages}/jupyter-contrib-nbextensions/nbextensions/* $out/share/jupyter/nbextensions
      ln -s ${rise}/${pythonPackages.python.sitePackages}/rise/static $out/share/jupyter/nbextensions/rise
      ln -s ${vim_binding} $out/share/jupyter/nbextensions/vim_binding
      ln -s ${widgetsnbextension}/share/jupyter/nbextensions/* $out/share/jupyter/nbextensions

      echo "import rise" >> $out/share/jupyter/jupyter_notebook_config.py

      cat > $out/share/jupyter/jupyter_nbconvert_config.py << EOF
      c = get_config()
      c.Exporter.preprocessors = ['jupyter_contrib_nbextensions.nbconvert_support.pre_pymarkdown.PyMarkdownPreprocessor']
      EOF
    '';
  };
};

in with self;

pkgs.stdenv.mkDerivation rec {
  name = "jupyter";
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';
  buildInputs = [
    jupyter
    jupyter_config_dir
    pkgs.geckodriver
    pkgs.chromedriver
    pkgs.pandoc
    pkgs.inkscape
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
      adjustbox
      collectbox
      enumitem
      mathpazo
      palatino
      scheme-small
      ucs;
    })
  ];
  shellHook = ''
    mkdir -p $PWD/.jupyter
    export JUPYTER_CONFIG_DIR=${jupyter_config_dir}/etc/jupyter
    export JUPYTER_PATH=${jupyter_config_dir}/etc/jupyter
    export JUPYTER_DATA_DIR=$PWD/.jupyter
    export JUPYTER_RUNTIME_DIR=$PWD/.jupyter
  '';
}
