{ pkgs, python36Packages}:

with pkgs;

let self = rec {

  pythonPackages = python36Packages;

  # patches

  robot_mode = ./robotframework.js;

  notebook = pythonPackages.notebook.overridePythonAttrs(old: { 
    postInstall = ''
      ls $out/${pythonPackages.python.sitePackages}/notebook/static/components/codemirror/mode/
      mkdir $out/${pythonPackages.python.sitePackages}/notebook/static/components/codemirror/mode/robotframework
      cp ${robot_mode} $out/${pythonPackages.python.sitePackages}/notebook/static/components/codemirror/mode/robotframework/robotframework.js
    '';
    doCheck = false;
  });

  # kernels

  python36_with_packages = python36.buildEnv.override {
    extraLibs = with python36Packages; [
      iplantuml
      ipykernel
      ipywidgets
      lti
      magic
      requests
    ];
  };

  python36_kernel = stdenv.mkDerivation rec {
    name = "python36";
    buildInputs = [ python36_with_packages ];
    json = builtins.toJSON {
      argv = [ "${python36_with_packages}/bin/python3.6"
               "-m" "ipykernel" "-f" "{connection_file}" ];
      display_name = "Python 3.6";
      language = "python";
      env = { PYTHONPATH = ""; };
    };
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out
      cat > $out/kernel.json << EOF
      $json
      EOF
    '';
  };

  lti = pythonPackages.buildPythonPackage rec {
    pname = "lti";
    version = "0.9.2";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0k9d6ii7wf0iym0jspw0bpc4vvkk7xz7wibvmbzjjzn5rrnb3ak4";
    };
    propagatedBuildInputs = with pythonPackages; [
      lxml
      requests_oauthlib
    ];
    doCheck = false;
  };

  iplantuml = pythonPackages.buildPythonPackage rec {
    pname = "IPlantUML";
    version = "0.1.1";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0p1w48iik5cl137b4xy3g5x52wl5b9fayxlr97bzd5v65dqxz1hg";
    };
    buildInputs = with pythonPackages; [ notebook ];
    propagatedBuildInputs = [
      plantweb
      plantuml
    ];
    doCheck = false;
  };

  plantweb = pythonPackages.buildPythonPackage rec {
    pname = "plantweb";
    version = "1.1.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "1974022rpmwjg1amc7k8n3274bfj0vychd5c81r6q4mm02kysksz";
    };
    propagatedBuildInputs = with pythonPackages; [
      docutils
      requests
    ];
  };

  robot_with_packages = buildEnv {
    name = "robotkernel";
    paths = [
      (python36.buildEnv.override {
        extraLibs = with python36Packages; [
          ipykernel
          ipywidgets
          (import ./setup.nix {
            inherit pkgs;
            pythonPackages = python36Packages;
          }).build
        ];
      })
    ];
  };

  robot_kernel = stdenv.mkDerivation rec {
    name = "robot";
    buildInputs = [ robot_with_packages ];
    json = builtins.toJSON {
      argv = [ "${robot_with_packages}/bin/python3.6"
               "-m" "robotkernel" "-f" "{connection_file}" ];
      display_name = "Robot Framework";
      language = "robotframework";
      env = { PYTHONPATH = ""; };
    };
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out
      cat > $out/kernel.json << EOF
      $json
      EOF
    '';
  };

  # extensions

  rise = pythonPackages.buildPythonPackage rec {
    pname = "rise";
    version = "5.1.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0b5rimnzd6zkgs7f286vr58a5rlzv275zd49xw48mn4dc06wfpz9";
    };
    buildInputs = [ notebook ];
    postPatch = ''
      sed -i "s|README.md'|README.md', encoding='utf-8'|" setup.py
    '';
  };

  jupyter_nbextensions_configurator = pythonPackages.buildPythonPackage rec {
    pname = "jupyter_nbextensions_configurator";
    version = "0.3.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "11qq1di2gas8r302xpa0h2xndd5qgrz4a77myd2bd43c0grffa6b";
    };
    doCheck = false;
    installFlags = [ "--no-dependencies" ];
    propagatedBuildInputs = with pythonPackages; [ pyyaml ];
  };

  jupyter_contrib_nbextensions = pythonPackages.buildPythonPackage rec {
    pname = "jupyter_contrib_nbextensions";
    version = "0.3.3";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0v730d5sqx6g106ii5r08mghbmbqi12pm6mpvjc0vsx703syd83f";
    };
    doCheck = false;
    installFlags = [ "--no-dependencies" ];
    propagatedBuildInputs = with pythonPackages; [ lxml ];
  };

  vim_binding = fetchFromGitHub {
    owner = "lambdalisue";
    repo = "jupyter-vim-binding";
    rev = "c9822c753b6acad8b1084086d218eb4ce69950e9";
    sha256 = "1951wnf0k91h07nfsz8rr0c9nw68dbyflkjvw5pbx9dmmzsa065j";
  };

  # notebook

  jupyter = pythonPackages.jupyter.overridePythonAttrs (old: {
    propagatedBuildInputs = old.propagatedBuildInputs ++ [
      jupyter_contrib_nbextensions
      jupyter_nbextensions_configurator
      rise
    ];
    postInstall = with pythonPackages; ''
      mkdir -p $out/bin
      ln -s ${jupyter_core}/bin/jupyter $out/bin
      wrapProgram $out/bin/jupyter \
        --prefix PYTHONPATH : "${notebook}/${python.sitePackages}:$PYTHONPATH" \
        --prefix PATH : "${notebook}/bin:$PATH"
    '';
  });

  jupyter_nbconfig = stdenv.mkDerivation rec {
    name = "jupyter";
    json = builtins.toJSON {
      load_extensions = {
        "rise/main" = true;
        "python-markdown/main" = true;
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
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out
      cat > $out/notebook.json << EOF
      $json
      EOF
    '';
  };

  jupyter_config_dir = stdenv.mkDerivation {
    name = "jupyter";
    buildInputs = [
      python36_kernel
      robot_kernel
      rise
      vim_binding
    ];
    builder = writeText "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out/etc/jupyter/nbextensions
      mkdir -p $out/etc/jupyter/kernels
      mkdir -p $out/etc/jupyter/migrated
      ln -s ${python36_kernel} $out/etc/jupyter/kernels/${python36_kernel.name}
      ln -s ${robot_kernel} $out/etc/jupyter/kernels/${robot_kernel.name}
      ln -s ${jupyter_nbconfig} $out/etc/jupyter/nbconfig
      ln -s ${jupyter_contrib_nbextensions}/${pythonPackages.python.sitePackages}/jupyter_contrib_nbextensions/nbextensions/* $out/etc/jupyter/nbextensions
      ln -s ${rise}/${pythonPackages.python.sitePackages}/rise/static $out/etc/jupyter/nbextensions/rise
      ln -s ${vim_binding} $out/etc/jupyter/nbextensions/vim_binding
      cat > $out/etc/jupyter/jupyter_notebook_config.py << EOF
      import os
      import rise
      c.KernelSpecManager.whitelist = {
        '${python36_kernel.name}',
        '${robot_kernel.name}'
      }
      c.NotebookApp.ip = os.environ.get('JUPYTER_NOTEBOOK_IP', 'localhost')
      EOF

      cat > $out/etc/jupyter/jupyter_nbconvert_config.py << EOF
      c = get_config()
      c.Exporter.preprocessors = ['jupyter_contrib_nbextensions.nbconvert_support.pre_pymarkdown.PyMarkdownPreprocessor']
      EOF
    '';
  };
};

in with self;

stdenv.mkDerivation rec {
  name = "jupyter";
  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';
  buildInputs = [
    jupyter
    jupyter_config_dir
    geckodriver
    chromedriver
    plantweb
    plantuml
    pandoc
    inkscape
    (texlive.combine {
      inherit (texlive)
      adjustbox
      collectbox
      enumitem
      mathpazo
      palatino
      scheme-small
      ucs;
    })
  ] ++ stdenv.lib.optionals stdenv.isLinux [ bash fontconfig tini ];
  shellHook = ''
    mkdir -p $PWD/.jupyter
    export JUPYTER_CONFIG_DIR=${jupyter_config_dir}/etc/jupyter
    export JUPYTER_PATH=${jupyter_config_dir}/etc/jupyter
    export JUPYTER_DATA_DIR=$PWD/.jupyter
    export JUPYTER_RUNTIME_DIR=$PWD/.jupyter
  '';
}
