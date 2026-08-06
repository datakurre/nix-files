{ pkgs, ... }:

let
  commonExtensions = with pkgs.vscode-marketplace; [
    vscodevim.vim
    datakurre.devenv
    datakurre.vscode-operaton-form-js-modeler
    datakurre.vscode-operaton-bpmn-js-modeler
    datakurre.vscode-operaton-dmn-js-modeler
  ];
  codiumProfiles = [
    "java"
    "python"
    "python-rust"
    "elm"
    "react"
    "svelte"
    "python-robot"
    "java-python-robot"
  ];
in
{
  programs.vscodium = {
    enable = true;
    package = pkgs.vscodium;
    
    profiles = {
      java = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          redhat.java
          vscjava.vscode-java-debug
          vscjava.vscode-java-test
          vscjava.vscode-maven
        ]);
      };
      
      python = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          ms-python.python
          charliermarsh.ruff
        ]);
      };

      python-rust = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          ms-python.python
          charliermarsh.ruff
          rust-lang.rust-analyzer
          tamasfe.even-better-toml
        ]);
      };
      
      elm = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          elmtooling.elm-ls-vscode
        ]);
      };
      
      react = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          dbaeumer.vscode-eslint
          esbenp.prettier-vscode
          dsznajder.es7-react-js-snippets
          bradlc.vscode-tailwindcss
        ]);
      };
      
      svelte = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          svelte.svelte-vscode
          dbaeumer.vscode-eslint
          esbenp.prettier-vscode
        ]);
      };

      python-robot = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          ms-python.python
          d-biehl.robotcode
        ]);
      };

      java-python-robot = {
        extensions = commonExtensions ++ (with pkgs.vscode-marketplace; [
          redhat.java
          vscjava.vscode-java-debug
          vscjava.vscode-java-test
          vscjava.vscode-maven
          ms-python.python
          d-biehl.robotcode
        ]);
      };
    };
  };

  home.packages = with pkgs; (map (profile: writeShellScriptBin "codium-${profile}" ''exec ${vscodium}/bin/codium --new-window --profile ${profile} "$@"'') codiumProfiles) ++
    (map (profile: makeDesktopItem {
      name = "codium-${profile}";
      desktopName = "VSCodium (${profile})";
      exec = "codium-${profile} %F";
      icon = "vscodium";
      terminal = false;
      type = "Application";
      categories = [ "TextEditor" "Development" "IDE" ];
      startupWMClass = "vscodium";
    }) codiumProfiles) ++ [
    vite
    ruff
    uv
  ];
}
