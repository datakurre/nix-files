{ pkgs, ... }:

let
  commonExtensions = with pkgs.vscode-marketplace; [
    datakurre.devenv
    datakurre.vscode-operaton-form-js-modeler
    datakurre.vscode-operaton-bpmn-js-modeler
    datakurre.vscode-operaton-dmn-js-modeler
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
    };
  };

  home.packages = with pkgs; [
    (writeShellScriptBin "codium-java" ''exec ${vscodium}/bin/codium --profile java "$@"'')
    (writeShellScriptBin "codium-python-rust" ''exec ${vscodium}/bin/codium --profile python-rust "$@"'')
    (writeShellScriptBin "codium-elm" ''exec ${vscodium}/bin/codium --profile elm "$@"'')
    (writeShellScriptBin "codium-react" ''exec ${vscodium}/bin/codium --profile react "$@"'')
    (writeShellScriptBin "codium-svelte" ''exec ${vscodium}/bin/codium --profile svelte "$@"'')
    (writeShellScriptBin "codium-python-robot" ''exec ${vscodium}/bin/codium --profile python-robot "$@"'')

    vite
    ruff
    uv
  ];
}
