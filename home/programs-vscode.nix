{
  pkgs,
  lib,
  ...
}:
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscode-with-extensions.override {
      vscodeExtensions = [
        pkgs.vscode-extensions.bbenoist.nix
        pkgs.vscode-extensions.ms-pyright.pyright
        pkgs.vscode-extensions.ms-python.python
        pkgs.vscode-extensions.ms-python.debugpy
        pkgs.vscode-extensions.ms-vscode.makefile-tools
        pkgs.vscode-extensions.vscodevim.vim
        (pkgs.vscode-extensions.charliermarsh.ruff.overrideAttrs (old: {
          postInstall = ''
            rm -f $out/share/vscode/extensions/charliermarsh.ruff/bundled/libs/bin/ruff
            ln -s ${pkgs.ruff}/bin/ruff $out/share/vscode/extensions/charliermarsh.ruff/bundled/libs/bin/ruff
          '';
        }))
        pkgs.vscode-extensions.tamasfe.even-better-toml
        pkgs.vscode-extensions.github.copilot
        pkgs.vscode-extensions.github.copilot-chat
      ];
    };
  };
}
