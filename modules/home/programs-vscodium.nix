{ pkgs, ... }:

let
  devenv-pin = builtins.fromJSON (builtins.readFile ./datakurre.devenv.json);
  devenv-ext = pkgs.vscode-utils.buildVscodeExtension {
    name = "datakurre.devenv-${devenv-pin.version}";
    pname = "datakurre.devenv";
    version = devenv-pin.version;
    vscodeExtPublisher = "datakurre";
    vscodeExtName = "devenv";
    vscodeExtUniqueId = "datakurre.devenv";
    src = pkgs.fetchurl {
      url = devenv-pin.url;
      sha256 = devenv-pin.sha256;
    };
  };

  commonExtensions = with pkgs.vscode-marketplace; [
    vscodevim.vim
    devenv-ext
    datakurre.vscode-operaton-form-js-modeler
    datakurre.vscode-operaton-bpmn-js-modeler
    datakurre.vscode-operaton-dmn-js-modeler
    jnoortheen.nix-ide
  ];
  codiumProfiles = [
    "plain"
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
      plain = {
        extensions = commonExtensions;
      };

      java = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-extensions; [
            (vscjava.vscode-java-debug.overrideAttrs (old: {
              postInstall = (old.postInstall or "") + ''
                sed -i 's|".noConfigDebugAdapterEndpoints"|"../../../../../../../../../../tmp/.noConfigDebugAdapterEndpoints"|g' $out/share/vscode/extensions/vscjava.vscode-java-debug/dist/extension.js
              '';
            }))
            vscjava.vscode-java-test
            vscjava.vscode-maven
            redhat.java
          ]);
      };

      python = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-marketplace; [
            charliermarsh.ruff
          ])
          ++ (with pkgs.vscode-extensions; [
            ms-python.python
          ]);
      };

      python-rust = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-marketplace; [
            charliermarsh.ruff
            rust-lang.rust-analyzer
            tamasfe.even-better-toml
          ])
          ++ (with pkgs.vscode-extensions; [
            ms-python.python
          ]);
      };

      elm = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-marketplace; [
            elmtooling.elm-ls-vscode
          ]);
      };

      react = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-marketplace; [
            dbaeumer.vscode-eslint
            esbenp.prettier-vscode
            dsznajder.es7-react-js-snippets
            bradlc.vscode-tailwindcss
          ]);
      };

      svelte = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-marketplace; [
            svelte.svelte-vscode
            dbaeumer.vscode-eslint
            esbenp.prettier-vscode
          ]);
      };

      python-robot = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-marketplace; [
            d-biehl.robotcode
          ])
          ++ (with pkgs.vscode-extensions; [
            ms-python.python
            (ms-python.debugpy.overrideAttrs (old: {
              postInstall = (old.postInstall or "") + ''
                sed -i 's|".noConfigDebugAdapterEndpoints"|"../../../../../../../../../../tmp/.noConfigDebugAdapterEndpoints"|g' $out/share/vscode/extensions/ms-python.debugpy/dist/extension.js
              '';
            }))
          ]);
      };

      java-python-robot = {
        extensions =
          commonExtensions
          ++ (with pkgs.vscode-marketplace; [
            d-biehl.robotcode
          ])
          ++ (with pkgs.vscode-extensions; [
            (vscjava.vscode-java-debug.overrideAttrs (old: {
              postInstall = (old.postInstall or "") + ''
                sed -i 's|".noConfigDebugAdapterEndpoints"|"../../../../../../../../../../tmp/.noConfigDebugAdapterEndpoints"|g' $out/share/vscode/extensions/vscjava.vscode-java-debug/dist/extension.js
              '';
            }))
            vscjava.vscode-java-test
            vscjava.vscode-maven
            ms-python.python
            (ms-python.debugpy.overrideAttrs (old: {
              postInstall = (old.postInstall or "") + ''
                sed -i 's|".noConfigDebugAdapterEndpoints"|"../../../../../../../../../../tmp/.noConfigDebugAdapterEndpoints"|g' $out/share/vscode/extensions/ms-python.debugpy/dist/extension.js
              '';
            }))
            redhat.java
          ]);
      };
    };
  };

  home.packages =
    with pkgs;
    (map (
      profile:
      writeShellScriptBin "codium-${profile}" ''exec ${vscodium}/bin/codium --new-window --profile ${profile} "$@"''
    ) codiumProfiles)
    ++ (map (
      profile:
      makeDesktopItem {
        name = "codium-${profile}";
        desktopName = "VSCodium (${profile})";
        exec = "codium-${profile} %F";
        icon = "vscodium";
        terminal = false;
        type = "Application";
        categories = [
          "TextEditor"
          "Development"
          "IDE"
        ];
        startupWMClass = "vscodium";
      }
    ) codiumProfiles)
    ++ [
      vite
      ruff
      uv
    ];

  xdg.desktopEntries = {
    "codium" = {
      name = "VSCodium";
      noDisplay = true;
    };
    "codium-url-handler" = {
      name = "VSCodium - URL Handler";
      noDisplay = true;
    };
  };
}
