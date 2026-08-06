{ pkgs, ... }:

let
  manualExtsData = builtins.fromJSON (builtins.readFile ./manual-extensions.json);

  buildManualExt =
    id: data:
    pkgs.vscode-utils.buildVscodeExtension {
      name = "${id}-${data.version}";
      pname = id;
      version = data.version;
      vscodeExtPublisher = data.publisher;
      vscodeExtName = data.name;
      vscodeExtUniqueId = id;
      src = pkgs.fetchurl {
        name = "${id}-${data.version}.vsix";
        url = data.url;
        sha256 = data.sha256;
      };
    };

  manualExtensions = pkgs.lib.mapAttrsToList buildManualExt manualExtsData;

  commonExtensions =
    (with pkgs.open-vsx; [
      vscodevim.vim
      jnoortheen.nix-ide
    ])
    ++ manualExtensions;
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
    package = pkgs.unstable.vscodium;

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
          ++ (with pkgs.open-vsx; [
            charliermarsh.ruff
          ])
          ++ (with pkgs.vscode-extensions; [
            ms-python.python
          ]);
      };

      python-rust = {
        extensions =
          commonExtensions
          ++ (with pkgs.open-vsx; [
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
          ++ (with pkgs.open-vsx; [
            elmtooling.elm-ls-vscode
          ]);
      };

      react = {
        extensions =
          commonExtensions
          ++ (with pkgs.open-vsx; [
            dbaeumer.vscode-eslint
            esbenp.prettier-vscode
            dsznajder.es7-react-js-snippets
            bradlc.vscode-tailwindcss
          ]);
      };

      svelte = {
        extensions =
          commonExtensions
          ++ (with pkgs.open-vsx; [
            svelte.svelte-vscode
            dbaeumer.vscode-eslint
            esbenp.prettier-vscode
          ]);
      };

      python-robot = {
        extensions =
          commonExtensions
          ++ (with pkgs.open-vsx; [
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
          ++ (with pkgs.open-vsx; [
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
      writeShellScriptBin "codium-${profile}" ''exec ${pkgs.unstable.vscodium}/bin/codium --new-window --profile ${profile} "$@"''
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
