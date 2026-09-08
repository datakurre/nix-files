{
  config,
  pkgs,
  lib,
  nixgl,
  operatonBpmnModeler,
  ...
}:
let
  nixglConfig =
    if builtins.pathExists ./nixgl-local.json then
      builtins.fromJSON (builtins.readFile ./nixgl-local.json)
    else
      null;
  nixglPackage = lib.optional (nixglConfig != null && nixglConfig.nvidiaVersion != null) (
    let
      nixglPkgs = import nixgl.inputs.nixpkgs {
        system = nixglConfig.system;
        config.allowUnfree = true;
      };
      nixglWrappers = import (nixgl + "/default.nix") {
        pkgs = nixglPkgs;
        nvidiaVersion = nixglConfig.nvidiaVersion;
      };
    in
    pkgs.runCommand "nixGLNvidia" { } ''
      mkdir -p $out/bin
      ln -s ${nixglWrappers.nixGLNvidia}/bin/nixGLNvidia-${nixglConfig.nvidiaVersion} $out/bin/nixGLNvidia
    ''
  );
  bpmnModeler = lib.optional (nixglConfig != null && nixglConfig.nvidiaVersion != null) (
    pkgs.writeShellScriptBin "bpmn-modeler" ''
      exec env GDK_BACKEND=x11 nixGLNvidia \
        ${operatonBpmnModeler.packages.${pkgs.system}.default}/bin/operaton-bpmn-editor "$@"
    ''
  );
  riverSession = pkgs.writeShellScriptBin "river-session" ''
    if [ -f "${config.home.homeDirectory}/.bashrc.d/99-nix.sh" ]; then
      . "${config.home.homeDirectory}/.bashrc.d/99-nix.sh"
    fi
    export PATH="${config.home.homeDirectory}/.nix-profile/bin:$PATH"
    export XDG_DATA_DIRS="${config.home.homeDirectory}/.nix-profile/share:''${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
    export XDG_CURRENT_DESKTOP=river
    export MOZ_ENABLE_WAYLAND=1
    export NIXOS_OZONE_WL=1
    default_renderer="''${WLR_RENDERER:-gles2}"
    river_bin="${lib.getExe pkgs.river-classic}"
    nixgl_nvidia="$(command -v nixGLNvidia 2>/dev/null || true)"

    if [ -r /proc/driver/nvidia/version ] && [ -x "$nixgl_nvidia" ]; then
      WLR_RENDERER="$default_renderer" "$nixgl_nvidia" "$river_bin" && exit 0
      if [ "$default_renderer" != "pixman" ]; then
        exec env WLR_RENDERER=pixman "$nixgl_nvidia" "$river_bin"
      fi
      exit 1
    fi

    WLR_RENDERER="$default_renderer" "$river_bin" && exit 0
    if [ "$default_renderer" != "pixman" ]; then
      exec env WLR_RENDERER=pixman "$river_bin"
    fi
    exit 1
  '';
  tmpDir = "${config.home.homeDirectory}/MyTemp";
in
{
  imports = (import ./modules/home/default.nix) ++ [
    ./modules/home/services-river.nix
  ];
  programs.home-manager.enable = true;
  programs.chromium.enable = true;
  # dconf D-Bus activation requires a running GNOME/dconf session; on RHEL 9
  # the system-level dconf service is not set up by Home Manager alone (only
  # NixOS does this via programs.dconf.enable).  Disable here to prevent
  # `home-manager switch` from failing outside a desktop session.
  dconf.enable = lib.mkForce false;
  home.sessionVariables.TMPDIR = tmpDir;
  home.packages = [
    riverSession
  ]
  ++ nixglPackage
  ++ bpmnModeler;
  programs.nushell.environmentVariables.TMPDIR = tmpDir;
  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
  home.file.".bashrc.d/99-nix.sh".text = ''
    . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
    export TMPDIR=${tmpDir}
    export SHELL=${config.home.homeDirectory}/.nix-profile/bin/nu
    export XTERM_SHELL=${config.home.homeDirectory}/.nix-profile/bin/nu
  '';
  xdg.dataFile."wayland-sessions/river.desktop".text = ''
    [Desktop Entry]
    Name=River
    Comment=Dynamic tiling Wayland compositor
    Exec=${config.home.homeDirectory}/.nix-profile/bin/river-session
    Type=Application
    DesktopNames=river
  '';

  services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "default";
        profile.outputs = [
          {
            criteria = "*";
            scale = 2.0;
          }
        ];
      }
    ];
  };
}
