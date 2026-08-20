{
  config,
  pkgs,
  lib,
  ...
}:
let
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
  evdev-debounce = pkgs.callPackage ./pkgs/evdev-debounce { };
  evdev-debounce-window = 10;
  interceptionConfig = pkgs.writeText "udevmon.yaml" ''
    - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${lib.getExe evdev-debounce} ${toString evdev-debounce-window} | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
      DEVICE:
        NAME: "Logitech USB Trackball"
  '';
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
    evdev-debounce
    pkgs.interception-tools
  ];
  systemd.user.services.evdev-debounce = {
    Unit = {
      Description = "Debounce Logitech USB Trackball input events";
      After = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.interception-tools}/bin/udevmon -c ${interceptionConfig}";
      Restart = "on-failure";
      RestartSec = 2;
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
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
