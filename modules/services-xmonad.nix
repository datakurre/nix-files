{ config, pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.blueman
    pkgs.brightnessctl
    pkgs.cbatticon
    pkgs.gnome3.adwaita-icon-theme
    pkgs.gnome3.nautilus
    pkgs.gnome3.sushi
    pkgs.networkmanagerapplet
    pkgs.paprefs
    pkgs.pasystray
    pkgs.pavucontrol
    pkgs.qpaeq
    pkgs.xlockmore
  ];
  programs.dconf.enable = true;
  home-manager.users.${config.user.name} = {
    services = {
      stalonetray = {
        config = {
          decorations = null;
          dockapp_mode = null;
          geometry = "4x1-0+0";
          max_geometry = "5x1-0+0";
          grow_gravity = "SW";
          icon_gravity = "SW";
          kludges = "force_icons_size";
          skip_taskbar = true;
          sticky = true;
        };
        enable = true;
      };
      screen-locker = {
        enable = true;
        lockCmd = "${pkgs.xlockmore}/bin/xlock -mode xjack -erasedelay 0";
      };
    };
    xresources.properties = {
      "Xcursor.theme" = "Adwaita";

      "XTerm*wideChars" = "true";
      "XTerm*locale" = "true";
      "XTerm*utf8" = "true";
      "XTerm*vt100Graphics" = "true";

      "XTerm*selectToClipboard" = "true";
      "XTerm*faceName" = "DejaVu Sans Mono for Powerline";
      "XTerm*faceSize" = "11";
      "XTerm*saveLines" = "1024";

      "*background" = "#002b36";
      "*foreground" = "#839496";
      "*fadeColor" = "#002b36";
      "*cursorColor" = "#93a1a1";
      "*pointerColorBackground" = "#586e75";
      "*pointerColorForeground" = "#93a1a1";

      # black dark/light
      "*color0" = "#073642";
      "*color8" = "#002b36";

      # red dark/light
      "*color1" = "#dc322f";
      "*color9" = "#cb4b16";

      # green dark/light
      "*color2" = "#859900";
      "*color10" = "#586e75";

      # yellow dark/light
      "*color3" = "#b58900";
      "*color11" = "#657b83";

      # blue dark/light
      "*color4" = "#268bd2";
      "*color12" = "#839496";

      # magenta dark/light
      "*color5" = "#d33682";
      "*color13" = "#6c71c4";

      # cyan dark/light
      "*color6" = "#2aa198";
      "*color14" = "#93a1a1";

      # white dark/light
      "*color7" = "#eee8d5";
      "*color15" = "#fdf6e3";
    };
    xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./services-xmonad.hs;
    };
  };
  services = {
    blueman.enable = true;
    dbus.packages = with pkgs; [ gnome3.sushi ];
    displayManager.defaultSession = "none+xmonad";
    gnome.at-spi2-core.enable = true;
    gvfs.enable = true;
    udev.packages = [ pkgs.gnome3.gnome-settings-daemon ];
    xserver = {
      displayManager = {
        lightdm = {
          enable = true;
          greeters.mini = {
            enable = true;
            user = config.user.name;
          };
        };
        sessionCommands =
          let
            # Prioritize nautilus by default when opening directories
            mimeAppsList = pkgs.writeTextFile {
              name = "gnome-mimeapps";
              destination = "/share/applications/mimeapps.list";
              text = ''
                [Default Applications]
                inode/directory=nautilus.desktop;org.gnome.Nautilus.desktop
              '';
            };
          in
          with pkgs;
          with lib;
          ''
            # Nautilus
            export XDG_DATA_DIRS=$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}${mimeAppsList}/share
            export NAUTILUS_EXTENSION_DIR=${config.system.path}/lib/nautilus/extensions-3.0/
            ${pkgs.xdg-user-dirs}/bin/xdg-user-dirs-update
            # XMousePasteBlock
            ${pkgs.xmousepasteblock}/bin/xmousepasteblock &
            # XLock
            xss-lock -- xlock -mode xjack -erasedelay 0 &
            # Tray
            stalonetray &
            nm-applet &
            sleep 0.3 && blueman-applet &
            sleep 0.6 && pasystray &
            sleep 0.9 && cbatticon &
          '';
      };
      enableTCP = false;
      enable = true;
      updateDbusEnvironment = true;
      windowManager.xmonad.enable = true;
      xkb.options = "eurosign:e,caps:escape,nbsp:none";
    };
  };
}
