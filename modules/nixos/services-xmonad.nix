{
  config,
  pkgs,
  lib,
  ...
}:
{
  environment.systemPackages = [
    pkgs.blueman
    pkgs.brightnessctl
    pkgs.cbatticon
    pkgs.adwaita-icon-theme
    pkgs.nautilus
    pkgs.sushi
    pkgs.networkmanagerapplet
    pkgs.paprefs
    pkgs.pasystray
    pkgs.pavucontrol
    pkgs.qpaeq
    pkgs.xlockmore
  ];
  programs.dconf.enable = true;
  services = {
    blueman.enable = true;
    dbus.packages = [ pkgs.sushi ];
    displayManager.defaultSession = "none+xmonad";
    gnome.at-spi2-core.enable = true;
    gvfs.enable = true;
    udev.packages = [ pkgs.gnome-settings-daemon ];
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
