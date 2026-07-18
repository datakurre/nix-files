{
  config,
  pkgs,
  lib,
  ...
}:

with lib;

let
  layoutRotate = pkgs.writeShellScriptBin "river-layout-rotate" ''
    state="$HOME/.cache/river-main-location"
    curr=$(cat "$state" 2>/dev/null || echo left)
    case "$curr" in
      left)   next="top" ;;
      top)    next="right" ;;
      right)  next="bottom" ;;
      *)      next="left" ;;
    esac
    mkdir -p "$(dirname "$state")"
    echo "$next" > "$state"
    riverctl send-layout-cmd rivertile "main-location $next"
  '';

  layoutReset = pkgs.writeShellScriptBin "river-layout-reset" ''
    state="$HOME/.cache/river-main-location"
    mkdir -p "$(dirname "$state")"
    echo "left" > "$state"
    riverctl send-layout-cmd rivertile "main-location left"
    riverctl send-layout-cmd rivertile "main-ratio 0.5"
    riverctl send-layout-cmd rivertile "main-count 1"
  '';
in
{
  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface".scaling-factor = lib.hm.gvariant.mkUint32 2;
    };
  };

  xresources.properties."Xft.dpi" = "192";

  home.packages = [
    pkgs.wl-clipboard
    pkgs.wlr-randr
    pkgs.pasystray
    pkgs.xrandr
    pkgs.xrdb
    layoutRotate
    layoutReset
  ];

  xdg = {
    userDirs = {
      enable = true;
      createDirectories = true;
      setSessionVariables = true;
    };
    mimeApps = {
      enable = true;
      defaultApplications."inode/directory" = [
        "nemo.desktop"
      ];
    };
  };

  xdg.configFile."river/init" = {
    executable = true;
    text = ''
      #!/bin/sh

      systemctl --user set-environment GDK_SCALE=2
      systemctl --user set-environment QT_SCALE_FACTOR=2
      systemctl --user set-environment MOZ_ENABLE_WAYLAND=1
      systemctl --user set-environment GDK_BACKEND=wayland

      systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP DBUS_SESSION_BUS_ADDRESS GDK_SCALE QT_SCALE_FACTOR MOZ_ENABLE_WAYLAND GDK_BACKEND
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP DBUS_SESSION_BUS_ADDRESS GDK_SCALE QT_SCALE_FACTOR MOZ_ENABLE_WAYLAND GDK_BACKEND
      systemctl --user start graphical-session.target
      systemctl --user start gammastep

      xrandr --dpi 192
      xrdb -merge ~/.Xresources

      riverctl keyboard-layout -options "eurosign:e,caps:escape,nbsp:none" fi
      riverctl focus-follows-cursor disabled
      riverctl set-repeat 25 660
      riverctl xcursor-theme Adwaita 48
      riverctl background-color 0x002b36
      riverctl border-width 1
      riverctl border-color-focused 0x268bd2
      riverctl border-color-unfocused 0x586e75
      riverctl border-color-urgent 0xdc322f
      riverctl hide-cursor when-typing enabled

      for dev in $(riverctl list-inputs | grep -i trackball); do
        riverctl input "$dev" scroll-method button
        riverctl input "$dev" scroll-button BTN_SIDE
        riverctl input "$dev" scroll-button-lock enabled
      done

      riverctl map normal Super+Shift Return spawn foot
      riverctl map normal Super P spawn fuzzel
      riverctl map normal Super+Shift X spawn fuzzel
      riverctl map normal Super+Shift C close
      riverctl map normal Super Space spawn river-layout-rotate
      riverctl map normal Super+Shift Space spawn river-layout-reset
      riverctl map normal Super J focus-view next
      riverctl map normal Super K focus-view previous
      riverctl map normal Super Tab focus-view next
      riverctl map normal Super Return zoom
      riverctl map normal Super+Shift J swap next
      riverctl map normal Super+Shift K swap previous
      riverctl map normal Super H send-layout-cmd rivertile "main-ratio -0.01"
      riverctl map normal Super L send-layout-cmd rivertile "main-ratio +0.01"
      riverctl map normal Super+Shift H send-layout-cmd rivertile "main-ratio -0.01"
      riverctl map normal Super+Shift L send-layout-cmd rivertile "main-ratio +0.01"
      riverctl map normal Super+Shift A send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super+Shift Z send-layout-cmd rivertile "main-count -1"
      riverctl map normal Super Comma send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super Period send-layout-cmd rivertile "main-count -1"
      riverctl map normal Super T toggle-float
      riverctl map normal Super F toggle-fullscreen
      riverctl map normal Super+Shift F toggle-float
      riverctl map normal Super+Shift Q exit
      riverctl map normal Super W focus-output next
      riverctl map normal Super+Shift W send-to-output next

      for i in $(seq 1 9); do
        riverctl map normal Super "$i"            set-focused-tags "$((1 << (i - 1)))"
        riverctl map normal Super+Shift "$i"       set-view-tags "$((1 << (i - 1)))"
        riverctl map normal Super+Control "$i"        toggle-focused-tags "$((1 << (i - 1)))"
        riverctl map normal Super+Shift+Control "$i"  toggle-view-tags "$((1 << (i - 1)))"
      done

      riverctl map -repeat normal None XF86MonBrightnessUp     spawn "brightnessctl set 5%+"
      riverctl map -repeat normal None XF86MonBrightnessDown   spawn "brightnessctl set 5%-"
      riverctl map -repeat normal None XF86AudioRaiseVolume    spawn "amixer set Master 5%+ unmute"
      riverctl map -repeat normal None XF86AudioLowerVolume    spawn "amixer set Master 5%- unmute"
      riverctl map normal None XF86AudioMute                   spawn "amixer set Master toggle"
      riverctl map normal None XF86AudioMicMute                spawn "amixer set Capture toggle"
      riverctl map normal None XF86Favorites                   spawn swaylock
      riverctl map normal None Cancel                          spawn "systemctl suspend"

      riverctl map-pointer normal Super BTN_LEFT move-view
      riverctl map-pointer normal Super BTN_RIGHT resize-view

      riverctl default-layout rivertile
      rivertile -main-location left -main-count 1 -main-ratio 0.5 -view-padding 0 -outer-padding 0 &

      nm-applet --indicator &
      blueman-applet &
      pasystray &
      waybar &
    '';
  };

  programs = {
    foot = {
      enable = true;
      settings = {
        main = {
          font = "DejaVu Sans Mono for Powerline:size=9";
          dpi-aware = "yes";
        };
        scrollback.lines = 1024;
        cursor.style = "block";
        colors = {
          background = "002b36";
          foreground = "839496";
          regular0 = "073642";
          regular1 = "dc322f";
          regular2 = "859900";
          regular3 = "b58900";
          regular4 = "268bd2";
          regular5 = "d33682";
          regular6 = "2aa198";
          regular7 = "eee8d5";
          bright0 = "002b36";
          bright1 = "cb4b16";
          bright2 = "586e75";
          bright3 = "657b83";
          bright4 = "839496";
          bright5 = "6c71c4";
          bright6 = "93a1a1";
          bright7 = "fdf6e3";
        };
      };
    };

    fuzzel = {
      enable = true;
      settings = {
        main = {
          font = "DejaVu Sans Mono:size=14";
          terminal = "foot";
          lines = 10;
          width = 40;
          horizontal-pad = 16;
          vertical-pad = 8;
          inner-pad = 8;
        };
        border.width = 2;
        colors = {
          background = "002b36ff";
          text = "839496ff";
          match = "cb4b16ff";
          selection = "073642ff";
          selection-text = "93a1a1ff";
          border = "268bd2ff";
        };
      };
    };

    swaylock = {
      enable = true;
      settings = {
        color = "002b36";
        inside-color = "002b36";
        ring-color = "268bd2";
        key-hl-color = "859900";
        text-color = "839496";
        font = "DejaVu Sans Mono";
        indicator-radius = 100;
      };
    };

    waybar = {
      enable = true;
      systemd.enable = false;
      settings = [
        {
          layer = "top";
          position = "top";
          exclusive = false;
          passthrough = false;
          height = 44;
          modules-left = [ ];
          modules-center = [ ];
          modules-right = [ "tray" ];
          tray = {
            icon-size = 36;
            spacing = 12;
          };
        }
      ];
      style = ''
        @define-color base03 #002b36;

        * {
          border: none;
          border-radius: 0;
          font-family: "DejaVu Sans Mono";
          font-size: 9px;
          min-height: 0;
        }

        window#waybar {
          background: transparent;
        }

        #tray {
          background: @base03;
          margin: 8px 8px 0 0;
          padding: 4px 12px;
          border-radius: 12px;
        }
      '';
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 600;
        command = "${pkgs.swaylock}/bin/swaylock";
      }
    ];
    events = {
      "before-sleep" = "${pkgs.swaylock}/bin/swaylock";
      "lock" = "${pkgs.swaylock}/bin/swaylock";
    };
  };
}
