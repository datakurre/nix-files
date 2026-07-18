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
  home.packages = [
    pkgs.wl-clipboard
    pkgs.wlr-randr
    pkgs.pasystray
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

      export GDK_SCALE=2
      export QT_SCALE_FACTOR=2

      systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP DBUS_SESSION_BUS_ADDRESS GDK_SCALE QT_SCALE_FACTOR
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP DBUS_SESSION_BUS_ADDRESS GDK_SCALE QT_SCALE_FACTOR
      systemctl --user start graphical-session.target
      systemctl --user start gammastep

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
          position = "top";
          height = 36;
          modules-left = [ ];
          modules-center = [ ];
          modules-right = [
            "pulseaudio"
            "battery"
            "tray"
          ];
          battery = {
            format = "{icon}  {capacity}%";
            format-icons = [
              "▁"
              "▂"
              "▃"
              "▅"
              "█"
            ];
            states = {
              warning = 20;
              critical = 10;
            };
          };
          pulseaudio = {
            format = "{volume}% {icon}";
            format-muted = "muted";
            on-click = "pavucontrol";
          };
          tray = {
            icon-size = 24;
            spacing = 8;
          };
        }
      ];
      style = ''
        @define-color base03 #002b36;
        @define-color base02 #073642;
        @define-color base01 #586e75;
        @define-color base00 #657b83;
        @define-color base0 #839496;
        @define-color base1 #93a1a1;
        @define-color base2 #eee8d5;
        @define-color base3 #fdf6e3;
        @define-color yellow #b58900;
        @define-color orange #cb4b16;
        @define-color red #dc322f;
        @define-color magenta #d33682;
        @define-color violet #6c71c4;
        @define-color blue #268bd2;
        @define-color cyan #2aa198;
        @define-color green #859900;

        * {
          border: none;
          border-radius: 0;
          font-family: "DejaVu Sans Mono";
          font-size: 16px;
          min-height: 0;
        }

        window#waybar {
          background: @base03;
          color: @base0;
        }

        #battery {
          color: @base1;
        }
        #battery.warning {
          color: @orange;
        }
        #battery.critical {
          color: @red;
        }

        #pulseaudio {
          color: @base1;
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
