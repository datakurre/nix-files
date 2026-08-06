{
  config,
  pkgs,
  lib,
  osConfig ? null,
  ...
}:

let
  isStandalone = osConfig == null;

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
  dconf = lib.mkIf (!isStandalone) {
    enable = true;
    settings = {
      "org/gnome/desktop/interface".scaling-factor = lib.hm.gvariant.mkUint32 2;
    };
  };

  home.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING = "1";
  }
  // lib.optionalAttrs isStandalone {
    NIXOS_OZONE_WL = "1";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "24";
  };

  home.packages = [
    pkgs.wl-clipboard
    pkgs.wlr-randr
    pkgs.wlopm
    pkgs.pasystray
    layoutRotate
    layoutReset
  ]
  ++ lib.optionals isStandalone [
    pkgs.river-classic
    pkgs.foot
    pkgs.xwayland
    pkgs.adwaita-icon-theme
    pkgs.nemo
    pkgs.nemo-fileroller
    pkgs.networkmanagerapplet
    pkgs.blueman
    pkgs.paprefs
    pkgs.pavucontrol
    pkgs.qpaeq
    pkgs.kanshi
    pkgs.xdg-desktop-portal
    pkgs.xdg-desktop-portal-wlr
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
    configFile."xdg-desktop-portal/portals.conf" = lib.mkIf isStandalone {
      text = ''
        [preferred]
        default=wlr;gtk
      '';
    };
  };

  xdg.configFile."river/init" = {
    executable = true;
    text = ''
      #!/bin/sh

      systemctl --user set-environment MOZ_ENABLE_WAYLAND=1
      systemctl --user set-environment GDK_BACKEND=wayland

      systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_SESSION_TYPE DBUS_SESSION_BUS_ADDRESS MOZ_ENABLE_WAYLAND NIXOS_OZONE_WL GDK_BACKEND XCURSOR_THEME XCURSOR_SIZE
      dbus-update-activation-environment --systemd --all

      # We must *restart* graphical-session.target here, not just *start* it.
      # If the target is already active (e.g., from a display manager), services
      # like kanshi, gammastep, waybar, and applets may have started before 
      # WAYLAND_DISPLAY was exported, causing them to fail or fall back to XWayland.
      # Restarting ensures they all pick up the newly exported Wayland environment.
      systemctl --user restart graphical-session.target

      # gpg-agent is often socket-activated before the import above, leaving
      # pinentry-qt without WAYLAND_DISPLAY -- it then falls back to XWayland,
      # which wlroots does not scale. try-restart is a no-op if it isn't up yet.
      systemctl --user try-restart gpg-agent.service || true

      # Explicitly restart kanshi as a workaround for a startup race condition.
      # When graphical-session.target is restarted, kanshi may start before River 
      # has completely registered its wlr-output-management globals, causing it 
      # to fail or miss the outputs. This ensures it applies HiDPI configurations.
      systemctl --user restart kanshi || true

      riverctl keyboard-layout -options "eurosign:e,caps:escape,nbsp:none" fi
      riverctl focus-follows-cursor disabled
      riverctl set-repeat 25 660
      riverctl xcursor-theme Adwaita 24
      riverctl background-color 0x002b36
      riverctl border-width 1
      riverctl border-color-focused 0x268bd2
      riverctl border-color-unfocused 0x586e75
      riverctl border-color-urgent 0xdc322f
      riverctl hide-cursor when-typing enabled

    ''
    + (
      if config.home.username == "atsoukka" then
        ''
          # BTN_TASK, not BTN_SIDE: hwdb remaps the small left button (see
          # machines/makondo-p7670/manual.nix) so libinput's replayed click is inert
          # instead of triggering Firefox's "Back".
          for dev in $(riverctl list-inputs | grep -iE 'trackball|marble'); do
            riverctl input "$dev" scroll-method button
            riverctl input "$dev" scroll-button BTN_TASK
            riverctl input "$dev" scroll-button-lock disabled
            riverctl input "$dev" middle-emulation disabled
          done
          # Trackball only: mute the touchpad, its trackpoint node, the ELAN
          # touchscreen, the phantom PS/2 mouse and the Ergodox's pointer endpoints.
          for dev in $(riverctl list-inputs | grep -E '^(pointer|touch)-' | grep -viE 'trackball|marble'); do
            riverctl input "$dev" events disabled
          done
        ''
      else
        ''
          for dev in $(riverctl list-inputs | grep -i trackball); do
            riverctl input "$dev" scroll-method button
            riverctl input "$dev" scroll-button BTN_SIDE
            riverctl input "$dev" scroll-button-lock enabled
          done
        ''
    )
    + ''

      riverctl map normal Super+Shift Return spawn foot
      riverctl map normal Super P spawn fuzzel
      riverctl map normal Super+Shift X spawn fuzzel
      riverctl map normal Super C resize vertical -9999
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
      riverctl map normal Super Left send-layout-cmd rivertile "main-ratio -0.01"
      riverctl map normal Super Right send-layout-cmd rivertile "main-ratio +0.01"
      riverctl map normal Super+Shift H send-layout-cmd rivertile "main-ratio -0.05"
      riverctl map normal Super+Shift L send-layout-cmd rivertile "main-ratio +0.05"
      riverctl map normal Super+Shift Left send-layout-cmd rivertile "main-ratio -0.05"
      riverctl map normal Super+Shift Right send-layout-cmd rivertile "main-ratio +0.05"
      riverctl map normal Super+Shift A send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super+Shift Z send-layout-cmd rivertile "main-count -1"
      riverctl map normal Super Comma send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super Period send-layout-cmd rivertile "main-count -1"
      riverctl map normal Super Up send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super Down send-layout-cmd rivertile "main-count -1"
      riverctl map normal Super+Shift Up send-layout-cmd rivertile "main-count +1"
      riverctl map normal Super+Shift Down send-layout-cmd rivertile "main-count -1"
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
      riverctl map normal None XF86AudioPlay                   spawn "playerctl play-pause"
      riverctl map normal None XF86AudioPause                  spawn "playerctl play-pause"
      riverctl map normal None XF86AudioNext                   spawn "playerctl next"
      riverctl map normal None XF86AudioPrev                   spawn "playerctl previous"
      riverctl map normal None XF86Favorites                   spawn swaylock
      riverctl map normal None Cancel                          spawn "systemctl suspend"

      riverctl map-pointer normal Super BTN_LEFT move-view
      riverctl map-pointer normal Super BTN_RIGHT resize-view

      riverctl default-layout rivertile
      rivertile -main-location left -main-count 1 -main-ratio 0.5 -view-padding 0 -outer-padding 0 &

      pasystray &
    '';
  };

  programs = {
    foot = {
      enable = true;
      settings = {
        main = {
          font = "DejaVu Sans Mono for Powerline:size=9";
          dpi-aware = "yes";
          term = "foot";
        };
        scrollback.lines = 1024;
        scrollback.multiplier = 9.0;
        cursor.style = "block";
        colors-dark = {
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
        effect = "xjack";
        xjack-opacity = "0.85";
        xjack-font = "DejaVu Sans Mono";
        xjack-speed = "0.8";
        # Original colors:
        xjack-color = "e0e0e0";
        xjack-bg-color = "000000";
        # Matrix green theme:
        # xjack-color = "00ff00";
        # xjack-bg-color = "000000";
        # Weighted word slot example (| separates multiple slot defs):
        xjack-source = "All {verb} and no play makes Jack a dull boy.  ";
        xjack-word = "verb:work=0.8,plone=0.1,nix=0.1";
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
      systemd.enable = true;
      settings = [
        {
          layer = "top";
          position = "right";
          exclusive = false;
          passthrough = false;
          height = 22;
          width = 22;
          modules-left = [ ];
          modules-center = [ ];
          modules-right = [ "tray" ];
          tray = {
            icon-size = 18;
            spacing = 6;
          };
        }
      ];
      style = ''
        @define-color base03 #002b36;

        * {
          border: none;
          border-radius: 0;
          font-family: "DejaVu Sans Mono";
          font-size: 12px;
          min-height: 0;
        }

        window#waybar {
          background: transparent;
        }

        #tray {
          background: @base03;
          margin: 4px 4px 0 0;
          padding: 2px 6px;
          border-radius: 6px;
        }
      '';
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 600;
        command = "${pkgs.procps}/bin/pgrep -x swaylock || ${pkgs.swaylock}/bin/swaylock -f";
      }
      {
        timeout = 1200;
        command = "${pkgs.wlopm}/bin/wlopm --off '*'";
        resumeCommand = "${pkgs.wlopm}/bin/wlopm --on '*'";
      }
    ];
    events = {
      "before-sleep" = "${pkgs.procps}/bin/pgrep -x swaylock || ${pkgs.swaylock}/bin/swaylock -f";
      "lock" = "${pkgs.procps}/bin/pgrep -x swaylock || ${pkgs.swaylock}/bin/swaylock -f";
      "unlock" = "${pkgs.wlopm}/bin/wlopm --on '*'";
      "after-resume" = "${pkgs.wlopm}/bin/wlopm --on '*'";
    };
  };

  # Prevent rapid crash loops (start-limit-hit) when River is still initializing
  systemd.user.services.waybar.Service.RestartSec = "2";
  systemd.user.services.swayidle.Service.RestartSec = "2";

  systemd.user.services.kanshi = lib.mkIf config.services.kanshi.enable {
    Service.RestartSec = "2";
  };
}
