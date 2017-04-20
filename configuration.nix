# Dell Precision M3800
{ config, pkgs, ... }:

let unstable = import "/nix/var/nix/profiles/per-user/root/channels/nixos-unstable" {
  config = {
    allowUnfree = true;
  };
};

in

{
  imports = [
    ./hardware-configuration.nix
#   ./hydra-configuration.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_4_10;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    cleanTmpDir = true;
  };

  time.timeZone = "Europe/Helsinki";

  networking = {
    hostName = "makondo";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      trustedInterfaces = [ "docker0" "vboxnet0" ];
    };
    vpnc.services = {
      staff = builtins.readFile ./vpnc-staff.conf;
      sysadmin = builtins.readFile ./vpnc-sysadmin.conf;
    };
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "fi";
    defaultLocale = "fi_FI.UTF-8";
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      terminus_font
    ];
  };

  hardware = {
    enableAllFirmware = true;
    bluetooth.enable = true;
    nvidiaOptimus.disable = true;
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
    pulseaudio.configFile = ./pulseaudio.conf;
    opengl.extraPackages = [ pkgs.vaapiIntel ];
  };
  sound.mediaKeys.enable = true;

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "powersave";
  };

  programs = {
    ssh.startAgent = false;
    zsh.enable = true;
  };

  services = {
    logind.extraConfig = ''
        HandlePowerKey=ignore
        HandeSuspendKey=ignore
        HandleHibernateKey=ignore
        HandleLidSwitch=ignore
    '';
    acpid = {
      enable = true;
      powerEventCommands = ''
        systemctl suspend
      '';
      lidEventCommands = ''
        systemctl hibernate
      '';
      acEventCommands = ''
        if [ `cat /sys/class/power_supply/ACAD/online` -eq 0 ]; then
          tee /sys/class/backlight/intel_backlight/brightness <<< 1000
          /run/current-system/sw/bin/cpupower frequency-set -u 1.50GHz
          tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor <<< powersave
        else
          tee /sys/class/backlight/intel_backlight/brightness <<< 3000
          tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor <<< performance
          /run/current-system/sw/bin/cpupower frequency-set -u 3.30GHz
        fi
      '';
      handlers = {
        brightnessDown = {
          event = "video/brightnessdown.*";
          action = ''
            val=`cat /sys/class/backlight/intel_backlight/brightness`
            tee /sys/class/backlight/intel_backlight/brightness <<< `expr $val - 200`
          '';
        };
        brightnessUp = {
          event = "video/brightnessUp.*";
          action = ''
            val=`cat /sys/class/backlight/intel_backlight/brightness`
            tee /sys/class/backlight/intel_backlight/brightness <<< `expr $val + 200`
          '';
        };
      };
    };
    mopidy = {
      enable = true;
      configuration = builtins.readFile ./mopidy.conf;
      extensionPackages = [
        pkgs.mopidy-spotify
        pkgs.mopidy-soundcloud
      ];
    };
    pcscd.enable = true;
    unclutter.enable = true;
    nixosManual.showManual = false;
  };

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  services.redshift = {
    enable = true;
    brightness.day = "1.0";
    brightness.night = "0.7";
    latitude = "62.1435";
    longitude = "25.4449";
  };

  services.locate.enable = true;
  services.memcached.enable = true;

  services.xserver = {
    enable = true;
    enableTCP = false;

    layout = "fi";
    xkbOptions = "eurosign:e,caps:escape";

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "atsoukka";
    displayManager.xserverArgs = [ "-dpi 192" ];
    displayManager.sessionCommands = ''
      xss-lock -- xlock -mode xjack -erasedelay 0 &
      # xss-lock -- xlock -mode xjack -erasedelay 0 -dpmsstandby 60 &
      # https://github.com/NixOS/nixpkgs/commit/5391882ebd781149e213e8817fba6ac3c503740c
      gpg-connect-agent /bye
      GPG_TTY=$(tty)
      export GPG_TTY
    '';

    desktopManager.xterm.enable = false;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";

    videoDrivers = [ "intel" "nouveau" ];

    libinput = {
      enable = true;
      accelProfile = "adaptive";
      accelSpeed = "0.7";
      scrollMethod = "twofinger";
      tapping = false;
      tappingDragLock = false;
      naturalScrolling = true;
      disableWhileTyping = true;
    };

    config = ''
      Section "InputClass"
        Identifier   "Logitech USB Trackball"
        MatchProduct "Logitech USB Trackball"
        Driver "evdev"
        Option "EmulateWheel"       "true"
        Option "EmulateWheelButton" "3"
        Option "XAxisMapping"       "6 7"
        Option "YAxisMapping"       "4 5"
        Option "DeviceAccelProfile" "3"
        Option "AccelerationProfile"     "2"
        Option "AdaptiveDeceleration"    "2"
        Option "AccelerationNumerator"   "2"
        Option "AccelerationDenominator" "1"
        Option "AccelerationThreshold"   "4"
      EndSection
    '';

    exportConfiguration = true;
  };

  environment.systemPackages = with pkgs; [
    unstable.chromium
    unstable.firefox
    unstable.vokoscreen

    git
    gnumake
    irssi
    mercurial
    nodejs
    pythonFull
    vim
    vpnc
    acpi
    htop
    psmisc

    gettext
    pythonPackages.docker_compose
    vagrant

    unstable.pypi2nix
    unstable.npm2nix

    xlockmore
    xorg.xbacklight
    xss-lock
    haskellPackages.xmonad
    networkmanager_vpnc

    (unstable.idea.pycharm-professional.override {
      jdk = oraclejdk8;
    })

    ncmpcpp

    gnupg
    pass
    pythonPackages.xkcdpass

    isync
    msmtp
    notmuch
    afew
    pythonPackages.alot
    w3m
  ];

  security = {
    pam.enableU2F = true;
    pam.services.atsoukka.u2fAuth = true;
    sudo.enable = true;
    sudo.wheelNeedsPassword = true;
  };

  services.udev.extraRules = ''
    # Yubico YubiKey
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
    ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
  '';

  users.users.atsoukka = {
    isNormalUser = true;
    description = "Asko Soukka";
    home = "/home/atsoukka";
    extraGroups = [
      "wheel"
      "audio"
      "video"
      "networkmanager"
      "input"
      "vboxusers"
      "docker"
    ];
    uid = 1000;
    shell = "/run/current-system/sw/bin/zsh";
  };

  nixpkgs.config.packageOverrides = pkgs: rec {
    hydra = pkgs.hydra.overrideDerivation(old: {
      patches = old.patches ++ [
        (pkgs.fetchurl {
          url = "https://patch-diff.githubusercontent.com/raw/NixOS/hydra/pull/277.patch";
          sha256 = "0k9ms3p2sjy5kyrkajbdbhvkd4sql3lhsv2x0xhqssysbk773256";
        })
      ];
    });
    afew = pkgs.pythonPackages.afew.overrideDerivation(args: {
      postPatch = ''
        sed -i "s|'notmuch', 'new'|'test', '1'|g" afew/MailMover.py
      '';
    });
  };

  nixpkgs.config.allowUnfree = true;

  nix.extraOptions = ''
    auto-optimise-store = true
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';

  system.stateVersion = "17.03";
  system.autoUpgrade.enable = true;
}
