# Dell Precision M3800
{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix ];

  boot = {
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/sda";
    cleanTmpDir = true;
  };

  time.timeZone = "Europe/Helsinki";

  networking = {
    hostName = "makondo";
    networkmanager.enable = true;
    interfaceMonitor = {
      enable = true;
      commands = ''
        systemctl restart network-manager
      '';
    };
    firewall = {
      enable = true;
      trustedInterfaces = [ "docker0" ];
    };
    vpnc.services = {
      staff = builtins.readFile ./staff.conf;
      sysadmin = builtins.readFile ./sysadmin.conf;
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
    bluetooth.enable = false;
    pulseaudio.enable = true;
    pulseaudio.configFile = ./pulse/default.pa;
    nvidiaOptimus.disable = true;
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "powersave";
  };

  programs = {
    ssh.startAgent = false;
    zsh.enable = true;
  };

  services = {
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
      brightnessDownEventCommands = ''
        val=`cat /sys/class/backlight/intel_backlight/brightness`
        tee /sys/class/backlight/intel_backlight/brightness <<< `expr $val - 200`
      '';
      brightnessUpEventCommands = ''
        val=`cat /sys/class/backlight/intel_backlight/brightness`
        tee /sys/class/backlight/intel_backlight/brightness <<< `expr $val + 200`
      '';
    };
    pcscd.enable = true;
    unclutter.enable = true;
    nixosManual.showManual = true;
  };

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  services.redshift = {
    enable = true;
    brightness.day = "0.95";
    brightness.night = "0.7";
    latitude = "62.1435";
    longitude = "25.4449";
  };

  services.locate.enable = true;

  services.xserver = {
    enable = true;
    enableTCP = false;

    layout = "fi";
    xkbOptions = "eurosign:e,esc:nocaps";

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "atsoukka";
    displayManager.xserverArgs = [ "-dpi 192" ];
    displayManager.sessionCommands = ''
      xss-lock -- xlock &
    '';
    displayManager.desktopManagerHandlesLidAndPower = true;

    startGnuPGAgent = true;

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";

    desktopManager.xterm.enable = false;

    videoDrivers = [ "intel" "nouveau" ];
    vaapiDrivers = [ pkgs.vaapiIntel ];

    libinput = {
      enable = true;
      accelSpeed = "0.7";
      tapping = false;
      tappingDragLock = true;
      naturalScrolling = true;
      additionalOptions = ''
        Option "PalmDetection" "on"
        Option "DisableWhileTyping" "on"
      '';
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

  environment.x11Packages = with pkgs; [
    xss-lock
    xlockmore
  ];

  environment.systemPackages = with pkgs; [
    chromium
    firefox
    git
    gnumake
    irssi
    mercurial
    nodejs
    pythonFull
    spotify
    vim
    vpnc

    npm2nix
    pypi2nix

    xorg.xbacklight

    idea.idea-ultimate
    idea.pycharm-professional

    gnupg
    pass
    pythonPackages.xkcdpass

    dmenu
    haskellPackages.xmonad
    networkmanager_vpnc
    nmcli-dmenu

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
    sudo.wheelNeedsPassword = false;
  };

  services.udev.extraRules = ''
    # Yubico YubiKey
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess"
  '';

  users.extraUsers.atsoukka = {
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
    afew = pkgs.pythonPackages.afew.overrideDerivation(args: {
      postPatch = ''
        sed -i "s|'notmuch', 'new'|'notmuch', '--version'|g" afew/MailMover.py
      '';
    });
  };

  nixpkgs.config.allowUnfree = true;

  nix.extraOptions = ''
    auto-optimise-store = true
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';

  system.stateVersion = "16.03";
}
