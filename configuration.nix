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
    pulseaudio.enable = true;
    pulseaudio.configFile = ./pulse/default.pa;
    nvidiaOptimus.disable = true;
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };

  programs = {
    ssh.startAgent = false;
    zsh.enable = true;
  };

  services = {
    acpid.enable = true;
    pcscd.enable = true;
    unclutter.enable = true;
    nixosManual.showManual = true;
  };

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  services.xserver = {
    enable = true;

    layout = "fi";
    xkbOptions = "eurosign:e";

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "atsoukka";
    displayManager.xserverArgs = [ "-dpi 192" ];
    displayManager.sessionCommands = ''
      xscreensaver -no-splash &
      xss-lock -- xscreensaver-command -lock &
    '';

    startGnuPGAgent = true;

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";

    desktopManager.xterm.enable = false;
    desktopManager.xfce.enable = true;
    desktopManager.default = "xfce";

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
    xscreensaver
    xss-lock
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
    tmux
    vim
    vpnc

    xorg.xbacklight

    idea.idea-ultimate

    gnupg
    pass
    pythonPackages.xkcdpass

    dmenu
    haskellPackages.xmonad

    isync
    msmtp
    notmuch
    pythonPackages.afew
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
    pam_u2f = pkgs.pam_u2f.overrideDerivation (args: rec {
      name = "pam_u2f-${version}";
      version = "1.0.4";
       src = pkgs.fetchurl {
        url = "https://developers.yubico.com/pam-u2f/Releases/${name}.tar.gz";
        sha256 = "189j0wgx6fs146vfp88djqpl1flpfb3962l1a2marlp6d12jwm3i";
      };
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
