# Dell Precision M3800
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./packages-configuration.nix
    ./private-configuration
  ];

  boot.kernelPackages = pkgs.linuxPackages_4_11;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  hardware.bluetooth.enable = true;
  hardware.bumblebee.enable = true;
  hardware.bumblebee.connectDisplay = true;

  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
  hardware.pulseaudio.configFile = ./pulseaudio.conf;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  time.timeZone = "Europe/Helsinki";

  networking.hostName = "makondo";
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;
  networking.firewall.trustedInterfaces = [
    "docker0"
    "vboxnet0"
    "br-0875c46de2b1"
  ];

  i18n.consoleFont = "Lat2-Terminus16";
  i18n.consoleKeyMap = "fi";
  i18n.defaultLocale = "fi_FI.UTF-8";

  fonts.enableFontDir = true;
  fonts.enableCoreFonts = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fonts = with pkgs; [
    bakoma_ttf
    cantarell_fonts
    corefonts
    dejavu_fonts
    gentium
    inconsolata
    liberation_ttf
    terminus_font
    ubuntu_font_family
  ];

  sound.mediaKeys.enable = true;

  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = "powersave";

  programs.ssh.startAgent = false;
  programs.chromium.enable = true;
  programs.zsh.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandeSuspendKey=ignore
    HandleHibernateKey=ignore
    HandleLidSwitch=ignore
  '';
  services.acpid.enable = true;
  services.acpid.powerEventCommands = ''
    systemctl suspend
  '';
  services.acpid.lidEventCommands = ''
    systemctl hibernate
  '';
  services.acpid.acEventCommands = ''
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
  services.acpid.handlers.brightnessDown.event = "video/brightnessdown.*";
  services.acpid.handlers.brightnessDown.action = ''
    val=`cat /sys/class/backlight/intel_backlight/brightness`
    tee /sys/class/backlight/intel_backlight/brightness <<< `expr $val - 200`
  '';
  services.acpid.handlers.brightnessUp.event = "video/brightnessUp.*";
  services.acpid.handlers.brightnessUp.action = ''
    val=`cat /sys/class/backlight/intel_backlight/brightness`
    tee /sys/class/backlight/intel_backlight/brightness <<< `expr $val + 200`
  '';
  services.mopidy.enable = true;
  services.mopidy.extensionPackages = with pkgs; [
    (mopidy-spotify.overrideDerivation(args: {
      src = pkgs.fetchurl {
        url = "https://github.com/mopidy/mopidy-spotify/archive/feature/oauth.tar.gz";
        sha256 = "0kd7x7ilgw3vjxafk5hcdvap8l7is05zsqjr80f21v9z2wymva74";
      };
    }))
    mopidy-soundcloud
  ];
  services.nixosManual.showManual = false;
  services.pcscd.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  services.redshift.enable = true;
  services.redshift.brightness.day = "1.0";
  services.redshift.brightness.night = "0.7";
  services.redshift.latitude = "62.1435";
  services.redshift.longitude = "25.4449";

  services.locate.enable = true;
  services.memcached.enable = true;

  services.gnome3.at-spi2-core.enable = true;

  services.xserver.enable = true;
  services.xserver.enableTCP = false;
  services.xserver.layout = "fi";
  services.xserver.xkbOptions = "eurosign:e,caps:escape";
  services.xserver.displayManager.slim.enable = true;
  services.xserver.displayManager.slim.defaultUser = "atsoukka";
  services.xserver.displayManager.xserverArgs = [ "-dpi 192" ];
  services.xserver.displayManager.sessionCommands = ''
    # XLock
    xss-lock -- xlock -mode xjack -erasedelay 0 &
    # Tray
    trayer --edge top --align right --SetDockType true --SetPartialStrut true --height 64 --widthtype pixel --width 100 --expand false &
    nm-applet &
    blueman-applet &
    # GPG
    # https://github.com/NixOS/nixpkgs/commit/5391882ebd781149e213e8817fba6ac3c503740c
    gpg-connect-agent /bye
    GPG_TTY=$(tty)
    export GPG_TTY
    # HiDPI
    GDK_SCALE=2
    CLUTTER_SCALE=2
    '';
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";
  services.xserver.videoDrivers = [ "intel" "vesa" ];

  services.xserver.libinput.enable = true;
  services.xserver.libinput.accelProfile = "adaptive";
  services.xserver.libinput.accelSpeed = "0.7";
  services.xserver.libinput.scrollMethod = "twofinger";
  services.xserver.libinput.tapping = false;
  services.xserver.libinput.tappingDragLock = false;
  services.xserver.libinput.naturalScrolling = true;
  services.xserver.libinput.disableWhileTyping = true;
  services.xserver.libinput.additionalOptions = ''
    Option "SendEventsMode" "disabled-on-external-mouse"
  '';

  services.xserver.config = ''
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

  security.pam.enableU2F = true;
  security.pam.services.atsoukka.u2fAuth = true;
  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;

  users.users.atsoukka.isNormalUser = true;
  users.users.atsoukka.description = "Asko Soukka";
  users.users.atsoukka.home = "/home/atsoukka";
  users.users.atsoukka.extraGroups = [
    "wheel"
    "audio"
    "video"
    "networkmanager"
    "input"
    "vboxusers"
    "docker"
  ];
  users.users.atsoukka.uid = 1000;
  users.users.atsoukka.shell = "/run/current-system/sw/bin/zsh";

  nix.binaryCaches = [ https://cache.nixos.org ];
  nix.extraOptions = ''
    auto-optimise-store = true
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';

  services.udev.extraRules = ''
    # Yubico YubiKey
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
    ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
  '';

  system.stateVersion = "17.03";
}
