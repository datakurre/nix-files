# Dell Precision M3800
{ config, pkgs, ... }:

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

  unstable = import "/nix/var/nix/profiles/per-user/root/channels/nixos-unstable" {
    config = {
      allowUnfree = true;
    };
  };

in

{
  imports = [
    ./hardware-configuration.nix
    ./packages-configuration.nix
    ./private-configuration
    ./mopidy.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_4_14;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  hardware.bluetooth.enable = false;
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
  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;
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
  services.mopidyCustom.enable = true;
  services.mopidyCustom.extensionPackages = with pkgs; [
    mopidy-spotify
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
  services.gnome3.gvfs.enable = true;
  services.udisks2.enable = true;

  environment.systemPackages = [
    unstable.gnome3.nautilus
    unstable.gnome3.sushi
  ];

  services.dbus.packages = [ unstable.gnome3.sushi ];

  services.xserver.enable = true;
  services.xserver.enableTCP = false;
  services.xserver.layout = "fi";
  services.xserver.xrandrHeads = [ "eDP1" "DP1" ];
  services.xserver.xkbOptions = "eurosign:e,caps:escape";
  services.xserver.displayManager.slim.enable = true;
  services.xserver.displayManager.slim.defaultUser = "atsoukka";
  services.xserver.displayManager.xserverArgs = [ "-dpi 192" ];
  services.xserver.displayManager.sessionCommands = with pkgs; with lib;''
    # HiDPI
    export GDK_SCALE=2
    export CLUTTER_SCALE=2
    # Nautilus
    export XDG_DATA_DIRS=$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}${mimeAppsList}/share
    export NAUTILUS_EXTENSION_DIR=${config.system.path}/lib/nautilus/extensions-3.0/
    ${pkgs.xdg-user-dirs}/bin/xdg-user-dirs-update
    # XLock
    xss-lock -- xlock -mode xjack -erasedelay 0 &
    # Tray
    trayer --edge top --align right --SetDockType true --SetPartialStrut true --height 64 --widthtype pixel --width 64 --expand false &
    nm-applet &
    rfkill block bluetooth &
    # xrandr
    # xrandr --output eDP1 --auto --output DP1 --auto --scale 2x2 --right-of eDP1
    xrandr --output eDP1 --auto --output DP1 --auto --panning 3840x2160+3840+0 --scale 2x2 --right-of eDP1
  '';
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.updateDbusEnvironment = true;
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

  nix.package = pkgs.nixUnstable;
  nix.useSandbox = true;
  nix.sandboxPaths = [ "/etc/ssl/certs/ca-certificates.crt" ];
  nix.binaryCaches = [ https://cache.nixos.org ];
  nix.extraOptions = ''
    auto-optimise-store = true
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';

  services.udev.packages = [ pkgs.gnome3.gnome_settings_daemon ];
  services.udev.extraRules = ''
    # Yubico YubiKey
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
    ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
  '';

  system.stateVersion = "17.09";
}
