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

in

{
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.03.tar.gz}/nixos"
    ./modules/battery-notifier.nix
  ];

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  boot.extraModprobeConfig = ''
    # Handle NVIDIA Optimus power management quirk
    options bbswitch load_state=-1 unload_state=1
  '';

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.gfxmodeEfi = "1024x768";

  hardware.bumblebee.enable = true;
  hardware.bumblebee.group = "video";
  hardware.bumblebee.connectDisplay = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];

  hardware.bluetooth.enable = true;
  hardware.bluetooth.config = {
    General = {
      Enable = "Source,Sink,Media,Soket";
      Display = "Headset";
    };
  };
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.configFile = ./dotfiles/pulseaudio.conf;

  services.batteryNotifier.enable = true;

  services.mopidy.enable = false;
  services.mopidy.extensionPackages = with pkgs; [
    mopidy-spotify
    mopidy-soundcloud
  ];
  services.mopidy.configuration = builtins.readFile ./private/mopidy.conf;

  time.timeZone = "Europe/Helsinki";

  networking.hostName = "makondo";
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;
  networking.firewall.trustedInterfaces = [
    "docker0"
    "vboxnet0"
  ];

  networking.hosts = {
    "127.0.0.1" = [ "localhost" "keycloak" "prodwebdp1.certia.fi" "qaswebdp1.certia.fi" ];
    "127.0.1.1" = [ "makondo" ];
    "::1" = [ "localhost" "keycloak" ];
  };

  console.font = "Lat2-Terminus16";
  console.keyMap = "fi";
  i18n.defaultLocale = "fi_FI.UTF-8";

  fonts.enableFontDir = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fonts = with pkgs; [
    bakoma_ttf
    cantarell_fonts
    corefonts
    dejavu_fonts
    gentium
    inconsolata
    liberation_ttf
    powerline-fonts
    terminus_font
    ubuntu_font_family
  ];

  sound.mediaKeys.enable = true;
  powerManagement.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";
  virtualisation.virtualbox.host.enable = true;

  services.gnome3.at-spi2-core.enable = true;
  services.gvfs.enable = true;
  services.btrfs.autoScrub.enable = true;
  services.btrfs.autoScrub.fileSystems = [ "/" "/home" ];

  environment.systemPackages = [
    pkgs.gnome3.nautilus
    pkgs.gnome3.sushi
    pkgs.xlockmore
    pkgs.xss-lock
    pkgs.xorg.xbacklight
    pkgs.vim
  ];

  environment.shellAliases = {
    "vi" = "vim";
  };

  services.dbus.packages = with pkgs; [ gnome3.sushi ];

  programs.fuse.userAllowOther = true;
  programs.gnupg.agent.enableSSHSupport = true;
  programs.gnupg.agent.enable = true;
  programs.ssh.startAgent = false;
  programs.zsh.enable = true;
  services.pcscd.enable = true;

  services.memcached.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
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

  services.xserver.enable = true;
  services.xserver.enableTCP = false;
  services.xserver.layout = "fi";
  services.xserver.xrandrHeads = [ "eDP1" "DP1" ];
  services.xserver.xkbOptions = "eurosign:e,caps:escape,nbsp:none";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeters.mini.enable = true;
  services.xserver.displayManager.lightdm.greeters.mini.user = "atsoukka";
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.xserverArgs = [ "-dpi 224" ];
  services.xserver.displayManager.sessionCommands = with pkgs; with lib;''
    # Nautilus
    export XDG_DATA_DIRS=$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}${mimeAppsList}/share
    export NAUTILUS_EXTENSION_DIR=${config.system.path}/lib/nautilus/extensions-3.0/
    ${pkgs.xdg-user-dirs}/bin/xdg-user-dirs-update
    # XLock
    xss-lock -- xlock -mode xjack -erasedelay 0 &
  '';

  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.updateDbusEnvironment = true;
  services.xserver.videoDrivers = [ "nvidia" "intel" ];
  services.xserver.windowManager.xmonad.enable = true;

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

  security.pam.u2f.enable = true;
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

  home-manager.users.atsoukka = import ./home-configuration.nix {
    inherit pkgs; prefix = config.users.users.atsoukka.home;
  };

  nix.useSandbox = true;
  nix.sandboxPaths = [ "/etc/ssl/certs/ca-certificates.crt" ];
  nix.binaryCaches = [ https://cache.nixos.org ];
  nix.extraOptions = ''
    auto-optimise-store = true
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ./overlays/custom)
  ];

  services.nixosManual.showManual = false;

  services.udev.packages = with pkgs; [ gnome3.gnome_settings_daemon ];
  services.udev.extraRules = ''
    # Yubico YubiKey
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
    ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
  '';

  system.stateVersion = "20.03";
}
