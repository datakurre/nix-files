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
#   ./modules/jenkins-local.nix
    ./cachix.nix
  ];

  networking.hosts = {
    "127.0.0.1" = [ "localhost" "keycloak" ];
    "127.0.1.1" = [ "shangri-la" ];
    "::1" = [ "localhost" "keycloak" ];
  };

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = [ "kvm-intel" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;

  hardware.opengl.enable = true;

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

  services.logind.extraConfig = ''
    RuntimeDirectorySize=7.8G
  '';

  services.batteryNotifier.enable = true;

  services.mopidy.enable = false;
  services.mopidy.extensionPackages = with pkgs; [
    mopidy-spotify
    mopidy-soundcloud
  ];
  services.mopidy.configuration = builtins.readFile ./private/mopidy.conf;

  time.timeZone = "Europe/Helsinki";

  networking.hostName = "shangri-la";
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;
  networking.firewall.trustedInterfaces = [
    "docker0"
    "vboxnet0"
  ];

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
  virtualisation.docker.extraOptions = "--experimental";
# virtualisation.docker.storageDriver = "btrfs";
  virtualisation.virtualbox.host.enable = true;
  virtualisation.libvirtd.enable = true;

  services.gnome3.at-spi2-core.enable = true;
# services.gnome3.gvfs.enable = true;
  services.gvfs.enable = true;

  programs.fuse.userAllowOther = true;

  environment.systemPackages = [
    pkgs.gnome3.nautilus
    pkgs.gnome3.sushi
    pkgs.xorg.xmodmap
    pkgs.vim
  ];

  services.dbus.packages = [ pkgs.gnome3.sushi ];

  programs.gnupg.agent.enableSSHSupport = true;
  programs.gnupg.agent.enable = true;
  programs.ssh.startAgent = false;
  programs.zsh.enable = true;
  services.pcscd.enable = true;

  services.xserver.enable = true;
  services.xserver.enableTCP = false;
  services.xserver.layout = "fi";
  services.xserver.xkbOptions = "eurosign:e,caps:escape,nbsp:none";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeters.mini.enable = true;
  services.xserver.displayManager.lightdm.greeters.mini.user = "datakurre";
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.sessionCommands = with pkgs; with lib;''
    # Nautilus
    export XDG_DATA_DIRS=$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}${mimeAppsList}/share
    export NAUTILUS_EXTENSION_DIR=${config.system.path}/lib/nautilus/extensions-3.0/
    ${pkgs.xdg-user-dirs}/bin/xdg-user-dirs-update
    # Next / Previous
    xmodmap -e 'keycode 166=Prior'
    xmodmap -e 'keycode 167=Next'
  '';

  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.updateDbusEnvironment = true;
  services.xserver.windowManager.xmonad.enable = true;

  services.xserver.inputClassSections = [''
     Identifier      "Trackpoint Wheel Emulation"
     Driver          "evdev"
     MatchProduct    "TPPS/2 IBM TrackPoint"
     MatchDevicePath "/dev/input/event*"
     Option          "EmulateWheel"       "true"
     Option          "EmulateWheelButton" "2"
     Option          "Emulate3Buttons"    "false"
     Option          "XAxisMapping"       "6 7"
     Option          "YAxisMapping"       "4 5"
   ''];

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
  security.pam.services.datakurre.u2fAuth = true;
  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;

  users.users.docker.isNormalUser = false;
  users.users.docker.uid = 65;

  users.users.datakurre.isNormalUser = true;
  users.users.datakurre.description = "Asko Soukka";
  users.users.datakurre.home = "/home/datakurre";
  users.users.datakurre.extraGroups = [
    "wheel"
    "audio"
    "video"
    "networkmanager"
    "input"
    "vboxusers"
    "docker"
    "qemu"
  ];
  users.users.datakurre.uid = 1000;
  users.users.datakurre.shell = "/run/current-system/sw/bin/zsh";

  home-manager.users.datakurre = import ./home-configuration.nix {
    inherit pkgs; prefix = config.users.users.datakurre.home;
  };

  nix.buildMachines = [{
    hostName = "hydra01.kopla.jyu.fi";
    system = "x86_64-linux";
    maxJobs = 8;
    speedFactor = 2;
    supportedFeatures = [ "kvm" "big-parallel" ];
    mandatoryFeatures = [];
  }{
    hostName = "orion.psy.jyu.fi";
    system = "x86_64-linux";
    maxJobs = 16;
    speedFactor = 4;
    supportedFeatures = [ "kvm" "big-parallel" ];
    mandatoryFeatures = [];
  }];
  nix.distributedBuilds = true;

  nix.useSandbox = true;
  nix.sandboxPaths = [ "/dev/urandom" "/etc/ssl/certs/ca-certificates.crt" ];
  nix.binaryCaches = [ https://cache.nixos.org ];
  nix.extraOptions = ''
    auto-optimise-store = false
    builders-use-substitutes = true
    keep-outputs = true
    keep-derivations = true
  '';
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ./overlays/custom)
#   (import ./overlays/mrvandalo)
  ];

  environment.shellAliases = {
    "vi" = "vim";
  };

  services.nixosManual.showManual = false;

  services.udev.packages = [ pkgs.gnome3.gnome_settings_daemon ];
  services.udev.extraRules = ''
    # Yubico YubiKey
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
    # ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", ENV{DEVTYPE}=="usb_device", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
  '';

  services.tarsnap.enable = false;
  services.tarsnap.archives.data.directories = [
  ];

  system.stateVersion = "20.03";
}
