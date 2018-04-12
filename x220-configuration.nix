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
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
    ./modules/battery-notifier.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;

  hardware.opengl.driSupport32Bit = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.extraConfig = ''
    [general]
    Enable=Source,Sink,Media,Socket
    Disable=Headset
  '';
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.configFile = ./dotfiles/pulseaudio.conf;

  services.batteryNotifier.enable = true;

  services.mopidy.enable = true;
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
  services.gnome3.gvfs.enable = true;

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
  services.xserver.displayManager.slim.enable = true;
  services.xserver.displayManager.slim.defaultUser = "datakurre";
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
  services.xserver.windowManager.default = "xmonad";
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

  security.pam.enableU2F = true;
  security.pam.services.datakurre.u2fAuth = true;
  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;

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
  ];
  users.users.datakurre.uid = 1000;
  users.users.datakurre.shell = "/run/current-system/sw/bin/zsh";

  home-manager.users.datakurre = import ./home-configuration.nix {
    inherit pkgs; prefix = config.users.users.datakurre.home;
  };

  nix.useSandbox = true;
  nix.sandboxPaths = [ "/dev/urandom" "/etc/ssl/certs/ca-certificates.crt" ];
  nix.binaryCaches = [ https://cache.nixos.org ];
  nix.extraOptions = ''
    auto-optimise-store = false
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ./overlays/custom)
    (import ./overlays/mrvandalo)
  ];

  environment.shellAliases = {
    "vi" = "vim";
  };

  services.nixosManual.showManual = false;

  services.udev.packages = [ pkgs.gnome3.gnome_settings_daemon ];
  services.udev.extraRules = ''
    # Yubico YubiKey
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
    # ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
  '';

  services.tarsnap.enable = true;
  services.tarsnap.archives.data.directories = [
    "/home/datakurre/Asiakirjat"
    "/home/datakurre/Work/collective.flow"
    "/home/datakurre/Work/pwa"
    "/home/datakurre/Work/robotkernel"
    "/var/lib"
  ];

  system.stateVersion = "18.03";
}
