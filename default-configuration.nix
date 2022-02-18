{ config, lib, pkgs, ...}:

let

  cfg = config.default;

in

{
  options = {
    default.name = lib.mkOption {
      default = "atsoukka";
    };
    default.description = lib.mkOption {
      default = "Asko Soukka";
    };
    default.home = lib.mkOption {
      default = "/home/atsoukka";
    };
  };

  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-21.11.tar.gz}/nixos"
    ./modules/battery-notifier.nix
    ./modules/cachix/default.nix
    ./modules/private/default.nix
  ];

  config = {
    networking.networkmanager.enable = true;
    networking.firewall.enable = true;
    networking.firewall.trustedInterfaces = [
      "docker0"
      "vboxnet0"
    ];
    networking.firewall.allowedTCPPorts = [
      4444  # obs-websocket
    ];
    networking.firewall.allowedTCPPortRanges = [
      { from = 5990; to = 5999; }  # vscode
    ];

    console.font = "Lat2-Terminus16";
    console.keyMap = "fi";
    i18n.defaultLocale = "fi_FI.UTF-8";
    time.timeZone = "Europe/Helsinki";
    services.xserver.layout = "fi";

    fonts.fontconfig.enable = true;
    fonts.fontDir.enable = true;
    fonts.enableGhostscriptFonts = true;

    environment.systemPackages = with pkgs; [
      gnome3.nautilus
      gnome3.sushi
      xorg.xmodmap
      vim
      spice-gtk
    ];

    services.dbus.packages = with pkgs; [ gnome3.sushi ];
    services.gnome.at-spi2-core.enable = true;
    services.gvfs.enable = true;
    services.pcscd.enable = true;

    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.support32Bit = true;
    hardware.pulseaudio.package = pkgs.pulseaudioFull;
    hardware.pulseaudio.configFile = ./dotfiles/pulseaudio.conf;

    services.xserver.desktopManager.xterm.enable = false;
    services.xserver.enable = true;
    services.xserver.enableTCP = false;
    services.xserver.updateDbusEnvironment = true;
    services.xserver.windowManager.xmonad.enable = true;
    services.xserver.xkbOptions = "eurosign:e,caps:escape,nbsp:none";
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.greeters.mini.enable = true;
    services.xserver.displayManager.lightdm.greeters.mini.user = cfg.name;
    services.xserver.displayManager.defaultSession = "none+xmonad";
    services.xserver.displayManager.sessionCommands =
    let mimeAppsList = pkgs.writeTextFile {
      name = "gnome-mimeapps";
      destination = "/share/applications/mimeapps.list";
      text = ''
        [Default Applications]
        inode/directory=nautilus.desktop;org.gnome.Nautilus.desktop
      '';
    }; in ''
      # Nautilus
      export XDG_DATA_DIRS=$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}${mimeAppsList}/share
      export NAUTILUS_EXTENSION_DIR=${config.system.path}/lib/nautilus/extensions-3.0/
      ${pkgs.xdg-user-dirs}/bin/xdg-user-dirs-update
      # Next / Previous
      xmodmap -e 'keycode 166=Prior'
      xmodmap -e 'keycode 167=Next'
    '';
    services.xserver.inputClassSections = [''
      Identifier   "Logitech USB Trackball"
      MatchProduct "Logitech USB Trackball"
      Driver "evdev"
      Option "EmulateWheel"            "true"
      Option "EmulateWheelButton"      "8"
      Option "XAxisMapping"            "6 7"
      Option "YAxisMapping"            "4 5"
      Option "DeviceAccelProfile"      "3"
      Option "AccelerationProfile"     "2"
      Option "AdaptiveDeceleration"    "2"
      Option "AccelerationNumerator"   "2"
      Option "AccelerationDenominator" "1"
      Option "AccelerationThreshold"   "4"
    ''];

    programs.adb.enable = true;
    programs.dconf.enable = true;
    programs.fuse.userAllowOther = true;
    programs.gnupg.agent.enable = true;
    programs.gnupg.agent.enableSSHSupport = true;
    programs.ssh.startAgent = false;

    documentation.nixos.enable = false;

    security.pam.u2f.enable = true;
    security.pam.services.datakurre.u2fAuth = true;
    security.sudo.enable = true;
    security.sudo.wheelNeedsPassword = true;

    users.users = builtins.listToAttrs [{
      name = cfg.name;
      value = {
        description = cfg.description;
        home = cfg.home;
        isNormalUser = true;
        extraGroups = [
          "adbusers"
          "audio"
          "docker"
          "input"
          "libvirtd"
          "networkmanager"
          "qemu"
          "vboxusers"
          "video"
          "wheel"
        ];
        uid = 1000;
      };
    }];
    home-manager.users = builtins.listToAttrs [{
      name = cfg.name;
      value = import ./home-configuration.nix { inherit pkgs; user = cfg; };
    }];

    nix.binaryCaches = [ https://cache.nixos.org ];
    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 180d";
    };
    nix.extraOptions = ''
      auto-optimise-store = false
      builders-use-substitutes = true
      keep-outputs = true
      keep-derivations = true
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
    '';
    nix.useSandbox = true;
    nix.sandboxPaths = [ "/dev/urandom" "/etc/ssl/certs/ca-certificates.crt" ];

    nixpkgs.overlays = [
      (import ./overlays/custom)
    ];
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
      "corefonts"
      "Oracle_VM_VirtualBox_Extension_Pack"
    ];
    nixpkgs.config.permittedInsecurePackages = [
       "electron-12.0.7"  # Electron version 12.0.7 is EOL
    ];

    environment.shellAliases = {
      "vi" = "vim";
    };

    services.udev.packages = [
      pkgs.gnome3.gnome_settings_daemon
      pkgs.android-udev-rules
    ];
    services.udev.extraRules = ''
      # Yubico YubiKey
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
      # ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", ENV{DEVTYPE}=="usb_device", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
    '';

    virtualisation.docker.enable = true;
    virtualisation.docker.extraOptions = "--experimental";
    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;
    virtualisation.libvirtd.enable = true;
  };
}
