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
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-22.05.tar.gz}/nixos"
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
      "virbr0"
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

    services.avahi.enable = true;
    services.avahi.allowPointToPoint = true;
    services.avahi.publish.enable = true;
    services.avahi.publish.userServices = true;

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
      Identifier   "Marble Mouse"
      MatchProduct "Logitech USB Trackball"
      MatchIsPointer "on"
      MatchDevicePath "/dev/input/event*"
      Driver "evdev"
      Option "SendCoreEvents"          "true"
      Option "ButtonMapping"           "1 9 3 4 5 6 7 2"
      Option "EmulateWheel"            "true"
      Option "EmulateWheelButton"      "8"
      Option "YAxisMapping"            "4 5"
      Option "XAxisMapping"            "6 7"
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
    security.pki.certificates = [
      # FinEID
      ''
-----BEGIN CERTIFICATE-----
MIIGDjCCA/agAwIBAgIDAw1AMA0GCSqGSIb3DQEBDQUAMIGWMQswCQYDVQQGEwJG
STEhMB8GA1UECgwYVmFlc3RvcmVraXN0ZXJpa2Vza3VzIENBMSkwJwYDVQQLDCBD
ZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSBTZXJ2aWNlczEZMBcGA1UECwwQVmFybWVu
bmVwYWx2ZWx1dDEeMBwGA1UEAwwVVlJLIEdvdi4gUm9vdCBDQSAtIEcyMB4XDTE3
MTIxNDA4NTAzMVoXDTM4MTIxMzA4NTAzMVowgZYxCzAJBgNVBAYTAkZJMSEwHwYD
VQQKDBhWYWVzdG9yZWtpc3RlcmlrZXNrdXMgQ0ExKTAnBgNVBAsMIENlcnRpZmlj
YXRpb24gQXV0aG9yaXR5IFNlcnZpY2VzMRkwFwYDVQQLDBBWYXJtZW5uZXBhbHZl
bHV0MR4wHAYDVQQDDBVWUksgR292LiBSb290IENBIC0gRzIwggIiMA0GCSqGSIb3
DQEBAQUAA4ICDwAwggIKAoICAQC/1gBKiQ4vIztyf3MgZaBfFsV7XlwG+WZzIIL1
YpYXlFH+mzXo8g5ffyGVHGLA5PmCeFzvVcDH/A1587ZMgjYKsEv8LWGmC4i4T7kF
rgbMCdN7Sg1oiRNFAKOdXOZ+pR7nBi/wa0WkotSbh8qYZWDrWsyileyTW0qldn1f
ddItlUd6abFziKxlJHkgf4iGRWQS6BTHOJCXHPFB97jgN/+2tcwxWswo/4SoU1ZY
ct1jwDtHHYxWQ95UxwjMP3rowgPKNLyFlefD0SDS9Eor8envfXpbtQRgUgR4nejn
KUNuOwEA2CrMBiYCaoQ/8wiqPhT99/eOuYAwQqUFfM3zoYQieBFBCdWMgAtOWI2Y
1HM9FfdtmT3khPNHPC9rmRSEITucVmVS9Y+rDaljgsw5UrHqp1njo8APeT7olT5G
rLnduFeF9pf/nrMI5jdW3vymMziNvw1rlqaL6XBKt2dEqIkukOaXi+5vnKxzRftp
OP1W+AXroxHMyPLyxLD41xn4BuaWYH3U5Lbz1JsZX98xg8644HWWKW08L+hZwEqf
uuz6k/aRby0kFJIrvq2dCFg14WEqE9/Y0HzxVvNrdC3E4+6AYSyrCl1VSUthr5VO
sbdS1pnT7yTQHAZImhvCF5yy5ov9LXKxlzwYSVFWfFXkEr5QiR1pKBlIw9oigang
4AWqvQIDAQABo2MwYTAfBgNVHSMEGDAWgBTRpwgWB57pvU7T1yBTllkGJ9eITTAd
BgNVHQ4EFgQU0acIFgee6b1O09cgU5ZZBifXiE0wDgYDVR0PAQH/BAQDAgEGMA8G
A1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQENBQADggIBAC1Qj8Fm74llE8N41MzM
Wpdv7I9gVN5zZLcN6OE7pazPhbaWOUxEpDtZNwyAQBYzcnRI4IQloxstDQDhM2DC
wV92D7OiS3DFJkDNEPpY9IFTj67cJ0iFlaaizkpCGb+VNSBk30JqZnUNVltLdZY1
U4McUKDlx5Sdy9ayPZNKy5SQcchvb2GbbvHQiOvEbz6DNEBUmEf9TMzKHI2D4DFt
MDWz3yTEjTbdwNT8WYaso/BQvhhKQHhXoI3cDZK1yZZspzldPryuK9pxVj3RJ1Sq
tAZ82MA8bcWd8jxVvvFhDtgc0ah9b9izF0K31RJlJs77lIXGbG1a5W58gD07m84v
o/i98pIiXG4NeggKPlzd0//2F9YlZ8H7hnxUV2pzUr0HpUkF2RGLlUby3GIGiqyB
BFfJuFRGGInEaB8VHpUCWKrEYZ8uD0TbTAGCaJX7Mf/QwgROfUex95nN5Q7CjBcS
RJaCPZGYGpe2Z0Fw0o680WIgdoAS7Q65+Z8miUzXT2upbqXB+rsEE11mR46JqCqx
9l8XFtz9WRJuJ23dvej9xxF98vVWz6p+0P8TIoVi+UfqaO0Pk9hYYcrPdeMUZSfg
En8jHtbtDz69AVvmFCYjXeAER3QlrMGVM6gzYCmdnYZj9dC9LxYRJtOZKY+Clnpc
r/xS7vOO+Qq8VUHSmfQbp31m
-----END CERTIFICATE-----
''
];

    users.users = builtins.listToAttrs [{
      name = cfg.name;
      value = {
        description = cfg.description;
        home = cfg.home;
        isNormalUser = true;
        extraGroups = [
          "adbusers"
          "audio"
          "dialout"
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
        openssh.authorizedKeys.keys = [
          "TsOi8q+54I0W08g1Fsjn+AjXBF3SWyQMP2F/ZaF806LYBrRSUfbM3DWJawUALWfzTBdM6PV4kg3AK47EZDuVC3y21L1DiPsVo2XocFHehewcmCtaue6vZagTeJE8VXwOyKhYJ4gWofEhE49t/vmi1DWBaI1dJceR9vQPbP3wHwb8hBC0biTGc1ZWhhivMSIvvwpghvIT19HXxT0uGiT8+Yr1fwl7RaUmjp95/LuzETYXULVdzq8Lios+23dTbJRDlUblf2WljcglLoEHHdjyyqasc/0c9kwIuCQatBubde6POjoRRhMp30FkYLFhtkaSD6erBudauDatI4PzYvGZY9AzW7hfVeo76GQoKjGgHRL+/Necw7cT8BbHOxmXAYufgzaiAdQAfga7kDtc/OYRQc+fD5baPnDpf29zc/GGh1iOUgik= asko"
        ];
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
      experimental-features = nix-command flakes
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
      pkgs.gnome3.gnome-settings-daemon
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
#   virtualisation.virtualbox.host.enableExtensionPack = true;
    virtualisation.libvirtd.enable = true;
  };
}
