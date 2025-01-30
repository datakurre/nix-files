{ config, pkgs, ... }:
{
  boot.initrd.availableKernelModules = [
    "sd_mod"
    "usb_storage"
  ];
  boot.kernelPackages = pkgs.linuxPackages_6_12;
  boot.kernelParams = [
    # https://wiki.archlinux.org/index.php/Intel_graphics#Screen_flickering
    "i915.enable_psr=0"
    "kvm.enable_virt_at_load=0"
  ];
  boot.initrd.luks.fido2Support = true;
  boot.initrd.luks.devices = {
    "nixos-enc" = {
      device = "/dev/nvme0n1p1";
      preLVM = true; # You may want to set this to false if you need to start a network service first
      fido2.passwordLess = true;
      fido2.credentials = [
        "6fdec38a63b41c5bce4dd9d90f23ac8b"
        "4135d7bbe683c29f970578b5d2821d8117fd133cab1f4daaf894fd5eb6c005f9d05214ea1b54c0882d3a1a706daf217d"
        "58fab4e9b011003b8156d404ed8c2fac0684a7510ba51fcb35f77b35c603fbd2f5b0e5774e246bfe2ff9b90bd751cce1"
        "44710ee125c0bceb11b13b67a059ca49badf7c01bb00ff6ab797031163cfa4795b9db339735ad4861fa11c3438aaa6c1"
      ];
      bypassWorkqueues = true;
    };
  };
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };
  boot.tmp.cleanOnBoot = true;
  environment.systemPackages =
    let
      nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
        export __NV_PRIME_RENDER_OFFLOAD=1
        export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
        export __GLX_VENDOR_LIBRARY_NAME=nvidia
        export __VK_LAYER_NV_optimus=NVIDIA_only
        exec "$@"
      '';
    in
    [
      pkgs.acpi
      nvidia-offload
    ];
  fileSystems."/" = {
    options = [ "noatime" ];
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  hardware.graphics.enable = true;
  hardware.nvidia.open = false;
  hardware.nvidia.powerManagement.enable = true;
  hardware.nvidia.prime.nvidiaBusId = "PCI:1:0:0";
  hardware.nvidia.prime.intelBusId = "PCI:0:2:0";
  hardware.nvidia.prime.offload.enable = true;
  home-manager.users.${config.user.name} = {
    services.stalonetray.config.icon_size = 48;
    xresources.properties."Xcursor.size" = "64";
  };
  networking.hostName = "makondo";
  services = {
    fstrim.enable = true;
    fwupd.enable = true;
    libinput.touchpad.tapping = false;
    libinput.touchpad.tappingDragLock = false;
    libinput.touchpad.additionalOptions = ''
      Option "SendEventsMode" "disabled-on-external-mouse"
    '';
    xserver.displayManager.xserverArgs = [ "-dpi 224" ];
    xserver.videoDrivers = [ "nvidia" ];
  };
  system.stateVersion = "24.11";
}
