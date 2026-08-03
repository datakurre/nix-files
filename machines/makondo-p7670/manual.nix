{ config, pkgs, ... }:
{
  boot.initrd.availableKernelModules = [
    "sd_mod"
    "usb_storage"
  ];
  boot.kernelPackages = pkgs.linuxPackages_6_12;
  boot.kernelParams = [
    "i915.enable_psr=0"
    "kvm.enable_virt_at_load=0"
  ];
  boot.initrd.luks.devices = {
    "nixos-enc" = {
      device = "/dev/nvme0n1p1";
      preLVM = true;
      bypassWorkqueues = true;
    };
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
  hardware.graphics.enable = true;
  hardware.nvidia.open = false;
  hardware.nvidia.powerManagement.enable = true;
  hardware.nvidia.prime.nvidiaBusId = "PCI:1:0:0";
  hardware.nvidia.prime.intelBusId = "PCI:0:2:0";
  hardware.nvidia.prime.offload.enable = true;
  networking.hostName = "makondo";
  services = {
    fstrim.enable = true;
    libinput.touchpad.tapping = false;
    libinput.touchpad.tappingDragLock = false;
    libinput.touchpad.additionalOptions = ''
      Option "SendEventsMode" "disabled-on-external-mouse"
    '';
    xserver.videoDrivers = [ "nvidia" ];
  };
  system.stateVersion = "24.11";

  home-manager.users.${config.user.name} = {
    programs.firefox.profiles."default".settings."layout.css.devPixelsPerPx" = "2.0";
    services.kanshi = {
      enable = true;
      settings = [
        {
          profile.name = "default";
          profile.outputs = [
            {
              criteria = "DP-1";
              scale = 2.0;
            }
          ];
        }
      ];
    };
  };
}
