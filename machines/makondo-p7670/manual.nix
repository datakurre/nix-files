{
  config,
  lib,
  pkgs,
  ...
}:
let
  evdev-debounce = pkgs.callPackage ../../pkgs/evdev-debounce { };
in
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

  # Deliberately NO libinput quirks file for the trackball. ModelBouncingKeys reads like
  # "this device bounces, please debounce it" but is the opposite: it means timestamps
  # are untrustworthy, and debounce_plugin_device_added() returns early when it is set,
  # switching libinput's debouncing OFF. Its built-in windows (25ms bounce / 12ms
  # spurious) are also too narrow for this device, so the real work happens below, in
  # an interception-tools filter.
  #
  # A libinput Lua plugin would be the tidier home for this, but plugins are opt-in by
  # the compositor and neither wlroots 0.20 nor river-classic 0.3.17 calls
  # libinput_plugin_system_load_plugins(), so they would never run. interception-tools
  # sits below libinput at the evdev layer and is unaffected by that.
  services = {
    interception-tools = {
      enable = true;
      plugins = [ ]; # the JOB uses absolute paths; the default caps2esc is unwanted
      udevmonConfig = ''
        - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${lib.getExe evdev-debounce} | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
          DEVICE:
            NAME: "Logitech USB Trackball"
      '';
    };
    fstrim.enable = true;
    libinput.touchpad.tapping = false;
    libinput.touchpad.tappingDragLock = false;
    libinput.touchpad.additionalOptions = ''
      Option "SendEventsMode" "disabled-on-external-mouse"
    '';
    xserver.videoDrivers = [ "nvidia" ];
    # Marble trackball button remap. It declares HID button usages 1-5 and uses 1, 2, 4
    # and 5: the two big buttons are usages 1 and 2, the two small ones are 4 and 5.
    #
    # Both small buttons are mapped to the same code so they behave identically, and
    # that code is BTN_TASK: river uses it as the button-scroll trigger, and nothing in
    # GTK or Firefox binds it. That matters because libinput replays the scroll button
    # as a real click whenever it is tapped without rolling the ball -- as BTN_SIDE that
    # replay landed on Firefox's "Back". As BTN_TASK the replay is inert, so the small
    # buttons act only as scroll triggers.
    #
    # Nothing maps to BTN_MIDDLE: no button should act as a middle button. (libinput's
    # own 30-vendor-logitech.quirks also sets AttrEventCode=-BTN_MIDDLE for 046d:c408,
    # because the Marble declares a middle button it does not physically have.)
    udev.extraHwdb = ''
      evdev:name:Logitech USB Trackball:*
       KEYBOARD_KEY_90004=btn_task
       KEYBOARD_KEY_90005=btn_task
    '';
  };
  system.stateVersion = "24.11";

  # The internal panel is eDP-1 (DP-1 exists but is an unconnected external
  # port); a non-matching criteria leaves every output at scale 1, which is
  # what made GTK/Qt clients render tiny. Scaling is owned by kanshi alone --
  # no GDK_SCALE/QT_SCALE_FACTOR/Xft.dpi, or clients scale twice.
  home-manager.users.${config.user.name}.services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "internal";
        profile.outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
          }
        ];
      }
    ];
  };
}
