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
  environment.systemPackages = [
    pkgs.acpi
  ];
  fileSystems."/" = {
    options = [ "noatime" ];
  };
  hardware.graphics.enable = true;
  # hardware.graphics.package defaults to Mesa alone, which ships no VA-API driver for
  # the Alder Lake iGPU. Without these, OBS logs "VAAPI: Failed to initialize display"
  # for H264/HEVC/AV1 and obs-qsv11.so spends ~800ms at every startup discovering that
  # it has no runtime and registering zero encoders. intel-media-driver is the VA-API
  # (iHD) driver; vpl-gpu-rt is the oneVPL runtime that obs-qsv11 needs on Gen12.
  # Together they give the iGPU a working H.264/HEVC encoder, which is the fallback
  # that makes it possible to drop the NVIDIA offload wrapper below if the PipeWire
  # DMA-BUF import failures it causes ever become the bigger problem.
  hardware.graphics.extraPackages = [
    pkgs.intel-media-driver
    pkgs.vpl-gpu-rt
  ];
  hardware.nvidia.open = false;
  hardware.nvidia.powerManagement.enable = true;
  hardware.nvidia.prime.nvidiaBusId = "PCI:1:0:0";
  hardware.nvidia.prime.intelBusId = "PCI:0:2:0";
  hardware.nvidia.prime.offload.enable = true;
  hardware.nvidia.prime.offload.enableOffloadCmd = true;
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
  #
  # HEADLESS-1 is the virtual presentation output created by the headless
  # wlroots backend (modules/nixos/services-river.nix). It must be listed here
  # even though nothing is normally sent to it, for the same reason: kanshi
  # matches a profile against the whole connected output set, so omitting it
  # would stop this profile matching and reintroduce the scale-1 bug above.
  #
  # It sits to the right of the panel's 1920x1200 logical area, at scale 1 and
  # exactly 1920x1080, so an OBS canvas of the same size captures it 1:1 -- no
  # downscaling, and none of the 16:10 letterboxing the panel itself needs.
  #
  #
  # Wrap OBS with the PRIME render-offload env vars so its OpenGL context runs
  # on the NVIDIA GPU (same vars as the generated nvidia-offload command). This
  # is required, not just an optimization: obs-nvenc uses cuGraphicsGLRegisterImage
  # to hand OBS's rendered textures to NVENC directly, which only works when the
  # GL context and the CUDA context are on the same GPU.
  #
  # nixpkgs' obs-studio only patches the RUNPATH of $out/lib/obs-plugins/*.so
  # (see addDriverRunpath in the obs-studio derivation) so obs-nvenc.so itself
  # can dlopen libnvidia-encode.so.1 -- but NVENC support is actually probed by
  # spawning $out/bin/obs-nvenc-test as a subprocess, and that helper binary
  # gets no such RUNPATH, so it fails with "Cannot load libnvidia-encode.so.1"
  # even though the driver is present. Adding the same /run/opengl-driver/lib that
  # addDriverRunpath points at to LD_LIBRARY_PATH fixes the helper too, since it
  # inherits the wrapped obs process's environment.
  #
  # --suffix, not --prefix: this wrapper is the outermost of three (obs-studio's own
  # qtWrapperArgs, then this, then home-manager's wrapOBS), so a prefix would put the
  # driver env ahead of $out/lib and Mesa's libGL for the whole process. A suffix still
  # fixes obs-nvenc-test, which has no other source for libnvidia-encode.so.1.
  home-manager.users.${config.user.name} = {
    services.kanshi = {
      enable = true;
      settings = [
        {
          profile.name = "internal";
          profile.outputs = [
            {
              criteria = "eDP-1";
              scale = 2.0;
              position = "0,0";
            }
            {
              criteria = "HEADLESS-1";
              mode = "1920x1080";
              scale = 1.0;
              position = "1920,0";
            }
          ];
        }
      ];
    };

    programs.obs-studio.package = pkgs.symlinkJoin {
      name = "obs-studio";
      paths = [ pkgs.obs-studio ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/obs \
          --suffix LD_LIBRARY_PATH : /run/opengl-driver/lib \
          --set __NV_PRIME_RENDER_OFFLOAD 1 \
          --set __NV_PRIME_RENDER_OFFLOAD_PROVIDER NVIDIA-G0 \
          --set __GLX_VENDOR_LIBRARY_NAME nvidia \
          --set __VK_LAYER_NV_optimus NVIDIA_only
      '';
    };
  };
}
