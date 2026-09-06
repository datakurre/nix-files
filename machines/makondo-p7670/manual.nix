{
  config,
  lib,
  pkgs,
  ...
}:
let
  evdev-debounce = pkgs.callPackage ../../pkgs/evdev-debounce { };
  obs-nvenc =
    pkgs.runCommand "obs-nvenc"
      {
        nativeBuildInputs = [ pkgs.makeWrapper ];
      }
      ''
        mkdir -p $out/bin $out/share/applications
        makeWrapper ${pkgs.obs-studio}/bin/obs $out/bin/obs-nvenc \
          --suffix LD_LIBRARY_PATH : /run/opengl-driver/lib \
          --set __NV_PRIME_RENDER_OFFLOAD 1 \
          --set __NV_PRIME_RENDER_OFFLOAD_PROVIDER NVIDIA-G0 \
          --set __GLX_VENDOR_LIBRARY_NAME nvidia \
          --set __VK_LAYER_NV_optimus NVIDIA_only
        cp ${
          pkgs.makeDesktopItem {
            name = "obs-nvenc";
            desktopName = "OBS Studio (NVENC)";
            genericName = "Streaming/Recording Software (NVENC)";
            comment = "Free and open source software for video recording and live streaming (NVIDIA NVENC)";
            exec = "obs-nvenc %U";
            icon = "com.obsproject.Studio";
            categories = [
              "AudioVideo"
              "Recorder"
            ];
            startupNotify = true;
            startupWMClass = "obs";
          }
        }/share/applications/* $out/share/applications/
      '';
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
    obs-nvenc
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
  # Together they give the iGPU working H.264/HEVC QuickSync and VA-API hardware
  # encoders used by default OBS on the iGPU.
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
  # Default OBS is deliberately NOT wrapped with the PRIME render-offload env vars, and
  # therefore renders on the iGPU like everything else. That costs NVENC, and it
  # is not a free choice -- the two requirements are mutually exclusive here:
  #
  #   * obs-nvenc uses cuGraphicsGLRegisterImage to hand OBS's rendered textures
  #     to NVENC, which only works when the GL and CUDA contexts are on the same
  #     GPU. That needs OBS's GL context on the dGPU.
  #   * PipeWire screencast hands OBS DMA-BUFs allocated by river, which composites
  #     on the iGPU. Importing those into an NVIDIA EGL context fails with
  #     glEGLImageTargetTexture2DOES -> GL_INVALID_OPERATION. That needs OBS's GL
  #     context on the iGPU.
  #
  # Capture wins for the default session, because a recorder that cannot see the screen is useless while
  # one that encodes on the iGPU is merely slower. The failure was not subtle:
  # with the wrapper, capturing HEADLESS-1 negotiated BGRx/modifier 0, failed the
  # EGL import, renegotiated to modifier 0xffffffffffffff and RGBx, ran out of
  # options ("no more input formats"), disconnected and retried forever -- so OBS
  # recorded pure black. wl-mirror showed the same output correctly, because it
  # runs on the iGPU; that contrast is what identified the cause.
  #
  # Capturing eDP-1 happened to survive this (it offers a format both GPUs accept),
  # which is why the bug looked like harmless log noise until the headless stage --
  # the entire point of the setup -- was tried.
  #
  # Encoding in default OBS uses QSV/VAAPI on the iGPU via hardware.graphics.extraPackages
  # above; set the OBS encoder accordingly.
  #
  # For workloads where NVENC is desired, the alternative launcher `obs-nvenc`
  # (in environment.systemPackages above) runs OBS with PRIME render-offload and
  # --suffix LD_LIBRARY_PATH : /run/opengl-driver/lib (needed for obs-nvenc-test to find
  # libnvidia-encode.so.1).
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
              # --custom is required, not cosmetic. A wlroots headless output
              # advertises exactly one mode (1280x720); asking for a mode it
              # does not advertise makes kanshi reject the whole profile, and
              # kanshi applies profiles atomically -- so a plain "1920x1080"
              # here silently takes eDP-1's scale down with it, reinstating the
              # tiny-fonts bug from an unexpected direction.
              mode = "--custom 1920x1080@60Hz";
              scale = 1.0;
              position = "1920,0";
            }
          ];
        }
      ];
    };

  };
}
