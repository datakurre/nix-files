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
  boot.initrd = {
    availableKernelModules = [ "kvm" ];
    luks.devices."luks-05e3771a-f128-4db5-b054-8804bd6b7c14".device =
      "/dev/disk/by-uuid/05e3771a-f128-4db5-b054-8804bd6b7c14";
  };
  hardware.ipu6 = {
    enable = true;
    platform = "ipu6ep";
  };
  environment.systemPackages = [
    pkgs.acpi
    # Calibre
    (pkgs.calibre.override { unrarSupport = true; })
  ];
  networking.hostName = "albemuth";

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
    thinkfan.enable = true;
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
  system.stateVersion = "24.05";

  # Waydroid
  # virtualisation.waydroid.enable = true;
  # virtualisation.waydroid.package = pkgs.waydroid-nftables;

  # Enable the xpadneo driver for Xbox One/Series controllers
  hardware.xpadneo.enable = true;

  # Calibre
  services.udisks2.enable = true;

  # Roblox
  services.flatpak.enable = true;

  # Minecraft Bedrock dedicated server
  services.minecraft-bedrock.servers."crazy-land" = {
    enable = false;
    openFirewall = true;
    serverProperties = {
      server-name = "Crazy Land";
      level-name = "crazy_land";
      online-mode = false;
      enable-lan-visibility = true;
      gamemode = "survival";
      difficulty = "easy";
      force-gamemode = true;
    };
  };

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
