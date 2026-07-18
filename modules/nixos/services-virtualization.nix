{ config, pkgs, ... }:
{
  users.users.${config.user.name} = {
    extraGroups = [
      "libvirtd"
      "networkmanager"
      "qemu"
      "vboxusers"
    ];
    subUidRanges = [
      {
        startUid = 100000;
        count = 65536;
      }
    ];
    subGidRanges = [
      {
        startGid = 100000;
        count = 65536;
      }
    ];
  };
  networking.firewall.trustedInterfaces = [
    "podman0"
    "vboxnet0"
    "virbr0"
  ];
  virtualisation = {
    libvirtd.enable = true;
    # Rootless podman support (subuid helpers, newuidmap wrappers). The
    # user session API socket is managed by modules/home/services-virtualization.nix.
    podman.enable = true;
    virtualbox.host.enable = true;
  };
}
