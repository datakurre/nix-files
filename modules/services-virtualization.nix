{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    programs = {
      nushell = {
        environmentVariables.DOCKER_HOST = "\"unix:///var/run/user/${
          builtins.toString config.users.users.${config.user.name}.uid
        }/podman/podman.sock\"";
      };
    };
  };
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
    virtualbox.host.enable = true;
  };
}
