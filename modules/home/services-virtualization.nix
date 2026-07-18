{ config, ... }:
{
  # Rootless podman with a Docker-compatible user API socket.
  #
  # Requires subuid/subgid ranges and setuid newuidmap/newgidmap helpers.
  # On NixOS these are provided system-wide by
  # modules/nixos/services-virtualization.nix. On other distributions the
  # administrator must add e.g. "<user>:100000:65536" to /etc/subuid and
  # /etc/subgid (newuidmap/newgidmap are part of the shadow package).
  services.podman.enable = true;
  systemd.user = {
    services.podman = {
      Unit = {
        Description = "Podman API Service";
        Requires = [ "podman.socket" ];
        After = [ "podman.socket" ];
      };
      Service = {
        ExecStart = "${config.services.podman.package}/bin/podman system service";
        Delegate = true;
        KillMode = "process";
      };
    };
    sockets.podman = {
      Unit.Description = "Podman API Socket";
      Socket = {
        ListenStream = "%t/podman/podman.sock";
        SocketMode = "0660";
      };
      Install.WantedBy = [ "sockets.target" ];
    };
  };
  programs.nushell.environmentVariables.DOCKER_HOST = "unix://$XDG_RUNTIME_DIR/podman/podman.sock";
}
