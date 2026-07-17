{ config, lib, ... }:
let
  uid = if lib.hasAttr "users" config
    then builtins.toString config.users.users.${config.home.username}.uid
    else "1000";
in
{
  programs.nushell.environmentVariables.DOCKER_HOST = "\"unix:///var/run/user/${uid}/podman/podman.sock\"";
}
