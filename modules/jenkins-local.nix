{ config, lib, pkgs, ...}:

with lib;

{
  config.services.jenkins.enable = true;
  config.services.jenkins.listenAddress = "127.0.0.1";
  config.services.jenkins.port = 8800;
  config.services.jenkins.prefix = "/jenkins";
}
