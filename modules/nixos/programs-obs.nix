{ config, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [
    4444
  ];

  # OBS's virtual camera needs v4l2loopback; without it OBS logs "v4l2loopback not
  # installed, virtual camera not registered" at startup and the Start Virtual Camera
  # button is a no-op. exclusive_caps=1 makes the node advertise capture capability
  # only once OBS has opened it for output, which is what Chromium and Firefox require
  # before they will offer it as a webcam.
  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=9 card_label="OBS Virtual Camera" exclusive_caps=1
  '';
}
