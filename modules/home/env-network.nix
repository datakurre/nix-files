{ ... }:
{
  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };

  systemd.user.services.blueman-applet.Service = {
    Restart = "on-failure";
    RestartSec = "2";
  };
  systemd.user.services.network-manager-applet.Service = {
    Restart = "on-failure";
    RestartSec = "2";
  };
}
