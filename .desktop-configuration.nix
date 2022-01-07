{ config, ... }: {
  imports = [ ./desktop-configuration.nix ./default-configuration.nix ];

  config.default.name = "datakurre";
  config.default.description = "Asko Soukka";
  config.default.home = "/home/datakurre";
}
