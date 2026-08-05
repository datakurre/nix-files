{ ... }:
{
  services.gammastep = {
    enable = true;
    latitude = "62.1435";
    longitude = "25.4449";
    temperature.day = 5500;
    temperature.night = 3700;
    settings.general = {
      brightness-day = "1.0";
      brightness-night = "0.7";
    };
  };
}
