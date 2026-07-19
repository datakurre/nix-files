{ pkgs, ... }:
{
  programs = {
    carapace = {
      enable = true;
      enableBashIntegration = true;
      enableNushellIntegration = true;
    };
    nushell = {
      enable = true;
      extraConfig = ''
        $env.config.show_banner = false
      '';
      environmentVariables = {
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        GS_OPTIONS = "-sPAPERSIZE=a4";
      };
    };
    starship = {
      enable = true;
      enableBashIntegration = true;
      enableNushellIntegration = true;
    };
  };
  home.packages = [ pkgs.jq ];
}
