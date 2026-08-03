{
  pkgs,
  lib,
  ...
}:
{
  home.activation.regenerateCarapaceInit = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p $HOME/.cache/carapace
    $DRY_RUN_CMD ${pkgs.carapace}/bin/carapace _carapace nushell > $HOME/.cache/carapace/init.nu
  '';
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
