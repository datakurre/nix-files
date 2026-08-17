{
  pkgs,
  lib,
  ...
}:
let
  gitChangelog = pkgs.writeShellApplication {
    name = "git-changelog";
    runtimeInputs = [
      pkgs.git
      pkgs.python3
    ];
    text = ''
      exec ${pkgs.python3}/bin/python3 ${./git-changelog.py} "$@"
    '';
  };
in
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
    nushell-ai.enable = true;
    starship = {
      enable = true;
      enableBashIntegration = true;
      enableNushellIntegration = true;
    };
  };
  home.packages = [
    gitChangelog
    pkgs.jq
  ];
}
