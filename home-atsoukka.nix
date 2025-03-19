{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./home/env-fonts.nix
    ./home/programs-vscode.nix
    ./home/programs-git.nix
    ./home/programs-shell.nix
    ./home/programs-ssh.nix
  ];
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "corefonts"
      "vscode"
      "vscode-with-extensions"
      "vscode-extension-github-copilot"
      "vscode-extension-github-copilot-chat"
    ];
  home.packages = [
    pkgs.vim
  ];
  programs.home-manager.enable = true;
  services.redshift = {
    enable = true;
    latitude = "25.4449";
    longitude = "62.1435";
    settings.redshift = {
      brightness-day = "1.0";
      brightness-night = "0.7";
      temp-day = 5500;
      temp-night = 3700;
    };
  };
  # services.screen-locker = {
  #   enable = true;
  #   inactiveInterval = 1;
  #   lockCmd = "${pkgs.xlockmore}/bin/xlock -mode xjack -erasedelay 0";
  # };
  # systemd.user.startServices = true;
  xresources.properties = {
    "Xft.dpi" = "192";
    "Xcursor.theme" = "Adwaita";

    "XTerm*wideChars" = "true";
    "XTerm*locale" = "true";
    "XTerm*utf8" = "true";
    "XTerm*vt100Graphics" = "true";
    "XTerm*scrollBar" = "false";

    "XTerm*selectToClipboard" = "true";
    "XTerm*faceName" = "DejaVu Sans Mono for Powerline";
    "XTerm*faceSize" = "11";
    "XTerm*saveLines" = "1024";
    "XTerm*cursorTheme" = "Adwaita";

    "*background" = "#002b36";
    "*foreground" = "#839496";
    "*fadeColor" = "#002b36";
    "*cursorColor" = "#93a1a1";
    "*pointerColorBackground" = "#586e75";
    "*pointerColorForeground" = "#93a1a1";

    # black dark/light
    "*color0" = "#073642";
    "*color8" = "#002b36";

    # red dark/light
    "*color1" = "#dc322f";
    "*color9" = "#cb4b16";

    # green dark/light
    "*color2" = "#859900";
    "*color10" = "#586e75";

    # yellow dark/light
    "*color3" = "#b58900";
    "*color11" = "#657b83";

    # blue dark/light
    "*color4" = "#268bd2";
    "*color12" = "#839496";

    # magenta dark/light
    "*color5" = "#d33682";
    "*color13" = "#6c71c4";

    # cyan dark/light
    "*color6" = "#2aa198";
    "*color14" = "#93a1a1";

    # white dark/light
    "*color7" = "#eee8d5";
    "*color15" = "#fdf6e3";
  };
  xsession.enable = true;
  xsession.initExtra = ''
    xrandr --dpi 192
  '';
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./home/services-xmonad.hs;
  };
  home.file.".bashrc.d/99-nix.sh".source = lib.mkForce (
    builtins.toFile "99-nix.sh" ''
      . /home/atsoukka/.nix-profile/etc/profile.d/nix.sh
      export SHELL=/home/atsoukka/.nix-profile/bin/nu
      export XTERM_SHELL=/home/atsoukka/.nix-profile/bin/nu
    ''
  );
  home.file.".Xmodmap".source = lib.mkForce (
    builtins.toFile "Xmodmap" ''
      pointer = 1 2 3 4 5 6 7 0 9 10
    ''
  );
}
