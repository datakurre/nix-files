{ config, lib, pkgs, specialArgs, ... }:

let
  bashsettings = import ./bash.nix;
  packages = import ./packages.nix;
  vimsettings = import ./vim.nix;
  xresources = import ./xresources.nix;

  # hacky way of determining which machine I'm running this from
  inherit (specialArgs) withGUI isDesktop networkInterface localOverlay;

  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux isDarwin;
in {
  # nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ localOverlay ];

  nixpkgs.config = {
    allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "corefonts"
      "pycharm-professional"
      "teams"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.packages = packages pkgs withGUI;
  home.stateVersion = "22.11";

  fonts.fontconfig.enable = true;

  programs.git.enable = true;
  programs.git.userName = "Asko Soukka";
  programs.git.userEmail = "asko.soukka@iki.fi";
  programs.git.signing.key = "5A9D4532";
  programs.git.signing.signByDefault = true;
  programs.git.extraConfig = {
    push = { default = "current"; };
    apply = { whitespace = "nowarn"; };
    core = { autocrlf = "input"; };
  };

  programs.starship.enable = true;
  programs.starship.enableBashIntegration = true;

  programs.thunderbird.enable = true;
  programs.thunderbird.settings."general.useragent.override" = "";
  programs.thunderbird.settings."privacy.donottrackheader.enabled" = true;
  programs.thunderbird.profiles.atsoukka.isDefault = true;
  programs.thunderbird.profiles.atsoukka.settings."mail.spellcheck.inline" = false;
  accounts.email.accounts.atsoukka.primary = true;
  accounts.email.accounts.atsoukka.realName = "Asko Soukka";
  accounts.email.accounts.atsoukka.userName = "atsoukka@jyu.fi";
  accounts.email.accounts.atsoukka.address = "asko.soukka@jyu.fi";
  accounts.email.accounts.atsoukka.passwordCommand = "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/email/jyu.gpg";
  accounts.email.accounts.atsoukka.imap.host = "outlook.office365.com";
  accounts.email.accounts.atsoukka.imap.port = 993;
  accounts.email.accounts.atsoukka.imap.tls.enable = true;
  accounts.email.accounts.atsoukka.smtp.host = "smtp.office365.com";
  accounts.email.accounts.atsoukka.smtp.port = 587;
  accounts.email.accounts.atsoukka.smtp.tls.enable = true;
  accounts.email.accounts.atsoukka.smtp.tls.useStartTls= true;
  accounts.email.accounts.atsoukka.thunderbird.enable = true;

  services.stalonetray.config = {
    decorations = null;
    dockapp_mode = null;
    geometry = "1x1-0+0";
    max_geometry = "5x1-0+0";
    grow_gravity = "SW";
    icon_gravity = "SW";
    icon_size = 24;
    kludges = "force_icons_size";
    skip_taskbar = true;
    sticky = true;
  };
  services.stalonetray.enable = true;
  services.blueman-applet.enable = false;
  services.network-manager-applet.enable = true;

  programs.chromium.enable = true;

  programs.ssh.enable = true;
  programs.ssh.extraConfig = ''
    CanonicalizeHostname yes
    CanonicalDomains kopla.jyu.fi cc.jyu.fi
    CanonicalizeMaxDots 0
    CanonicalizeFallbackLocal no
  '';
  programs.ssh.matchBlocks = {
    "*.jyu.fi" = {
      user = builtins.substring 0 ((builtins.stringLength specialArgs.username) - 1) specialArgs.username + "_";
    };
    "jalava.cc.jyu.fi" = {
      user = specialArgs.username;
    };
  };
  services.gpg-agent.enable = true;
  services.gpg-agent.defaultCacheTtl = 1800;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.grabKeyboardAndMouse = true;
  services.gpg-agent.enableScDaemon = true;

  programs.gpg.enable = true;
  programs.gpg.scdaemonSettings = {
    "pcsc-driver" = "/usr/lib64/libpcsclite.so.1";
  };

  services.redshift.enable = true;
  services.redshift.settings.brightness-day = { gamma = 1.0; adustment-method = "randr"; };
  services.redshift.settings.brightness-night = { gamma = 0.7; adustment-method = "randr"; };
  services.redshift.latitude = "62.1435";
  services.redshift.longitude = "25.4449";

  services.screen-locker.enable = true;
  services.screen-locker.lockCmd = "xlock -mode xjack -erasedelay 0";

  services.udiskie.enable = true;
  xsession.windowManager.xmonad.enable = true;
  xsession.windowManager.xmonad.enableContribAndExtras = true;
  xsession.windowManager.xmonad.config = ./dotfiles/xmonad.hs;

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = false;
  programs.vim = vimsettings pkgs;
  programs.bash = bashsettings pkgs;
  inherit xresources;

  home.file.".buildout/default.cfg".text = ''
    [buildout]
    download-cache = ${specialArgs.homeDirectory}/.cache/download-cache
    eggs-directory = ${specialArgs.homeDirectory}/.cache/eggs-directory
    extends-cache = ${specialArgs.homeDirectory}/.cache/extends-cache
  '';
  home.file.".docutils.conf".source = ./dotfiles/docutils.conf;
  home.file.".editorconfig".source = ./dotfiles/editorconfig.conf;
  home.file.".gitconfig".source = ./dotfiles/gitconfig;
  home.file.".irssi/config".source = ./dotfiles/irssi.conf;
  home.file.".irssi/scripts/secrets.pl".source = ./dotfiles/irssi.secrets.pl;
  home.file.".irssi/secrets".source = ./dotfiles/irssi.secrets;
  home.file.".irssi/startup".source = ./dotfiles/irssi.startup;
  home.file.".pdbrc".source = ./dotfiles/pdbrc.py;
# home.file.".mailcap".source = ./dotfiles/mailcap;
# home.file.".mbsyncrc".source = ./dotfiles/mbsyncrc;
# home.file.".msmtprc".source = ./dotfiles/msmtprc;
# home.file.".config/afew/config".source = ./dotfiles/afew.conf;
# home.file.".config/alot/config".text = import ./dotfiles/alot.nix { inherit prefix; };
# home.file.".config/alot/signature-jyu".source = ./dotfiles/alot.signature-jyu;
# home.file.".config/alot/themes/solarized_dark".source = ./dotfiles/alot.theme;
# home.file.".notmuch-iki".text = import ./dotfiles/notmuch-iki.nix { inherit prefix; };
# home.file.".notmuch-jyu".text = import ./dotfiles/notmuch-jyu.nix { inherit prefix; };
# home.file.".mail/iki/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-iki-pre-new;
# home.file.".mail/iki/.notmuch/hooks/pre-new".executable = true;
# home.file.".mail/iki/.notmuch/hooks/post-new".source = ./dotfiles/notmuch-iki-post-new;
# home.file.".mail/iki/.notmuch/hooks/post-new".executable = true;
# home.file.".mail/jyu/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-jyu-pre-new;
# home.file.".mail/jyu/.notmuch/hooks/pre-new".executable = true;
}
