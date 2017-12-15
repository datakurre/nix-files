{ pkgs, ... }:

{
  home.packages = [];

  programs.home-manager.enable = true;
  programs.home-manager.path = "https://github.com/rycee/home-manager/archive/master.tar.gz";

  services.gpg-agent.enable = true;
  services.gpg-agent.defaultCacheTtl = 1800;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.grabKeyboardAndMouse = true;
  services.gpg-agent.enableScDaemon = true;

  programs.git.enable = true;
  programs.git.userName = "Asko Soukka";
  programs.git.userEmail = "asko.soukka@iki.fi";

  xsession.windowManager.xmonad.enable = true;
  xsession.windowManager.xmonad.enableContribAndExtras = true;
  xsession.windowManager.xmonad.config = ./dotfiles/xmonad.hs;

}
 
