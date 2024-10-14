{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    programs = {
      git = {
        enable = true;
        signing = {
          key = "5A9D4532";
          signByDefault = true;
        };
        userEmail = "asko.soukka@iki.fi";
        userName = "Asko Soukka";
        extraConfig = {
          push = {
            default = "current";
          };
          apply = {
            whitespace = "nowarn";
          };
          core = {
            autocrlf = "input";
          };
        };
      };
    };
    home = {
      file.".gitconfig".source = ./programs-git-gitconfig;
      packages = [ pkgs.git ];
    };
  };
}
