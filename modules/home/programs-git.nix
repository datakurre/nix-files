{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    signing = {
      key = "5A9D4532";
      signByDefault = true;
    };
    settings = {
      user = {
        email = "asko.soukka@iki.fi";
        name = "Asko Soukka";
      };
      push.default = "current";
      apply.whitespace = "nowarn";
      core.autocrlf = "input";
    };
  };
  home = {
    file.".gitconfig".source = ../programs-git-gitconfig;
    packages = [ pkgs.git ];
  };
}
