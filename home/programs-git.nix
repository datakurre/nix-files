{ config, pkgs, ... }:
{
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
    packages = [
      pkgs.git
      (pkgs.stdenv.mkDerivation {
        name = "gitlog";
        builder = builtins.toFile "builder.sh" ''
                      source $stdenv/setup
                      mkdir -p $out/bin
                      cat > $out/bin/gitlog << EOF
          #!/usr/bin/env bash
          git log \
          --pretty=format:"- %s%n  [%an]" "\`git describe --tags|grep -o '^[^-]*'\`"..HEAD
          EOF
                      chmod u+x $out/bin/gitlog
        '';
      })
      (pkgs.stdenv.mkDerivation {
        name = "gitclog";
        builder = builtins.toFile "builder.sh" ''
                      source $stdenv/setup
                      mkdir -p $out/bin
                      cat > $out/bin/gitclog << EOF
          #!/usr/bin/env bash
          head -n 6 \$1 \
          >> \$1.new && gitlog \
          >> \$1.new && tail -n +8 \$1 \
          >> \$1.new && mv \$1.new \$1
          EOF
                      chmod u+x $out/bin/gitclog
        '';
      })
    ];
  };
}
