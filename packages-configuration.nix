{ config, pkgs, ... }:

let unstable = import "/nix/var/nix/profiles/per-user/root/channels/nixos-unstable" {
  config = {
    allowUnfree = true;
  };
};

in

{
  environment.systemPackages = with pkgs; [
    acpi
    afew
    blueman
    dnsmasq
    gimp
    gnupg
    haskellPackages.xmonad
    isync
    lastpass-cli
    msmtp
    ncmpcpp
    networkmanagerapplet
    networkmanager_vpnc
    notmuch
    pass
    phantomjs2
    pythonPackages.alot
    trayer
    unzip
    vpnc
    w3m
    xlockmore
    xorg.xbacklight
    xss-lock
  ] ++ (with unstable; [
    azure-cli
    chromium
    firefox
    gettext
    git
    gnumake
#   (jetbrains.pycharm-professional.override { jdk = oraclejdk8; })
    jetbrains.pycharm-professional
    irssi
    libreoffice
    mercurial
    nixops
    nodejs
    npm2nix
    pypi2nix
    pythonFull
    pythonPackages.docker_compose
    vagrant
    vim
    vokoscreen
    yarn
  ]);

  nixpkgs.config.packageOverrides = pkgs: rec {
    afew = pkgs.pythonPackages.afew.overrideDerivation(args: {
      postPatch = ''
        sed -i "s|'notmuch', 'new'|'test', '1'|g" afew/MailMover.py
      '';
    });
  };

  nixpkgs.config.allowUnfree = true;
}
