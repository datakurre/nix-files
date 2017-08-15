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
    evince
    gimp
    gnupg
    haskellPackages.xmonad
    isync
    jq
    lastpass-cli
    msmtp
    ncmpcpp
    networkmanagerapplet
    networkmanager_vpnc
    pass
    phantomjs2
    trayer
    unzip
    vpnc
    w3m
    xlockmore
    xpdf
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
    notmuch
    npm2nix
    pypi2nix
    pythonFull
    pythonPackages.docker_compose
    pythonPackages.alot
    vagrant
    vim
    vokoscreen
    yarn
  ]);

  nixpkgs.config.packageOverrides = pkgs: rec {
    gmime = pkgs.gmime.overrideDerivation(old: {
      propagatedBuildInputs = old.propagatedBuildInputs ++ [
        pkgs.gpgme.dev
      ];
    });
    afew = pkgs.pythonPackages.afew.overrideDerivation(old: {
      postPatch = ''
        sed -i "s|'notmuch', 'new'|'test', '1'|g" afew/MailMover.py
      '';
    });
  };

  nixpkgs.config.allowUnfree = true;
}
