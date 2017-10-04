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
    azure-cli
    blueman
    dnsmasq
    evince
    gettext
    gimp
    git
    gnumake
    gnupg
    haskellPackages.xmonad
    htop
    irssi
    isync
    jq
    lastpass-cli
    libreoffice
    mercurial
    msmtp
    ncmpcpp
    networkmanagerapplet
    networkmanager_vpnc
    notmuch
    pass
    phantomjs2
    psmisc
    pythonFull
    pythonPackages.alot
    trayer
    zip
    unzip
    vagrant
    vim
    vokoscreen
    vpnc
    w3m
    xlockmore
    xorg.xbacklight
    xpdf
    xss-lock
  ] ++ (with unstable; [
    chromium
    firefox
#   jetbrains.pycharm-professional
    (jetbrains.pycharm-professional.override { jdk = oraclejdk8; })
    nixops
    nodejs
    npm2nix
    pypi2nix
    pythonPackages.docker_compose
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
