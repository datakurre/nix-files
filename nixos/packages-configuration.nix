{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    acpi
    afew
    azure-cli
    chromium
    dnsmasq
    evince
    firefox-devedition-bin
    gettext
    gimp
    git
    gnumake
    gnupg
    haskellPackages.xmonad
    htop
    irssi
    isync
    jetbrains.pycharm-professional
#   (jetbrains.pycharm-professional.override { jdk = oraclejdk8; })
    jq
    lastpass-cli
    libreoffice
    mercurial
    msmtp
    ncmpcpp
    networkmanagerapplet
    networkmanager_vpnc
    nixops
    nodejs
    notmuch
    pass
    phantomjs2
    psmisc
    pythonFull
    pythonPackages.alot
    pythonPackages.docker_compose
    rfkill
    trayer
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
    yarn
    zip
  ];

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
