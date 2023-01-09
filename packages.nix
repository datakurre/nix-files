pkgs: withGUI: with pkgs; [
  acpi
  autorandr
  bitwarden-cli
  camunda-modeler
  cookiecutter
  docker-compose
  evince
  firefox
  gimp
  git
  gnumake
  htop
  irssi
  isync
  jetbrains.idea-community
  jetbrains.pycharm-professional
  jq
  libreoffice
  lynx
  mockoon
  nixGL
  networkmanager-vpnc
  networkmanagerapplet
  obs-studio
  pass
  pavucontrol
  psmisc
  rccFHSUserEnv
  teams
  unzip
  vagrant
  vanilla-dmz
  vokoscreen
  vpnc
  w3m
  xlockmore
  yarn
  zbctl
  zip
# gitclog
# gitlog
# previous.zest-releaser-python2
# previous.zest-releaser-python3
  (xterm.overrideDerivation(old: {
    # fixes issue where locales were broken on non NixOS host
    postInstall = ''
      for prog in $out/bin/*; do
        wrapProgram $prog --set LOCALE_ARCHIVE ${pkgs.glibcLocales}/lib/locale/locale-archive
      done
    '';
  }))

  # fonts
  bakoma_ttf
  cantarell-fonts
  corefonts
  dejavu_fonts
  gentium
  inconsolata
  liberation_ttf
  nerdfonts
  powerline-fonts
  terminus_font
  ubuntu_font_family
]
