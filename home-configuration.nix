{ pkgs, prefix, ... }:

let username = "atsoukka";
    unstable = import "/home/atsoukka/.nix-defexpr/channels/unstable" {};
in

{
  imports = [
    ./private/factorio.nix
  ];

  programs.home-manager.enable = true;
  programs.direnv.enable = true;
  programs.direnv.enableNixDirenvIntegration = true;

  fonts.fontconfig.enable = pkgs.lib.mkForce true;

  home.packages = with pkgs; [
    acpi
    afew
    autorandr
    cachix
    camunda-modeler
    chromium
    cookiecutter
    docker_compose
    evince
    findimagedupes
    firefox-devedition-bin
    gimp
    git
    gitclog
    gitlog
    gnumake
    gnupg
    htop
    imagemagick
    irssi
    isync
    jetbrains.idea-community
    jetbrains.pycharm-professional
    jq
    lastpass-cli
    lessc
    lynx
    msmtp
    ncmpcpp
    networkmanager_vpnc
    networkmanagerapplet
    notmuch
    pass
    pavucontrol
    pulseeffects
    pidgin-with-plugins
    psmisc
    python3Full
    sass
    signal-desktop
    teams
    unzip
    vagrant
    vanilla-dmz
    vlc
    vokoscreen
    vpnc
    w3m
    xlockmore
    yarn
    zest-releaser-python2
    zest-releaser-python3
    zip

    (unstable.obs-studio.override {
      libcef = ((unstable.libcef.overrideDerivation(old: {
        src = fetchurl {
          name = "cef_binary_74.1.14+g50c3c5c+chromium-74.0.3729.131_linux64_minimal.tar.bz2";
          url = "https://cef-builds.spotifycdn.com/cef_binary_74.1.14%2Bg50c3c5c%2Bchromium-74.0.3729.131_linux64.tar.bz2";
          sha256 = "1p88vxivvg133x6p7lk88snn6884n5x10qp2g8m9lagfngdb4cb3";
        };
      })).override {
        nss = unstable.nss_3_53;
      });
    })

#   (unstable.obs-studio.override {
#     libcef = ((unstable.libcef.overrideDerivation(old: {
#       src = fetchurl {
#         name = "cef_binary_75.1.14+gc81164e+chromium-75.0.3770.100_linux64.tar.bz2";
#         url = "https://cef-builds.spotifycdn.com/cef_binary_75.1.14%2Bgc81164e%2Bchromium-75.0.3770.100_linux64.tar.bz2";
#         sha256 = "1lfk7by8ndr9zc1qjh8i1ckzzhs838scc0c1m716qlnj4nmrn406";
#       };
#     })).override {
#       nss = unstable.nss_3_53;
#     });
#   })

    (python3Packages.alot.overridePythonAttrs(old: {
      postPatch = ''
        find alot -type f -print0|xargs -0 sed -i "s|payload.encode('raw-unicode-escape')|payload.encode('utf-8')|g"
      '';
    }))

    (xterm.overrideDerivation(old: {
      # fixes issue where locales were broken on non NixOS host
      postInstall = ''
        for prog in $out/bin/*; do
          wrapProgram $prog --set LOCALE_ARCHIVE ${pkgs.glibcLocales}/lib/locale/locale-archive
        done
      '';
    }))
  ] ++ [
    bakoma_ttf
    cantarell_fonts
    corefonts
    dejavu_fonts
    gentium
    inconsolata
    liberation_ttf
    nerdfonts
    powerline-fonts
    terminus_font
    ubuntu_font_family
  ];

  home.file.".buildout/default.cfg".text = ''
    [buildout]
    download-cache = ${prefix}/.cache/download-cache
    eggs-directory = ${prefix}/.cache/eggs-directory
    extends-cache = ${prefix}/.cache/extends-cache
  '';
  home.file.".docutils.conf".source = ./dotfiles/docutils.conf;
  home.file.".gitconfig".source = ./dotfiles/gitconfig;
  home.file.".editorconfig".source = ./dotfiles/editorconfig.conf;
  home.file.".mailcap".source = ./dotfiles/mailcap;
  home.file.".mbsyncrc".source = ./dotfiles/mbsyncrc;
  home.file.".msmtprc".source = ./dotfiles/msmtprc;
  home.file.".pdbrc".source = ./dotfiles/pdbrc.py;
  home.file.".irssi/config".source = ./dotfiles/irssi.conf;
  home.file.".irssi/scripts/secrets.pl".source = ./dotfiles/irssi.secrets.pl;
  home.file.".irssi/secrets".source = ./dotfiles/irssi.secrets;
  home.file.".irssi/startup".source = ./dotfiles/irssi.startup;
  home.file.".config/afew/config".source = ./dotfiles/afew.conf;
  home.file.".config/alot/config".text = import ./dotfiles/alot.nix { inherit prefix; };
  home.file.".config/alot/signature-jyu".source = ./dotfiles/alot.signature-jyu;
  home.file.".config/alot/themes/solarized_dark".source = ./dotfiles/alot.theme;
  home.file.".notmuch-iki".text = import ./dotfiles/notmuch-iki.nix { inherit prefix; };
  home.file.".notmuch-jyu".text = import ./dotfiles/notmuch-jyu.nix { inherit prefix; };
  home.file.".mail/iki/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-iki-pre-new;
  home.file.".mail/iki/.notmuch/hooks/pre-new".executable = true;
  home.file.".mail/iki/.notmuch/hooks/post-new".source = ./dotfiles/notmuch-iki-post-new;
  home.file.".mail/iki/.notmuch/hooks/post-new".executable = true;
  home.file.".mail/jyu/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-jyu-pre-new;
  home.file.".mail/jyu/.notmuch/hooks/pre-new".executable = true;

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

  programs.bash.enable = true;
  programs.bash.shellAliases = {
    notmuch-iki = "EDITOR=vim notmuch --config=${prefix}/.notmuch-iki";
    notmuch-jyu = "EDITOR=vim notmuch --config=${prefix}/.notmuch-jyu";
    alot-iki = "EDITOR=vim alot -n ${prefix}/.notmuch-iki";
    alot-jyu = "EDITOR=vim alot -n ${prefix}/.notmuch-jyu";
    tls-fingerprint= "openssl s_client -connect $ -starttls smtp < /dev/null | openssl x509 -fingerprint -noout | cut -d'=' -f2";
  };
  programs.bash.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    EDITOR = "vim";
    NIX_REMOTE = "daemon";
    GS_OPTIONS = "-sPAPERSIZE=a4";
    SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";
  };
  programs.starship.enable = true;
  programs.starship.enableBashIntegration = true;

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

  programs.ssh.enable = true;
  programs.ssh.extraConfig = ''
    CanonicalizeHostname yes
    CanonicalDomains kopla.jyu.fi cc.jyu.fi
    CanonicalizeMaxDots 0
    CanonicalizeFallbackLocal no
  '';
  programs.ssh.matchBlocks = {
    "*.jyu.fi" = {
      user = builtins.substring 0 ((builtins.stringLength username) - 1) username + "_";
    };
    "jalava.cc.jyu.fi" = {
      user = username;
    };
  };
  services.gpg-agent.enable = true;
  services.gpg-agent.defaultCacheTtl = 1800;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.grabKeyboardAndMouse = true;
  services.gpg-agent.enableScDaemon = true;

  services.redshift.enable = true;
  services.redshift.brightness.day = "1.0";
  services.redshift.brightness.night = "0.7";
  services.redshift.latitude = "62.1435";
  services.redshift.longitude = "25.4449";

  services.screen-locker.enable = true;
  services.screen-locker.lockCmd = "xlock -mode xjack -erasedelay 0";

  services.udiskie.enable = true;
  xsession.windowManager.xmonad.enable = true;
  xsession.windowManager.xmonad.enableContribAndExtras = true;
  xsession.windowManager.xmonad.config = ./dotfiles/xmonad.hs;

  xresources.properties = {
    "Xcursor.theme" = "Vanilla-DMZ";
    "Xcursor.size" = "32";

    "XTerm*wideChars" = "true";
    "XTerm*locale" = "true";
    "XTerm*utf8" = "true";
    "XTerm*vt100Graphics" = "true";

    "XTerm*selectToClipboard" = "true";
    "XTerm*faceName" = "DejaVu Sans Mono for Powerline";
    "XTerm*faceSize" = "11";
    "XTerm*saveLines" = "1024";

    "UXTerm*faceName" = "DejaVu Sans Mono for Powerline";
    "UXTerm*faceSize" = "11";

    "*background" = "#002b36";
    "*foreground" = "#839496";
    "*fadeColor" = "#002b36";
    "*cursorColor" = "#93a1a1";
    "*pointerColorBackground" = "#586e75";
    "*pointerColorForeground" = "#93a1a1";

    # black dark/light
    "*color0" = "#073642";
    "*color8" = "#002b36";

    # red dark/light
    "*color1" = "#dc322f";
    "*color9" = "#cb4b16";

    # green dark/light
    "*color2" = "#859900";
    "*color10" = "#586e75";

    # yellow dark/light
    "*color3" = "#b58900";
    "*color11" = "#657b83";

    # blue dark/light
    "*color4" = "#268bd2";
    "*color12" = "#839496";

    # magenta dark/light
    "*color5" = "#d33682";
    "*color13" = "#6c71c4";

    # cyan dark/light
    "*color6" = "#2aa198";
    "*color14" = "#93a1a1";

    # white dark/light
    "*color7" = "#eee8d5";
    "*color15" = "#fdf6e3";
  };

  programs.vim.enable = true;
  programs.vim.settings.tabstop = 4;
  programs.vim.settings.shiftwidth = 4;
  programs.vim.plugins = [ pkgs.vimPlugins.Solarized ];
  programs.vim.extraConfig = ''
    " Enable autoindent
    set ai

    " Enable options in editable files
    set modeline

    " Enable filetype detection
    filetype on

    " Enable loading the plugin files for specific file types
    filetype plugin on

    " Write the old file out when switching between files
    set autowrite

    " Backups
    set noswapfile
    set nobackup       " disabled to keep inode numbers unchanged on OSX
    set nowritebackup  " disabled to keep inode numbers unchanged on OSX

    " Do not store global and local values in a session
    set ssop-=options

    " Do not store folds
    set ssop-=folds

    " When editing a file, always jump to the last cursor position
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal g'\"" |
    \ endif
    command SessionSave mksession .session.vim
    command SessionLoad source .session.vim

    " Colors
    set t_Co=256
    colorscheme solarized
    let g:solarized_termcolors=256
    set background=dark

    " Change the terminal's title
    set title

    " Don't beep
    set noerrorbells

    " Hide buffers instead of closing them
    set hidden

    " Enable syntax higlighting
    syntax on

    " Show trailing whitespace characters
    set list
    set listchars=tab:>.,trail:.,extends:#,nbsp:.

    " Show margin column
    set colorcolumn=88

    " Leader mappings
    let mapleader = ","
    let maplocalleader = ";"

    " No Vi stuff anymore
    set nocompatible

    " Make backspace work like most other apps
    set backspace=2

    " Show error buffer
    nnoremap <leader>e :Errors<CR>

    " Force redraw to C-l
    nnoremap <Leader>r :redraw!<CR>

    " Map escape key to jj -- much faster
    imap jj <esc>

    " Use Q for formatting the current paragraph (or selection).
    " Forces 80 character lines.
    vmap Q gq
    nmap Q gqap

    " Move by screen lines instead of file line. Nice with long lines.
    nnoremap j gj
    nnoremap k gk

    " Strip all trailing whitespace in the current file
    nnoremap <leader>W :%s/\s\+$//<cr>:let @/=\'\'<CR>

    " Toggle spelling
    set spelllang=en_us
    nmap <silent> <leader>S :set spell!<CR>

    " Make searches case-insensitive, unless they contain upper-case letters:
    set ignorecase
    set smartcase

    " This turns off Vim’s crazy default regex characters and makes searches
    " use normal regexes.
    nnoremap / /\v
    vnoremap / /\v

    " Work together to highlight search results (as you type). It’s really
    " quite handy, as long as you have the next line as well.
    set incsearch
    set showmatch
    set hlsearch

    " Toggle pastemode easily in insert and command mode
    set pastetoggle=<F2>

    " Always disable paste mode when leaving insert mode
    au InsertLeave * set nopaste

    " Show path of the edited file
    map <Leader>p :echo expand('%:p') <CR>

    " Easily change directory to the file being edited.
    nmap <Leader>cd :cd %:p:h<CR>

    " Use smart indenting
    set smarttab expandtab autoindent

    " Command for resetting tab width
    command -nargs=1 TabWidth setlocal shiftwidth=<args> tabstop=<args> softtabstop=<args>

    " Makefiles and gitconfig require tab
    au FileType make,gitconfig setlocal noexpandtab

    " Define custom file types
    au BufNewFile,BufRead *.zcml setfiletype xml
    au BufNewFile,BufRead *.coffee setfiletype coffee
    au BufNewFile,BufRead *.json setfiletype json
    au BufNewFile,BufRead *.ru setfiletype ruby
    au BufNewFile,BufRead *.conf setfiletype conf
    au BufNewFile,BufRead *.jade setfiletype jade
    au BufNewFile,BufRead *.md setfiletype markdown
    au BufNewFile,BufRead *.markdown setfiletype markdown
    au BufNewFile,BufRead *.pt set filetype=xml
  '';

}
