{ pkgs, user ? { name = "atsoukka"; description = "Asko Soukka"; home = "/home/atsoukka"; }}:

{
  imports = [
#   ./private/factorio.nix
  ];

  programs.home-manager.enable = true;
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  home.packages = with pkgs; [
    alot
    acpi
    afew
    autorandr
    avidemux
    cachix
    camunda-modeler
    chromium
    cookiecutter
    curl
    cpulimit
    docker-compose
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
    libreoffice
    lynx
    zoom-us
    msmtp
    ncmpcpp
    networkmanager-vpnc
    networkmanagerapplet
    notmuch
    (openshot-qt.overridePythonAttrs(old: {
      postPatch = ''
        substituteInPlace src/classes/query.py \
          --replace 'hasattr("project", app)' 'hasattr(app, "project")' \
          --replace 'hasattr("current_filepath", app.project)' 'hasattr(app.project, "current_filepath")'
      '';
    }))
    pass
    pavucontrol
    pulseeffects-legacy
    gnome-icon-theme
    psmisc
    rcc
    signal-desktop
    teams
    unzip
    unzip
    vagrant
    v4l-utils
    virt-manager
    vingester
    vlc
    vokoscreen
    vpnc
    w3m
    xlockmore
    set-exposure
    zest-releaser-python2
    zest-releaser-python3
    zip
  ] ++ [
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
  ];

  home.file.".buildout/default.cfg".text = ''
    [buildout]
    download-cache = ${user.home}/.cache/download-cache
    eggs-directory = ${user.home}/.cache/eggs-directory
    extends-cache = ${user.home}/.cache/extends-cache
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
  home.file.".config/alot/config".text = import ./dotfiles/alot.nix { prefix = user.home; };
  home.file.".config/alot/signature-jyu".source = ./dotfiles/alot.signature-jyu;
  home.file.".config/alot/themes/solarized_dark".source = ./dotfiles/alot.theme;
  home.file.".notmuch-iki".text = import ./dotfiles/notmuch-iki.nix { prefix = user.home; };
  home.file.".notmuch-jyu".text = import ./dotfiles/notmuch-jyu.nix { prefix = user.home; };
  home.file.".mail/iki/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-iki-pre-new;
  home.file.".mail/iki/.notmuch/hooks/pre-new".executable = true;
  home.file.".mail/iki/.notmuch/hooks/post-new".source = ./dotfiles/notmuch-iki-post-new;
  home.file.".mail/iki/.notmuch/hooks/post-new".executable = true;
  home.file.".mail/jyu/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-jyu-pre-new;
  home.file.".mail/jyu/.notmuch/hooks/pre-new".executable = true;

  programs.obs-studio.enable = true;
  programs.obs-studio.package = pkgs.obs-studio;
  programs.obs-studio.plugins = with pkgs.obs-studio-plugins; [
    obs-websocket
    obs-ndi
    pkgs.obs-backgroundremoval
  ];

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

  programs.vscode.enable = true;
  programs.vscode.package = (pkgs.vscode-fhsWithPackages (ps: with ps; [
    elmPackages.elm
    elmPackages.elm-analyse
#   elmPackages.elm-coverage
    elmPackages.elm-doc-preview
#   elmPackages.elm-format
    elmPackages.elm-instrument
    elmPackages.elm-json
    elmPackages.elm-language-server
    elmPackages.elm-live
    elmPackages.elm-optimize-level-2
    elmPackages.elm-review
    elmPackages.elm-test
    elmPackages.elm-upgrade
#   elmPackages.elm-verify-examples
    elmPackages.elm-xref
    (ps.python3Full.withPackages(ps: [
      (ps.robotframework.overridePythonAttrs(old: rec {
        version = "4.1.1";
        src =  ps.fetchPypi {
          pname = "robotframework";
          extension = "zip";
          inherit version;
          sha256 = "0ddd9dzrn9gi29w0caab78zs6mx06wbf6f99g0xrpymjfz0q8gv6";
        };
        doCheck = false;
      }))
    ]))
  ]));
  programs.vscode.extensions = (with pkgs.vscode-extensions; [
    ms-python.python
    ms-vsliveshare.vsliveshare
    vscodevim.vim
    (pkgs.vscode-utils.buildVscodeMarketplaceExtension rec {
      mktplcRef = {
        name = "test-adapter-converter";
        publisher = "ms-vscode";
        version = "0.1.5";
        sha256 = "11x29njy746mjbv58s7cis2fj6xnmjqc0vn3nb4vv53skxcbhn4y";
      };
    })
    (pkgs.vscode-utils.buildVscodeMarketplaceExtension rec {
      mktplcRef = {
        name = "vscode-test-explorer";
        publisher = "hbenl";
        version = "2.21.1";
        sha256 = "022lnkq278ic0h9ggpqcwb3x3ivpcqjimhgirixznq0zvwyrwz3w";
      };
    })
    (pkgs.vscode-utils.buildVscodeMarketplaceExtension rec {
      mktplcRef = {
        name = "elm-ls-vscode";
        publisher = "Elmtooling";
        version = "2.4.1";
        sha256 = "1idhsrl9w8sc0qk58dvmyyjbmfznk3f4gz2zl6s9ksyz9d06vfrd";
      };
    })
    (pkgs.vscode-utils.buildVscodeMarketplaceExtension rec {
      mktplcRef = {
        name = "robotframework-lsp";
        publisher = "robocorp";
        version = "0.29.0";
        sha256 = "1waz2kkzy10rjwxpw9wdicm0bz5a10jpy06cwd9f95id1ppn3l0z";
      };
    })
    (pkgs.vscode-utils.buildVscodeMarketplaceExtension rec {
      mktplcRef = {
        name = "robocorp-code";
        publisher = "robocorp";
        version = "0.20.0";
        sha256 = "09dl08fb0qrnnna4x5d6z3jmj0kkl6gzkjwj12bi7v7khwm0r92a";
      };
      postInstall = ''
        mkdir -p $out/share/vscode/extensions/robocorp.robocorp-code/bin
        ln -s ${pkgs.rcc}/bin/rcc $out/share/vscode/extensions/robocorp.robocorp-code/bin
      '';
    })
  ]);

  programs.bash.enable = true;
  programs.bash.shellAliases = {
    vi = "vim";
    notmuch-iki = "EDITOR=vim notmuch --config=${user.home}/.notmuch-iki";
    notmuch-jyu = "EDITOR=vim notmuch --config=${user.home}/.notmuch-jyu";
    alot-iki = "EDITOR=vim alot -n ${user.home}/.notmuch-iki";
    alot-jyu = "EDITOR=vim alot -n ${user.home}/.notmuch-jyu";
    tls-fingerprint= "openssl s_client -connect $ -starttls smtp < /dev/null | openssl x509 -fingerprint -noout | cut -d'=' -f2";
  };
  programs.bash.bashrcExtra = ''
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    export NIX_PATH=$HOME/.nix-defexpr/channels$'' + ''{NIX_PATH:+:}$NIX_PATH
  '';
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
      user = builtins.substring 0 ((builtins.stringLength user.name) - 1) user.name + "_";
    };
    "jalava.cc.jyu.fi" = {
      user = user.name;
    };
  };
  services.gpg-agent.enable = true;
  services.gpg-agent.defaultCacheTtl = 1800;
  services.gpg-agent.enableSshSupport = true;
  services.gpg-agent.grabKeyboardAndMouse = true;
  services.gpg-agent.enableScDaemon = true;

  services.redshift.enable = true;
  services.redshift.settings.redshift.brightness-day = "1.0";
  services.redshift.settings.redshift.brightness-night = "0.7";
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
