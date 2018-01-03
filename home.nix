{ pkgs, config, ... }:

{
  imports = [ ./factorio.nix ];

  home.packages = with pkgs; [
    acpi
    afew
    chromium
    evince
    firefox-devedition-bin
    gimp
    git
    gnumake
    gnupg
    htop
    irssi
    isync
    jetbrains.pycharm-professional
    jq
    lastpass-cli
    libreoffice
    lynx
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
    vanilla-dmz
    vokoscreen
    vpnc
    xlockmore
    w3m
    yarn
    zip
  ];

  home.file.".buildout/default.cfg".text = ''
    [buildout]
    download-cache = ${config.home.homeDirectory}/.cache/download-cache
    eggs-directory = ${config.home.homeDirectory}/.cache/eggs-directory
    extends-cache = ${config.home.homeDirectory}/.cache/extends-cache
  '';
  home.file.".docutils.conf".source = ./dotfiles/docutils.conf;
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
  home.file.".config/alot/config".text = import ./dotfiles/alot.nix { inherit config; };
  home.file.".config/alot/signature-jyu".source = ./dotfiles/alot.signature-jyu;
  home.file.".config/alot/themes/solarized_dark".source = ./dotfiles/alot.theme;
  home.file.".notmuch-iki".text = import ./dotfiles/notmuch-iki.nix { inherit config; };
  home.file.".notmuch-jyu".text = import ./dotfiles/notmuch-jyu.nix { inherit config; };
  home.file.".mail/iki/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-iki-pre-new;
  home.file.".mail/iki/.notmuch/hooks/pre-new".executable = true;
  home.file.".mail/jyu/.notmuch/hooks/pre-new".source = ./dotfiles/notmuch-jyu-pre-new;
  home.file.".mail/jyu/.notmuch/hooks/pre-new".executable = true;

  programs.home-manager.enable = true;
  programs.home-manager.path = "https://github.com/rycee/home-manager/archive/master.tar.gz";

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

  programs.zsh.enable = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.oh-my-zsh.enable = true;
  programs.zsh.shellAliases = {
    vi = "vim";
    tray = "${pkgs.trayer}/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --widthtype pixel --width 32 --expand false";
    notmuch-iki = "notmuch --config=${config.home.homeDirectory}/.notmuch-iki";

    alot-iki = "alot -n ${config.home.homeDirectory}/.notmuch-iki";
  };
  programs.zsh.oh-my-zsh.plugins = [
    "cpv" "git" "pass" "pip" "python" "coffee" "colorize"
  ];
  programs.zsh.oh-my-zsh.theme = "robbyrussell";
  programs.zsh.initExtra = ''
    function dot2pdf() { nix-shell -p graphviz --run "dot -Tps $1"|ps2pdf - }
    function gitlog() { git log --pretty=format:"- %s%n  [%an]" "`git describe --tags|grep -o '^[^-]*'`"..HEAD; }
    function gitclog() { head -n 6 $1 >> $1.new && gitlog >> $1.new && tail -n +8 $1 >> $1.new && mv $1.new $1; }
    export EDITOR="vim";
    export GS_OPTIONS="-sPAPERSIZE=a4";
    export SSL_CERT_FILE="/etc/ssl/certs/ca-bundle.crt";
  '';

  services.network-manager-applet.enable = true;

  services.polybar.enable = false;
  services.polybar.script = "polybar default &";
  services.polybar.config = {
    "section/base" = { include-file = ./dotfiles/polybar.conf; };
  };

  programs.ssh.enable = true;
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
    "Xcursor.size" = "24";

    "XTerm*selectToClipboard" = "true";
    "XTerm*faceName" = "DejaVuSansMonoBook";
    "XTerm*faceSize" = "11";
    "XTerm*saveLines" = "1024";

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
  programs.vim.plugins = [ "Solarized" ];
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
    set colorcolumn=80

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
