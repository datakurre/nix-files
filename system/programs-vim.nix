{ config, pkgs, ... }:
{
  home-manager.users.${config.user.name} = {
    programs = {
      nushell = {
        environmentVariables.EDITOR = "vim";
        shellAliases."vi" = "vim";
      };
      vim = {
        enable = true;
        settings.tabstop = 4;
        settings.shiftwidth = 4;
        plugins = [ pkgs.vimPlugins.Solarized ];
        extraConfig = ''
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
      };
    };
  };
  environment = {
    shellAliases."vi" = "vim";
    systemPackages = [
      pkgs.vim
      pkgs.unzip
    ];
  };
}
