" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

set nocompatible
filetype plugin indent on    " required
"allow backspacing over everything in insert mode
set backspace=indent,eol,start
set nobackup		" do not keep a backup file, use versions instead
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching
set autoindent		" always set autoindenting on
set directory=~/tmp/
set clipboard=unnamedplus
set nowritebackup
set hlsearch
set incsearch
set keymodel=startsel,stopsel
set ruler
set selection=exclusive
set selectmode=mouse,key
set whichwrap=b,s,<,>,[,]
set number 
syntax on
set encoding=utf-8
set nobomb
set fileencoding=utf-8
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent
set wrapmargin=5
set expandtab
set textwidth=78
set formatoptions+=tcoqnlw
set smarttab
set magic
set hidden
set laststatus=2
set statusline=
set statusline+=%-3.3n\ " buffer number
set statusline+=%f\ " filename
set statusline+=%h%m%r%w " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type
set statusline+=%= " right align remainder
set statusline+=%k\  " keymap name
set statusline+=%-14(%l/%L,%c%V%) " line, character
set statusline+=%<%p%%,%P " file position
set autowrite
set background=dark
set foldcolumn=2
