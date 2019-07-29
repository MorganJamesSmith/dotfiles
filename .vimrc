"        _
" __   _(_)_ __ ___  _ __ ___
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__
"   \_/ |_|_| |_| |_|_|  \___|

let mapleader =" "

if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ~/.config/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

if ! filereadable(expand('~/.config/nvim/colors/apprentice.vim'))
	echo "Downloading apprentice color scheme..."
	silent !mkdir -p ~/.config/nvim/colors/
	silent !curl "https://raw.githubusercontent.com/romainl/Apprentice/master/colors/apprentice.vim" > ~/.config/nvim/colors/apprentice.vim
endif


call plug#begin('~/.vim/plugged')
	Plug 'scrooloose/nerdtree'
	Plug 'jreybert/vimagit'
	Plug 'terryma/vim-multiple-cursors'
	Plug 'tpope/vim-speeddating'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-syntastic/syntastic'
	Plug 'airblade/vim-gitgutter'
	Plug 'ledger/vim-ledger'
call plug#end()

set bg=light
set go=a
set mouse=a
set nohlsearch
set clipboard=unnamedplus

" Some basics:
	set nocompatible
	filetype plugin on
	syntax on
	set encoding=utf-8
	set number relativenumber
" Enable autocompletion:
	set wildmode=longest,list,full
" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
	set tabstop=4
	set shiftwidth=4
	set noexpandtab
	set updatetime=100

" Spell-check set to <leader>o, 'o' for 'orthography':
	map <leader>o :setlocal spell! spelllang=en_us<CR>

" Splits open at the bottom and right
	set splitbelow splitright

" Airline
let g:airline#extensions#tabline#enabled = 1

" Nerd tree
	map <leader>n :NERDTreeToggle<CR>
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Check file in shellcheck:
	map <leader>s :!clear && shellcheck %<CR>

" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>

" Compile document, be it groff/LaTeX/markdown/etc.
	map <leader>c :w! \| !compiler <c-r>%<CR>

" Open corresponding .pdf/.html or preview
	map <leader>p :!opout <c-r>%<CR><CR>

" Automatically deletes all trailing whitespace on save.
	autocmd BufWritePre * %s/\s\+$//e

colorscheme apprentice

