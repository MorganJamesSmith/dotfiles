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
endif

if ! filereadable(expand('~/.config/nvim/color/apprentice.vim'))
	echo "Downloading apprentice color scheme..."
	silent !mkdir -p ~/.config/nvim/color/
	silent !curl "https://raw.githubusercontent.com/romainl/Apprentice/master/colors/apprentice.vim" > ~/.config/nvim/color/apprentice.vim
endif


call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
    Plug 'vim-airline/vim-airline'
    Plug 'tpope/vim-speeddating'
    Plug 'vim-syntastic/syntastic'
    Plug 'jreybert/vimagit'
    Plug 'airblade/vim-gitgutter'
call plug#end()

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

" Airline
let g:airline#extensions#tabline#enabled = 1

" Spell-check set to <leader>o, 'o' for 'orthography':
	map <leader>o :setlocal spell! spelllang=en_us<CR>

" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow splitright

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

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

