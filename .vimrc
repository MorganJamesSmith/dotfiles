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
	Plug 'airblade/vim-gitgutter'
	Plug 'ledger/vim-ledger'
	Plug 'jamessan/vim-gnupg'
	Plug 'ycm-core/YouCompleteMe'
	Plug 'vim-scripts/TeTrIs.vim'
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
set softtabstop=4
set shiftwidth=4
set noexpandtab
set updatetime=100
set ignorecase " ignore case when searching...
set smartcase " ... unless pattern has uppercase character
set autochdir "change the working directory to the directory in which the file being opened lives
set cursorline "highlight current line
set listchars=trail:-,tab:-- " change the way tabs and line ends are displayed
set cursorline "highlight current line
set autoread

" Make tabs and trailing spaces visable
set list
set listchars=tab:!·,trail:·

" Spell-check set to <leader>o, 'o' for 'orthography':
map <leader>o :setlocal spell! spelllang=en_us<CR>

" Splits open at the bottom and right
set splitbelow splitright

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_min_count = 2 "show buffer line only when at least 2 buffers are open
let g:airline#extensions#tabline#buffer_idx_mode = 1 "show numbers in buffer line
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
nmap <leader>- <Plug>AirlineSelectPrevTab
nmap <leader>+ <Plug>AirlineSelectNextTab

" Nerd tree
map <leader>n :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" GitGutter settings
let g:gitgutter_max_signs = 5000 "max diff of 5000 lines

" Allow saving of files as sudo when forgetting to start vim using sudo
cmap w!! w !sudo tee > /dev/null %

" Check file in shellcheck:
map <leader>s :!clear && shellcheck %<CR>

" Check file in splint:
autocmd Filetype c map <leader>s :!clear && splint +posixstrictlib %<CR>

" Replace all is aliased to S.
nnoremap S :%s//g<Left><Left>

" Compile document, be it groff/LaTeX/markdown/etc.
map <leader>c :w! \| !compiler <c-r>%<CR>:edit<CR>

" Open corresponding .pdf/.html or preview
map <leader>p :!opout <c-r>%<CR><CR>

" Automatically deletes all trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e

colorscheme apprentice

function! LoadCscope()
	let db = findfile("cscope.out", ".;")
	if (!empty(db))
		let path = strpart(db, 0, match(db, "/cscope.out$"))
		set nocscopeverbose " suppress 'duplicate connection' error
		exe "cs add " . db . " " . path
		set cscopeverbose
		set csre "use cscope.out file location as the prefix to construct an absolute path
	endif
endfunction

autocmd Filetype c call LoadCscope()
autocmd Filetype cpp call LoadCscope()

if ! filereadable(expand('~/.config/nvim/cscope_maps.vim'))
	echo "Downloading cscope mappings"
	silent !curl http://cscope.sourceforge.net/cscope_maps.vim --output ~/.config/nvim/cscope_maps.vim
endif

source ~/.config/nvim/cscope_maps.vim
