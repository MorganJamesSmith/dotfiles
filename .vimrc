"        _
" __   _(_)_ __ ___  _ __ ___
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__
"   \_/ |_|_| |_| |_|_|  \___|

let mapleader =" "

call plug#begin('~/.vim/plugged')
    Plug 'junegunn/goyo.vim'
    Plug 'vim-airline/vim-airline'
    Plug 'terryma/vim-multiple-cursors'
    Plug 'suan/vim-instant-markdown'
    Plug 'tpope/vim-speeddating'
    Plug 'vim-syntastic/syntastic'
    Plug 'ctrlpvim/ctrlp.vim'
    " Git Plugins
    Plug 'jreybert/vimagit'
    Plug 'airblade/vim-gitgutter'
    " Typescript/Javascript Plugins
    Plug 'Quramy/tsuquyomi'
    Plug 'leafgarland/typescript-vim'
    Plug 'Quramy/vim-js-pretty-template'
    Plug 'jason0x43/vim-js-indent'
call plug#end()

" Plugin Settings

" Airline
let g:airline#extensions#tabline#enabled = 1

" Tsuquyomi
autocmd FileType typescript nmap <buffer> <Leader>t : <C-u>echo tsuquyomi#hint()<CR>
let g:tsuquyomi_disable_quickfix = 1

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_typescript_checkers = ['tsuquyomi'] " You shouldn't use 'tsc' checker.

" CtrlP
let g:ctrlp_working_path_mode = 'w'

" Some basics:
set nocompatible
filetype plugin on
syntax on
set encoding=utf-8
set number relativenumber
filetype plugin indent on
set tabstop=4
set shiftwidth=4
set updatetime=100

" Enable autocompletion:
set wildmode=longest,list,full

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Goyo plugin makes text more readable when writing prose:
map <leader>f :Goyo \| set linebreak<CR>

" Spell-check set to <leader>s, 's' for 'spelling':
map <leader>s :setlocal spell! spelllang=en_us<CR>

" Splits open at the bottom and right
set splitbelow splitright

" Shortcutting split navigation, saving a keypress:
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Replace all is aliased to S.
nnoremap S :%s//g<Left><Left>

" Use urlscan to choose and open a url:
:noremap <leader>u :w<Home>silent <End> !urlscan<CR>
:noremap ,, :w<Home>silent <End> !urlscan<CR>

" Automatically deletes all trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e

