""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
"                                 Vim Configs
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set nocompatible                    " be iMproved, required
filetype off                        " required

set hidden
set number
set relativenumber
set autoindent
set smartindent
set tabstop=4
set expandtab
set shiftwidth=4
set splitright
set textwidth=120
set t_co=256
set showmatch                       " intelligent comments
set comments=sl:/*,mb:\ *,elx:\ */
set mouse=a
set hlsearch
set autochdir                       " current dir is always current working file dir
set clipboard=unnamedplus           " yank/put uses system clipboard by default

let mapleader = ","
set timeoutlen=10
set ttimeoutlen=0

" Remap for escape key
inoremap kj <Esc>
inoremap jk <Esc>
inoremap fd <Esc>
inoremap df <Esc>

" U is useless
nnoremap U u

" Allows to search for selection with /
vnoremap / y/\V<C-R>"<CR>

" Better highlight color for readability
highlight Search ctermbg=lightblue ctermfg=black  term=underline

" remove trailing whitespaces on save
autocmd BufWritePre * %s/\s\+$//e

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                    Style
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme desert


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                    Gvim
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set guioptions-=m "remove menu bar
set guioptions-=T "remove toolbar"
set guioptions-=r "remove right-hand scroll bar
set guioptions-=L "remove left-hand scroll bar

set guifont=Monospace\ 10.5


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  Vundle
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let vundle_installed = 0
if vundle_installed

set rtp+=/home/void/.vim/bundle/Vundle.vim
call vundle#begin()

" ===== PLUGINS =====
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'valloric/youcompleteme'
Plugin 'tpope/vim-commentary'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'rdnetto/YCM-Generator'
Plugin 'tpope/vim-fugitive'
Plugin 'leafgarland/typescript-vim'
Plugin 'dkprice/vim-easygrep'
Plugin 'Galooshi/vim-import-js'
Plugin 'jlanzarotta/bufexplorer'
Plugin 'ctrlpvim/ctrlp.vim'         " Fuzzy search
Plugin 'mhinz/vim-startify'         " start screen
Plugin 'schickling/vim-bufonly'     " delete all but current buffer
Plugin 'vim-airline/vim-airline'    " Cool bottom bar
Plugin 'shoulgo/vimshell.vim'       " a better shell inside vim

" ==== PLUGIN THEMES ====
Plugin 'jonathanfilip/vim-lucius'
Plugin 'morhetz/gruvbox'
Plugin 'flazz/vim-colorschemes' " one stop shop for vim colorschemes

" ==== PLUGIN SYNTAXES ====
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'hdima/python-syntax'
Plugin 'othree/yajs.vim'
Plugin 'mitsuhiko/vim-jinja'

call vundle#end()            " required by vundle

endif
filetype plugin indent on    " required by vundle
syntax enable                " highlight matching braces

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   NERDTREE
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let NERDTreeIgnore =
    \['\.pyc$', '\.o$', '\.so$', '\.a$', '\.swp', '*\.swp', '\.swo',
    \'\.swn', '\.swm', '[a-zA-Z]*egg[a-zA-Z]*', '[a-zA-Z]*cache[a-zA-Z]*']

let NERDTreeShowHidden=1
let g:NERDTreeWinPos="left"
let g:NERDTreeDirArrows=0
map <C-t> :NERDTreeToggle .<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                YouCompleteMe
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" let g:ycm_show_diagnostics_ui = 0 " disable error check

" fallback configs
let g:ycm_global_ycm_extra_conf =
            \'/home/void/.vim/default_ycm_config/.ycm_extra_conf.py'

let g:ycm_confirm_extra_conf = 0


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Switch pane with Ctrl+[hjkl] (has to be after plugins)
let g:C_Ctrl_j = 'off'
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l



