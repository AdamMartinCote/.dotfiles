" Neovim Configuration

syntax enable
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smarttab
set smartindent
set autoindent
" filetype indent on

set lazyredraw

" for color problems
set t_ut=
set hlsearch

" set pastetoggle=<F2>

set number
set relativenumber

" automatically reload files modified outside vim
set autoread

" Use standard Perl regex
nnoremap / /\v
vnoremap / /\v

" Make substitution global by default
set gdefault


" Space as mapleader
let mapleader = " "
nnoremap <leader>g :grep -rIn 
nnoremap <leader>I :set list!<cr>
inoremap kj <Esc>
inoremap jk <Esc>
vnoremap ff <Esc>
map ; :FZF<CR>
nnoremap <leader>W :%s/\s\+$//e<CR>

set noswapfile
set nowritebackup
set nobackup

" commentary
autocmd Filetype php setlocal commentstring=//\ %s

map <C-t> :NERDTreeToggle<CR>

" clear search highlight
map <silent> <leader><CR> :noh<CR>

set so=7
" colorscheme monokai
" set background=dark
" set ruler
set hidden

" searching
set ignorecase
set smartcase
set incsearch

" bracket matching
set showmatch
set mat=2

" set foldcolumn=1

" Cycle through buffers
nnoremap <C-n> :bnext<CR>
nnoremap <C-p> :bprevious<CR>

" completion
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" save with usual shortcut
nnoremap <C-s> :write<CR>

" set cursorline


call plug#begin()
" Plug 'roxma/nvim-completion-manager'
" Plug 'SirVer/ultisnips'
" Plug 'honza/vim-snippets'
Plug 'junegunn/fzf'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree'
" Plug 'mattn/emmet-vim'
Plug 'https://github.com/ajh17/VimCompletesMe.git'
" Plug 'Shougo/denite.nvim'
call plug#end()

