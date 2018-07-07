" Neovim Configuration

let g:loaded_matchparen = 1

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smarttab
set smartindent
set autoindent
filetype indent on
autocmd FileType html setlocal shiftwidth=2 tabstop=2

" Syntax
syntax enable
syntax sync minlines=100
syntax sync maxlines=240
set synmaxcol=800

" Disable relativenumber for php files only :'(
autocmd Filetype php setlocal nocursorline norelativenumber

" fold
set foldmethod=indent   
set foldnestmax=10
set nofoldenable
set foldlevel=2

set lazyredraw

" for color problems
" set t_ut=
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
let g:mapleader = " "
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
" autocmd Filetype php setlocal commentstring=//\ %s

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
" set showmatch
" set mat=2

" set foldcolumn=1
" Cycle through buffers
nnoremap <C-n> :bnext<CR>
nnoremap <C-p> :bprevious<CR>

" save with usual shortcut
nnoremap <C-s> :write<CR>

" select all
nnoremap <C-h> 1GVG
nnoremap <C-g> :UndotreeToggle<CR>

" make error label readable
hi SpellBad ctermfg=016 ctermbg=160 guifg=#000000 guibg=#700000
hi SpellCap ctermfg=016 ctermbg=160 guifg=#000000 guibg=#700000


autocmd FileType python nnoremap <mapleader>= :0,$!yapf<CR>
" set cursorline

call plug#begin()
" Plug 'roxma/nvim-completion-manager'
" Plug 'SirVer/ultisnips'
" Plug 'honza/vim-snippets'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree'
Plug 'mattn/emmet-vim'
Plug 'https://github.com/ajh17/VimCompletesMe.git'
" Plug 'Shougo/denite.nvim'
Plug 'StanAngeloff/php.vim'
Plug 'w0rp/ale'
Plug 'syngan/vim-vimlint'
Plug 'ynkdir/vim-vimlparser'
Plug 'nathanaelkane/vim-indent-guides'
" Plug 'StanAngeloff/php.vim'
Plug 'ap/vim-css-color'
Plug 'tpope/vim-surround'
Plug 'mbbill/undotree'
Plug 'google/yapf'
call plug#end()

