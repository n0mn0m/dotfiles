set nocompatible              " required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" add all your plugins here (note older versions of Vundle
" used Bundle instead of Plugin)

" Plugin 'tmhedberg/SimplyFold'
Plugin 'vim-scripts/indentpython.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'nvie/vim-flake8'
Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'liuchengxu/space-vim-dark'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'valloric/youcompleteme'
Plugin 'prabirshrestha/async.vim'
Plugin 'prabirshrestha/vim-lsp'
Plugin 'rust-lang/rust.vim'
Plugin 'plasticboy/vim-markdown'
Plugin 'racer-rust/vim-racer'


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

set t_Co=256
set background=dark
let g:space_vim_dark_background = 233
colorscheme space-vim-dark

set guifont=Source\ Code\ Pro
"set encoding=utf-8
set nu

" Split Navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Enable Folding
set foldmethod=indent
set foldlevel=99

" Fold with spacebar
nnoremap <space> za

" Python pep-8 styling
au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" Whitespace
highlight BadWhitespace ctermfg=16 ctermbg=253 guifg=#000000 guibg=#F8F8F0
au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

" vim-flake8
let python_highlight_all=1
syntax on

" Nerdtree
map <C-n> :NERDTreeToggle<CR>

" Language Server
" if you want it to turn on automatically
" let g:LanguageClient_autoStart = 1"
let g:LanguageClient_autoStart = 1
nnoremap <leader>lcs :LanguageClientStart<CR>
noremap <silent> H :call LanguageClient_textDocument_hover()<CR>
noremap <silent> Z :call LanguageClient_textDocument_definition()<CR>
noremap <silent> R :call LanguageClient_textDocument_rename()<CR>
noremap <silent> S :call LanugageClient_textDocument_documentSymbol()<CR>

if executable('rls')
    " rustup component add rls-preview rust-analysis rust-src
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'Cargo.toml'))},
        \ 'whitelist': ['rust'],
        \ })
endif

if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif
