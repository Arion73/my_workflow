set nocompatible              " required
filetype on                  " required

set hlsearch incsearch  "高亮搜索项
set backspace=2         "可随时用退格键删除
set autoindent          "自动缩排
set ruler               "显示最后一行的状态
set showmode            "左下角那一行的状态
set number              "显示行号
set guioptions-=r 		" 隐藏滚动条"
set guioptions-=L
set guioptions-=b
set showmatch   		"显示匹配的括号"
set lazyredraw
set synmaxcol=200
syntax on 
"hi LineNr ctermbg=NONE
"快速编辑 .vimrc
nnoremap <leader>ev :split $MYVIMRC<CR>

"快速由insert模式退出到normal模式
inoremap jk <esc>

" set the runtime path to include Vundle and initialize
"set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#rc()

" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
"Plugin 'gmarik/Vundle.vim'

" add all your plugins here (note older versions of Vundle
" used rundle bnstead of Plugin)

" colorscheme
set bg=dark             "显示底色
set t_Co=256
let g:solarized_termtrans = 1
colorscheme solarized 
"another colorscheme
"let g:gruvbox_contrast_dark = "hard"
"colorscheme gruvbox

" notedown plugin for vim to edit *.ipynb file
let g:notedown_enable=1
let g:notedown_code_match='all'

" Tree
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_browse_split = 3
let g:netrw_winsize = 25


" Enable folding
set foldmethod=indent
set foldlevel=99
" Enable folding with the spacebar
"nnoremap <space> za

" PEP8 indentation
au BufNewFile,BufRead *.py set tabstop=4 softtabstop=4 shiftwidth=4 textwidth=79 expandtab autoindent fileformat=unix
" indentation for other filetypes
au BufNewFile,BufRead *.js,*.html,*.css set tabstop=2 softtabstop=2 shiftwidth=2

" Flagging unnecessary whitespace
au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match Error /\s\+$/

"Finding files
set path+=**
" Display all matching files when tab complete
set wildmenu

" 调用终端运行python文件
au BufRead *.py map <buffer> <leader>b :w<CR>:!python3 % <CR>

" 分割布局相关
set splitbelow
set splitright
"""快捷键，ctrl+l切换到右边布局，ctrl+h切换到左边布局
"""ctrl+k切换到上面布局，ctrl+j切换到下面布局
""nnoremap <C-J> <C-W><C-J>
""nnoremap <C-K> <C-W><C-K>
""nnoremap <C-L> <C-W><C-L>
""nnoremap <C-H> <C-W><C-H>

" UTF8 support
set encoding=utf-8

" Templates files
autocmd BufNewFile *.py 0r ~/.vim/template/sample.py
autocmd BufNewFile *.js 0r ~/.vim/template/sample.js
autocmd BufNewFile *.css 0r ~/.vim/template/sample.css
autocmd BufNewFile *.html 0r ~/.vim/template/sample.html

" system clipboard
set clipboard=unnamed

" 缩进指示线
"Plugin 'Yggdroot/indentLine'
"let g:indentLine_char='┆'
"let g:indentLine_enabled = 1

" 状态栏
"Plugin 'vim-airline/vim-airline'
"Plugin 'vim-airline/vim-airline-themes'
"AirlineTheme"
"let g:airline_theme='powerlineish'

"python开发模块
"Plugin 'klen/python-mode'
"let g:pymode = 1
"let g:pymode_python = 'python3'
"let g:pymode_run_bind = '<leader>r'
"let g:pymode_breakpoint_bind = '<leader>b'

hi LineNr ctermbg=NONE

" All of your Plugins must be added before the following line
"call vundle#end()            " required
filetype plugin indent on    " required
