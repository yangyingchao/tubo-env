"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Author: YangYingchao
" Email: yangyingchao@gmail.com
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Update Log:
" V1:   Someday in 2009: Created and modified;
" V2:   2009,05,25: Update a shotkey to hide #ifdef Macro.
" V3:   2009,06,03: Update a function to control NERDTree with sigle
"           key; Update coding standadrd for Python.
" V3:   2009,06,04: Bind <F5> to run Bash, <F6> to run python.
" V4:   2009,06,05: Added some platform specific settings: fonts,
"           backups, and so on.
" V5:   2009,07,03: Added key settings related to subversion.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"*******************************************
" Part One: General A list of 'set' command.
"*******************************************
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" A varible to judge whether it is Windows OS or not
let s:MSWIN =   has("win16") || has("win32") || has("win64")

"Set mapleader
let mapleader = ","
let g:mapleader = ","
let g:C_MapLeader  = ','

"Get out of VI's compatible mode..
set nocompatible

"Sets how many lines of history VIM har to remember
set history=100

"Auto read a file when it has been changed by other program
set autoread

"Enable filetype plugin
filetype plugin on
filetype indent on
syntax on

"Favorite filetypes
set ffs=unix,dos

"Turn on WiLd menu
set wildmenu
"Always show current position
set ruler
"The commandbar is 1 high
set cmdheight=1
"Don't show line number
set nonu
"Do not redraw, when running macros.. lazyredraw
set lz
"Change buffer - without saving
set hid
"Set backspace
set backspace=eol,start,indent
"Bbackspace and cursor keys wrap to
"set whichwrap+=<,>,h,l
set whichwrap+=<,>
"Increase search mode
set incsearch
"Highlight search things
set hlsearch
"Set magic on
set magic
"No sound on errors.
set noerrorbells
set novisualbell
"show matching bracets
set showmatch

let g:vikiUseParentSuffix=1

"Always hide the statusline
set laststatus=2

"Tab configuration
set shiftwidth=4
set tabstop=4
set softtabstop=4
set expandtab

"Disable winaltkeys to map Alt.
set winaltkeys=no

"set cinoptions={0,1s,t0,n-2,p2s,(03s,=.5s,>1s,=1s,:1s
set cinoptions=:0,g0,t0,(0

"Restore cursor to file position in previous editing session
set viminfo='20,\"50,:20,n~/.viminfo
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif

augroup filetypedetect
au! BufRead,BufNewFile *.viki setfiletype viki
augroup END

" Session options
set sessionoptions-=curdir
set sessionoptions+=sesdir

" Files and backups
set nobackup
set writebackup
set backupcopy=auto
set backupext=.bak

" Text options
set smarttab
set tw=78

"Auto indent
set autoindent
set smartindent

"Wrap lines
set wrap

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme desert

" GUI settings
if has("gui_running")
set guioptions-=T
set guioptions-=L
set guioptions-=r
set guioptions-=m
set mouse=a

nmap <leader>p "+gp
nmap <leader>y "+gy

imap <leader>p "+gp
imap <leader>y "+gy

let s:MSWIN =   has("win16") || has("win32") || has("win64")
if s:MSWIN
set guifont=Monaco\ 12
else
set guifont=Monaco\ 12
endif
endif

"Some nice mapping to switch syntax (useful if one mixes different languages in one file)
autocmd BufEnter * :syntax sync fromstart

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"*******************************************
" Part Two: Configurations about plugins
"*******************************************
" Required Plugins:
" C-support: Also known as c.vim, a powerful tool to write C/C++" code.
" NERDTree: A tool to explore local and network files and directories.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" cscope setting
if has("cscope")
set csprg=/usr/bin/cscope
set csto=1
set cst
set nocsverb
" add any database in current directory
if filereadable("cscope.out")
cs add cscope.out
endif
set csverb
endif

nmap <C-@>s :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>g :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>c :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>t :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>e :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-@>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-@>d :cs find d <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>h :cs help <CR>

" Taglist
nmap <silent><F8>       :Tlist<CR>
imap <silent><F8>       :Tlist<CR>
let Tlist_Use_Right_Window = 1
let Tlist_Show_One_File = 1
let Tlist_WinWidth = 27
let Tlist_Exit_OnlyWindow = 1

" C-Support
"Mapleader was setted as ',' in previous setting.

" NERDTree function
let s:Dir_F = 0
func! NERDTree_OC()
exe "normal mz"
if s:Dir_F
let s:Dir_F = 0
exe "1"
exe "normal mz"
exe "NERDTreeClose"
"exe ":wincmd j"
else
let s:Dir_F = 1
exe "normal mz"
exe "NERDTree"
endif
endfunc

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"*******************************************
" Part Three: Configurations about filetypes.
"*******************************************
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" C/C++
autocmd FileType c,cpp  map <buffer> <leader><space> :make<cr>
autocmd FileType c,cpp  setlocal cindent
\ shiftwidth=8
\ tabstop=8
\ softtabstop=8

" Highlight space errors in C/C++ source files (Vim tip #935)
if $VIM_HATE_SPACE_ERRORS != '0'
let c_space_errors=1
endif

" HTML related
let xml_use_xhtml = 1

"To HTML
let html_use_css = 1
let html_number_lines = 0
let use_xhtml = 1

"""""""""""""""""""""""""""""""
" Vim section
"""""""""""""""""""""""""""""""
autocmd FileType vim set nofen
autocmd FileType vim map <buffer> <leader><space> :w!<cr>:source %<cr>

""""""""""""""""""""""""""""""
" HTML
"""""""""""""""""""""""""""""""
au FileType html set ft=xml
au FileType html set syntax=html

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text file: using syntax plain to show all text files.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
au BufRead,BufNewFile rfc*.* set syntax=plain
au BufRead,BufNewFile RFC*.* set syntax=plain
au BufRead,BufNewFile *.txt set syntax=plain
au BufRead,BufNewFile draft* set syntax=plain
au BufRead,BufNewFile DRAFT* set syntax=plain
au BufRead,BufNewFile readme set syntax=plain
au BufRead,BufNewFile README set syntax=plain
au BufRead,BufNewFile *.log set syntax=log

au BufRead,BufNewFile *.srt setl syntax=sts sts=2 dictionary=/home/yyc/.vim/dictionary/sts.dic
au BufRead,BufNewFile *.srt nmap <F5> :call Run_STS()<cr>
au BufRead,BufNewFile *.srt nmap <leader>lo :call Show_log()<cr>
au BufNewFile *srt call SRT_Insert()
au BufRead,BufNewFile *.SRT set syntax=sts
""""""""""""""""""""""""""""""
" Tex
"""""""""""""""""""""""""""""""
au BufRead,BufNewFile *.tex setl dictionary=/home/yyc/.vim/dictionary/tex.dic

""""""""""""""""""""""""""""""
" Python
"""""""""""""""""""""""""""""""
au BufRead,BufNewFile *.py   setlocal tabstop=4 softtabstop=4 shiftwidth=4
\ smarttab expandtab smarttab tw=72
\ dictionary=/home/yyc/.vim/dictionary/python.dic
\ syntax=python3
au BufRead,BufNewFile *.py nmap <leader>rr :!python %<cr>
au BufRead,BufNewFile *.py nmap <leader>pr :!python %<cr>
au BufRead,BufNewFile *.sh nmap <leader>rr :!bash %<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"*******************************************
" Part For: Key Mappings.
"*******************************************
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Keybind of F2 - F12
nmap <F2> :call NERDTree_OC()<cr>
" <F3> Was bind to a plugin named qbuf, which was used to display buffers.
if s:MSWIN
nmap <F6>   :!cmd<cr>
else
nmap <F6>   :!bash<cr>
endif
nmap <F7>   :!python<cr>
" <F8> Was bind by Taglist
" <F9> Was bind by C.vim to show errors when compile C code.
" <F11> was used by screen: move to the left window.
" <F12> was used by screen: move to the right window.
" Others: Not Finished yet.

"Case Sensitive or insensitive
map <silent> <leader>ic :set ic<cr>
map <silent> <leader>nic :set noic<cr>

"Fast saving
nmap <silent> <leader>ww :w<cr>
imap <silent> ,ww <Esc>:w<cr>
nmap <silent> <leader>wf :w!<cr>
nmap <silent> <leader>wa :wa<cr>

"Fast quiting
nmap <silent> <leader>xx :x<cr>
nmap <silent> <leader>qq :x<cr>
nmap <silent> <leader>qa :xa<cr>
imap <silent> ,xx <Esc>:x<cr>
imap <silent> ,qq <Esc>:x<cr>
nmap <silent> <leader>xa :xa<cr>
nmap <silent> <leader>xa :call Myquit()<cr>
vmap <silent> <leader>xa :call Myquit()<cr>
imap <silent> <leader>xa <Esc>:call Myquit()<cr>

" Windows configuration
nmap <silent> <leader>w2 :split<cr>
nmap <silent> <leader>w3 :vs<cr>
nmap <silent> <leader>clo :close<cr>
nmap <silent> <leader>ol :only<cr>

"Buffer -- goto Next and Previous
map <leader>bn :bn<cr>
map <leader>bp :bp<cr>
map <C-k> :bn<cr>
map <C-j> :bp<cr>
map <C-l> zz

" About Blank lines: delete or merge.
nmap <leader>db :g/^$/,/./j<cr>:w<cr>
nmap <leader>rb :call ReduceBlankLines()<cr>:w<cr>

" Sort the
nmap <leader>st :%sort<cr>
vmap <leader>st :sort<cr>

nmap <leader>mk : call Maketags()<cr>

" Enable of Disable Spell Check
nmap <leader>sp :set spell<cr>
nmap <leader>nsp :set nospell<cr>

" Delete Repeated Lines and sort.
nmap <leader>dr :%sort u
vmap <leader>dr :sort u

" Shotkey to change tab configuration
nmap <leader>t8 :set shiftwidth=8 softtabstop=8 tabstop=8<cr>
nmap <leader>t4 :set shiftwidth=4 softtabstop=4 tabstop=4<cr>
nmap <leader>et :set expandtab<cr>
nmap <leader>nt :set noexpandtab<cr>

" Shortkey to change Fileformats
nmap <leader>fd :se ff=dos<cr>
nmap <leader>fu :se ff=unix<cr>

" Delete the superflous white sapces
nmap <silent> <leader>ws :call DeleteTrailingWS()<cr>:w<cr>
imap <silent> <leader>ws <Esc>:call DeleteTrailingWS()<cr>:w<cr>

"Fast reloading and editing of .vimrc
map <silent> <leader>sr :source ~/.vimrc<cr>
map <silent> <leader>ee :e ~/.vimrc<cr>
map <silent> <leader>sudo :w !sudo tee %<cr>

" Fast grep
nmap <silent> <leader>lv :lv /<c-r>=expand("<cword>")<cr>/ %<cr>:lw<cr>
vmap <silent> <leader>lv :lv /<c-r>=<sid>GetVisualSelection()<cr>/ %<cr>:lw<cr>

" Mics: Other usefull settings.
vmap <leader>ml :s/\n/ /g<cr>
"Switch to current dir
map <silent> <leader>cd :cd %:p:h<cr>
"No heigh color
nmap <C-h> :noh<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"*******************************************
" Part Five: Functions called by obove key maps.
"*******************************************
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Merge Blank lines into one.
func! ReduceBlankLines()
exe "normal mz"
g/^$/,/./-j
nohl
exe "normal `z"
endfunc

" Fast grep
function! s:GetVisualSelection()
let save_a = @a
silent normal! gv"ay
let v = @a
let @a = save_a
let var = escape(v, '\\/.$*')
return var
endfunction

" Delete supefoulus white spaces.
func! DeleteTrailingWS()
exe "normal mz"
%s/\s\+$//ge
nohl
exe "normal `z"
endfunc

" Function to quit program in all modes.
func! Myquit()
exe "normal mz"
xa
endfunc

function! SRT_Insert()
exe "goto 1"
exe "insert"
// DESC_BEGIN
// DESC_END
endfunction

function! Maketags()
exe ": !/home/yyc/tools/bin/mktags"
exe "cs r"
endfunction

" Function to run my script(for isct project).
function! Run_STS()
let fscript=expand('%:p')
let cmd='! cd /home/yyc/work/sts/iscsi/src/python; ./run.py '.fscript
exe cmd
endfunction

function! Show_log()
let fn=expand('%:p:t:r').'.log'
let cmd='e /usr/local/inventec/sts/iscsi/log/'.fn
exe cmd
endfunction
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"*******************************************
" Part Six: Frequently used Abbrevs
"*******************************************
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

abbr Yang YangYingchao,
abbr yangying yangyingchao@gmail.com

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
