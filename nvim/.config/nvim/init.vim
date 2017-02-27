" vim: set foldmethod=marker foldmarker={,}:

" Plug-in {
    " Auto-install vim-plug if it's not present
    if empty(glob('~/.config/nvim/autoload/plug.vim'))
        !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall
    endif

    if filereadable(expand('~/.config/nvim/plugin-init.vim'))
        source ~/.config/nvim/plugin-init.vim
    endif
" }

" General {
    scriptencoding utf-8

    filetype plugin on

    filetype indent on

    syntax on

    set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility

    set mouse=""             " Disable the mouse
    set history=1000         " Store a ton of history (default is 20)
    set foldmethod=indent    " Group lines with same indent levels into folds
    set foldlevelstart=99    " Set default fold level to open all folds
    set noswapfile           " No more pesky swap files
    set autoread             " Automatically re-read a modified buffer
    set lazyredraw           " Don't redraw the screen while executing macros
    set ttimeout             " Timeout for vim key prefix search
    set ttimeoutlen=100
    "set complete-=i         " Eliminate searching files for completion and use tags instead
    "set autowrite           " Automatically write a file when leaving a modified buffer
    "set virtualedit=onemore " Allow for cursor beyond last character
    "set hidden              " Allow buffer switching without saving
    "set spell               " Spell checking on

    " Options depreciated by NeoVim
    "set encoding=utf-8       " Set default encoding to UTF-8

    augroup BufferAutoChdir
        autocmd!
        " autochdir option is not exactly the right option for this
        " Some windows don't have a directory ex-mode, messages, etc. so fail silently
        autocmd BufEnter * silent! lchdir %:h
    augroup END

    " Documentation Look-up {
        " Workaround for a current neovim bug with 'K'
        function! LookupDocs()
            let l:cmd = join(['silent', '0read', '!' . &keywordprg, expand('<cword>')], ' ')
            let l:ft = &filetype

            " execute 'vnew ' . l:tmpfile
            vnew Documentation
            augroup CleanupDocumentationBuffer
                autocmd!
                autocmd BufWinLeave <buffer> :bdelete!
            augroup END

            setlocal modifiable noreadonly
            execute l:cmd

            execute 'setfiletype ' . l:ft
            setlocal nomodified nomodifiable readonly 
            setlocal nolist nonumber norelativenumber
            normal gg
        endfunction

        "nnoremap K :call LookupDocs()<bar>wincmd p<cr>

        augroup keywordprg_configuration
            autocmd!
            autocmd FileType bash     setlocal keywordprg=man
            autocmd FileType python   setlocal keywordprg=pydoc3.5
            autocmd FileType vim,help setlocal keywordprg=:help

            " Workaround for vim help
            autocmd FileType vim,help nnoremap <buffer> <silent> K :vertical help <C-r><C-w><bar>wincmd p<cr>
        augroup END
    "}

    " Setting up the directories {
        set backup                      " Enable file back-ups
        set backupdir=~/.nvim/backup//   " File back-up directory
        if !filewritable(&backupdir)
            call mkdir(&backupdir)
        endif

        set undofile                " Use .undo files for the changes made to a file
        set undodir=~/.nvim/undo//   " File undo history directory
        if !filewritable(&undodir)
            call mkdir(&undodir)
        endif

        set undolevels=1000  " Maximum number of changes that can be undone
        set undoreload=10000 " Maximum number lines to save for undo on a buffer reload
    " }

    " NeoVim {
        augroup nvim_terminal
            autocmd!
            " autocmd TermOpen * set bufhidden=hide " Keep terminal buffers open if not closed explicitly
        augroup END

        " BUG: Currently python3 provider has problems in NeoVim
        "let g:python_host_skip_check = 1
        "let g:loaded_python3_provider = 0

        " Enable true color support
        let g:NVIM_TUI_ENABLE_TRUE_COLOR = 1

        set clipboard+=unnamedplus " Tee selection to clipboard register '+' in addition to '*'

        function! IsNotTerminalBuffer()
            return expand("%M") !~? "^term://"
        endfunction

        noremap <silent> <C-p> :FZF<cr>
        noremap <silent> <A-\> :Goyo<cr>

        nnoremap <C-h> <C-w>h
        nnoremap <C-j> <C-w>j
        nnoremap <C-k> <C-w>k
        nnoremap <C-l> <C-w>l

        inoremap <A-f> <C-o>e
        inoremap <A-b> <C-o>b

        " Use default escape map for escaping terminal mode
        tnoremap <C-[> <C-\><C-n>

        " Insert a new line below/above the cursor
        nnoremap <A-s> o<esc>
        nnoremap <A-S> O<esc>

        " Browse the directory of the current file
        nnoremap <silent> <-> :edit %:h<cr>

        " Browse the directory of the current file in a new tab
        nnoremap <silent> <C-A-d> :tabedit %:h<cr>

        " Change directory to the current file's parent dir
        nnoremap <silent> <A-c> :cd %:h<cr>

        " Clear leftover search highlight
        nnoremap <silent> <A-/> :nohlsearch<cr>

        " Align text by using a regular expression
        "nmap <A-t> <Plug>(EasyAlign)
        "vmap <A-t> <Plug>(EasyAlign)
        nnoremap <A-t> :Tabularize /
        vnoremap <A-t> :Tabularize /
    " }
" }

" Vim UI {
    if &term == 'xterm' || &term == 'tmux'
        set t_Co=256    " Enable 256 colors for xterm and tmux
    endif

    set shortmess+=I    " Disable splash screen

    set showcmd         " Show command in last screen line
    set showmode        " Display the current mode

    set cursorline      " Highlight the current line
    set nocursorcolumn  " Don't highlight the current column

    set relativenumber  " Relative line numbers on
    set incsearch       " Find as you type search
    set hlsearch        " Highlight search terms
    set ignorecase      " Case insensitive search
    set smartcase       " Case sensitive when there are upper cases
    set wildmenu        " Show list instead of just completing
    set foldenable      " Auto fold code
" }

" Formatting {
    set nowrap            " Do not wrap long lines
    set autoindent        " Indent at the same level of the previous line
    set expandtab         " Tabs are spaces, not tabs
    set smarttab          " Delete shiftwidth instead of 1 space with backspace
    set tabstop=4         " An indentation every four columns
    set shiftwidth=4      " Use indents of 4 spaces
    set softtabstop=4     " Let backspace delete indent
    set nojoinspaces      " Prevents inserting two spaces after punctuation on a join (J)
    set splitright        " Puts new vsplit windows to the right of the current
    set splitbelow        " Puts new split windows to the bottom of the current
    set formatoptions+=j  " Delete comment characters when joining commented lines

    augroup file_indentation
        autocmd!
        autocmd FileType javascript setlocal shiftwidth=2 softtabstop=2
        autocmd FileType       html setlocal shiftwidth=2 softtabstop=2
        autocmd FileType       scss setlocal shiftwidth=2 softtabstop=2
        autocmd FileType        css setlocal shiftwidth=2 softtabstop=2
        autocmd FileType        xml setlocal shiftwidth=2 softtabstop=2
        autocmd FileType    haskell setlocal shiftwidth=2 softtabstop=2
        autocmd FileType        asm setlocal shiftwidth=8 softtabstop=8
    augroup END

    " Highlighting {
        let g:highlight_long_lines=1        "Highlight lines longer than 80 characters
        let g:highlight_stray_whitespace=1  "Highlight trailing whitespaces in a line

        if exists("g:highlight_long_lines")
            highlight OverLengthLine cterm=bold ctermbg=blue ctermfg=white
            call matchadd("OverLengthLine", "/\%>79v.\+/")
            let g:highlight_long_lines=0
        endif

        if exists("g:long_line_ruler")
            highlight ColorColumn ctermbg=magenta ctermfg=white
            set textwidth=79
            set colorcolumn=+1
        endif

        if exists("g:highlight_stray_whitespace")
            highlight StrayWhitespace ctermbg=blue ctermfg=white
            call matchadd("StrayWhitespace", "/ \+$/")
            let g:highlight_stray_whitespace=0
        endif
    " }
" }

" Key Mappings {
   " TODO(tmrts): function that transposes list, dict items
    let mapleader = ','
    let maplocalleader = ' '

    " Emacs insert mode key bindings
    inoremap <C-a> <C-o>^
    inoremap <C-e> <C-o>$
    inoremap <C-k> <C-o>D
    inoremap <A-f> <C-o>w
    inoremap <A-b> <C-o>b

    colorscheme molokai
    let g:cschemes = ['atom-dark-256', 'molokai']
    let g:curr_alt = 0
 
    function! CycleColorScheme()
        execute "colorscheme" . " " . g:cschemes[g:curr_alt]

        let g:curr_alt = (g:curr_alt + 1) % len(g:cschemes)
    endfunction

    " Cycle color schemes
    nnoremap <C-A-\> :call CycleColorScheme()<cr>

    " Source my .vimrc
    nnoremap <C-A-s> :source $MYVIMRC<cr>

    " Wrapped lines goes down/up to next row, rather than next line in file.
    noremap j gj
    noremap k gk

    " Faster quick-fix item cycling
    noremap <silent> [q :cnext<cr>
    noremap <silent> ]q :cprevious<cr>

    " Faster cursor position cycling
    " noremap <silent> <A-n> <C-i>
    " noremap <silent> <A-p> <C-o>

    " Faster tab cycling
    noremap <silent> <A-l> :tabnext<cr>
    noremap <silent> <A-h> :tabprevious<cr>

    " TODO: Add file history
    " Faster file cycling
    noremap <silent> [f <C-^>
    noremap <silent> ]f <C-^>

    " Faster arg cycling
    noremap <silent> [a :next<cr>
    noremap <silent> ]a :previous<cr>

    " Faster tag cycling
    noremap <silent> [t :tnext<cr>
    noremap <silent> ]t :tprevious<cr>

    " Faster buffer cycling
    noremap <silent> [b :bnext<cr>
    noremap <silent> ]b :bprevious<cr>

    " TODO(tmrts): Second press in the same buffer should do 'q!'
    " Close current window
    nnoremap <C-q> <C-w>q
    vnoremap <C-q> <C-w>q
    inoremap <C-q> <C-o><C-w>q

    " Redraw line at center of window
    noremap <space><space> zz

    " Idiomatic yank
    map Y y$

    " Save file if modified
    nnoremap <leader>z :update<cr>

    " Save file if modified, and exit
    nnoremap <leader>x :exit<cr>

    nnoremap <leader>t :TagbarToggle<cr>

    " Starts a new undo entry for change operation
    inoremap <C-w> <C-g>u<C-w>
    inoremap <C-u> <C-g>u<C-u>
    inoremap <C-k> <C-g>u<C-o>d$

    " Insert mode undo
    inoremap <A-u> <C-g>u<esc>ui

    " map ,t :update\|:silent !echo \'printf \\\'\033c\'; py.test -v src/%' > test-commands<cr>

    " Repeat the latest search and center the result
    nnoremap n nzz
    nnoremap N Nzz

    " Open a terminal in a vertical split
    nnoremap <silent> <C-w>t :vsplit term://$SHELL<cr>

    " TODO(tmrts): Autocmd <A-i> so that it calls interpreters py, haskell, node, shell
    "
    " TODO(tmrts): Mark terminal buffer
    " TODO(tmrts): Send input to marked terminal buffer
    " TODO(tmrts): Build a plugin

    " Open the current file's parent directory on a vertical split
    "nnoremap <silent> <C-w>d :vsplit %:h<cr>

    " Print the ascii value of the character under the cursor in decimal
    nnoremap gpa ga
    " Print the hex value of the character under the cursor
    nnoremap gph g8

    " Scroll up/down previous window
    nnoremap <A-}> <C-w>p}<C-w>p
    nnoremap <A-{> <C-w>p{<C-w>p

    " Close previous window
    nnoremap <C-w>q <C-w>p<C-w>c

    " Go Mappings {
        function! GetGoPkgDocumentation()
            let pkg = input('Please enter a package name: ')

            " BUG:?GoDoc throws non-sense errors sometimes even though the look-up works
            silent execute 'GoDoc' . ' ' . l:pkg
        endfunction

        augroup go_mappings
        autocmd!
        autocmd FileType go nnoremap <buffer> <c-w>d <Plug>(go-def-vertical)
        autocmd FileType go nnoremap <buffer> <localleader>d :call GetGoPkgDocumentation()<cr>

        " go coverage
        " implements
        " tagbar
        " channel
        " referees
        autocmd FileType go nnoremap <buffer> <localleader>b :update\|:GoBuild<cr>
        autocmd FileType go nnoremap <buffer> <localleader>f :update\|:GoTestFunc!<cr>
        autocmd FileType go nnoremap <buffer> <localleader>l :update\|:GoMetaLint<cr>
        autocmd FileType go nnoremap <buffer> <localleader>r :update\|:GoRun<cr>
        autocmd FileType go nnoremap <buffer> <localleader>t :update\|:GoTest!<cr>
        augroup END

        " vim-go highlighting {
            let g:go_highlight_functions = 1
            let g:go_highlight_methods = 1
            let g:go_highlight_fields = 1
            let g:go_highlight_structs = 1
            let g:go_highlight_interfaces = 1
            let g:go_highlight_operators = 1
            let g:go_highlight_build_constraints = 1
        " }
    " }

    " Only show quick-scope highlights after f/F/t/T is pressed
    function! Quick_scope_selective(movement)
        let needs_disabling = 0
        if !g:qs_enable
            QuickScopeToggle
            redraw
            let needs_disabling = 1
        endif

        let letter = nr2char(getchar())

        if needs_disabling
            QuickScopeToggle
        endif

        return a:movement . letter
    endfunction

    let g:qs_enable = 0

    for i in  [ 'f', 'F', 't', 'T' ]
        execute 'noremap <expr> <silent>' . i . " Quick_scope_selective('". i . "')"
    endfor

    highlight QuickScopePrimary ctermfg=yellow
    highlight QuickScopeSecondary ctermfg=red

    " Utilities {
        function! RenameCurrentFile()
            let current_filename = expand('%:p')
            let new_filename = expand('%:h') . '/' . input('Enter the new file name: ')

            call rename(current_filename, new_filename)

            execute 'edit ' . new_filename
        endfunction

        nnoremap <A-r> :call RenameCurrentFile()<cr>

        function! ForceLastCommand()
            let l:last_command = split(getreg(':'))

            let l:force_command = [l:last_command[0] . '!'] + l:last_command[1:]

            execute join(l:force_command, ' ')
        endfunction

        " Force execute the last issued command
        nnoremap <A-!> :call ForceLastCommand()<cr>

        " J joins the lines, K splits them
        " nnoremap K i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>
    " }
" }

" Abbreviations {

    iabbrev re return
    iabbrev pk package
    iabbrev fn function
    iabbrev lh localhost

    iabbrev h/ http://
    iabbrev hs/ https://
    iabbrev f/ file://
    iabbrev w/ ws://

    iabbrev hl http://localhost:

" }

" Plugins {

"let g:tagbar_type_tex = {
            "\ 'ctagstype' : 'latex',
            "\ 'kinds'     : [
            "\ 's:sections',
            "\ 'g:graphics:1',
            "\ 'l:labels:1',
            "\ 'r:refs:1',
            "\ 'p:pagerefs:1'
            "\ ],
            "\ 'sort'    : 0
            "\ }

"let g:tagbar_type_nc = {
            "\ 'ctagstype' : 'nesc',
            "\ 'kinds'     : [
            "\ 'd:definition',
            "\ 'f:function',
            "\ 'c:command',
            "\ 'a:task',
            "\ 'e:event'
            "\ ],
            "\ }

"let g:tagbar_type_go = {
    "\ 'ctagstype' : 'go',
    "\ 'kinds'     : [
        "\ 'p:package',
        "\ 'i:imports:1',
        "\ 'c:constants',
        "\ 'v:variables',
        "\ 't:types',
        "\ 'n:interfaces',
        "\ 'w:fields',
        "\ 'e:embedded',
        "\ 'm:methods',
        "\ 'r:constructor',
        "\ 'f:functions'
    "\ ],
    "\ 'sro' : '.',
    "\ 'kind2scope' : {
        "\ 't' : 'ctype',
        "\ 'n' : 'ntype'
    "\ },
    "\ 'scope2kind' : {
        "\ 'ctype' : 't',
        "\ 'ntype' : 'n'
    "\ },
    "\ 'ctagsbin'  : 'gotags',
    "\ 'ctagsargs' : '-sort -silent'
"\ }
    " Misc {
        let g:NERDShutUp=1
        let b:match_ignorecase = 1
    " }

    " Session List {
        set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize

        if exists('g:mac')
            nmap <leader>sl :SessionList<CR>
            nmap <leader>ss :SessionSave<CR>
            nmap <leader>sc :SessionClose<CR>
        endif
    " }

    " PyMode {
        if exists('g:pymode')
            let g:pymode_lint_checker = 'pyflakes'
            let g:pymode_utils_whitespaces = 0
            let g:pymode_options = 0
        endif
    " }

    " LightLine {
        " set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

        set noshowmode   " Don't Display the current mode since light-line is active.
        set laststatus=2 " Show status line even when only one window is shown.

        let g:lightline = { 
                    \   'colorscheme': 'solarized',
                    \   'component': {
                    \       'fileencoding': '%{&fileencoding == "utf-8" ? "": "utf-8"}',
                    \       'fileformat': '%{&fileformat == "unix" ? "": "unix"}',
                    \       'filename': '%{IsNotTerminalBuffer() ? (expand("%M") != "" ? expand("%M") : "[Temporary]") : "" }',
                    \       'readonly': '%{&readonly?"⭤":""}',
                    \       'modified': '%{IsNotTerminalBuffer() && &modified ? "+" : ""}',
                    \       'close': '',
                    \   },
                    \   'component_visible_condition': {
                    \       'fileencoding': '(&fileencoding!=?"utf-8")',
                    \       'fileformat': '(&fileformat!=?"unix")',
                    \       'modified': '(IsNotTerminalBuffer() && &modified)',
                    \   },
                    \ }
    " }

    " Ctrl-P {
        set wildignore+=~
        set wildignore+=.hg,.git,.svn                   " Version control
        set wildignore+=__pycache__,migrations,*.pyc    " Python specific cache files and such
        set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg  " Binary images
    " }

    " UltiSnips {
        let g:UltiSnipsEditSplit = 'vertical'

        " let g:UltiSnipsSnippetsDir = '~/.nvim/ultisnips'
        " let g:UltiSnipsSnippetDirectories = [ g:UltiSnipsSnippetsDir ]

        " let g:UltiSnipsJumpForwards = '<tab>'
        " let g:UltiSnipsJumpBackwards = '<S-tab>'
    " }
" }

" Speed Optimizations {
    syntax sync minlines=1024

    " let g:loaded_matchparen=1
" }

" GUI Settings {
    if has('gui_running')
        " set mouse=c       " Change mouse mode to command-line mode
        set guioptions=M  " Remove toolbars
    endif
" }

function! PythonFile2TestFile()
    if expand('%:t') =~ '_test' || isdirectory(expand('%:t'))
        !echo '%:t' > test-commands
    else
        !echo '%:t:r_test.%:e' > test-commands
    endif
endfunction

" map ,t :update\|:silent !echo \'py.test $(find . -name %:t:r)\' > test-commands<cr>
" map ,t :update\|:silent call PythonFile2TestFile()<cr>

" Bug: Tmux doesn't re-draw vim if redraw command is not given.
" map <silent> ,t :update<bar>:!echo %:p > $TEST_PIPE<cr>:redr!<cr>

let g:go_fmt_command = 'goimports'

" noremap ,go :update\|:!printf '\033c' && go test -v -c -o ${GOBIN}/test && docker run --rm -v ${GOBIN}:/gobin centos /bin/sh -c 'gobin/test'<cr>
