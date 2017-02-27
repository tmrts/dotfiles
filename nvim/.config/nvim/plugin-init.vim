" vim: set foldmethod=marker foldmarker={,}:

" Plug Enter {
    call plug#begin('~/.nvim/plugged')
" }

" Cosmetic {
    Plug 'tomasr/molokai'
    Plug 'altercation/vim-colors-solarized'
" }

" General {
    "Plug 'benekastah/neomake'
    Plug 'gosukiwi/vim-atom-dark'
    Plug 'mtth/scratch.vim'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-vinegar'
    Plug 'tpope/vim-surround'
    Plug 'jiangmiao/auto-pairs'
    Plug 'itchyny/lightline.vim'
    Plug 'unblevable/quick-scope'
    Plug 'junegunn/goyo.vim',
    "Plug 'junegunn/vim-easy-align'
    Plug 'godlygeek/tabular'
    Plug 'ryanss/vim-hackernews'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    "Plug 'ervandew/supertab'
    "Plug 'tpope/vim-projectionist'
    "Plug 'tpope/vim-obsession'
    "Plug 'tpope/vim-jdaddy'
    "Plug 'terryma/vim-multiple-cursors'
    "Plug 'bling/vim-bufferline'
    "Plug 'Lokaltog/vim-easymotion'
" }

" Custom Text Objects {
    Plug 'kana/vim-textobj-user'
    Plug 'kana/vim-textobj-indent'

    Plug 'kana/vim-textobj-function'
    Plug 'kamichidu/vim-textobj-function-go',      { 'for': 'go' }
    Plug 'bps/vim-textobj-python',                 { 'for': 'python' }
    Plug 'thinca/vim-textobj-function-javascript', { 'for': 'javascript' }

    Plug 'glts/vim-textobj-comment'

    Plug 'sgur/vim-textobj-parameter'
" }

" Programming {
    Plug 'majutsushi/tagbar'
    Plug 'scrooloose/nerdcommenter'
    "Plug 'airblade/vim-gitgutter'

    "Plug 'scrooloose/syntastic'
" }

" Snippets {
    Plug 'sirver/ultisnips'
" }

" Python {
    "Plug 'klen/python-mode'
    "Plug 'python.vim'
    "Plug 'python_match.vim'
    "Plug 'pythoncomplete'
" }

" Scala {
    Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
" }

" Go {
    Plug 'fatih/vim-go', { 'for': 'go' }
" }

" Haskell {
    "Plug 'travitch/hasksyn'            
    "Plug 'dag/vim2hs'
    "Plug 'Twinside/vim-haskellConceal'
    "Plug 'lukerandall/haskellmode-vim'
    "Plug 'eagletmt/neco-ghc'
    "Plug 'eagletmt/ghcmod-vim'
    "Plug 'Shougo/vimproc'
    "Plug 'adinapoli/cumino'
    "Plug 'bitc/vim-hdevtools'
" }

" HTML {
    Plug 'mattn/emmet-vim',            { 'for': 'html' }
    Plug 'groenewege/vim-less',        { 'for': 'less' }
    Plug 'hail2u/vim-css3-syntax',     { 'for': 'html,css,less,scss' }
    Plug 'gorodinskiy/vim-coloresque', { 'for': 'html,css,less,scss' }
" }

" Javascript {
    Plug 'pangloss/vim-javascript', { 'for': 'js' }
" }

" Different Formats {
    Plug 'LnL7/vim-nix',            { 'for': 'nix' }
    Plug 'elzr/vim-json',           { 'for': 'json' }
    Plug 'stephpy/vim-yaml',        { 'for': 'yaml' }
    Plug 'cespare/vim-toml',        { 'for': 'toml' }
    Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
    Plug 'ekalinin/dockerfile.vim', { 'for': 'Dockerfile' }
" }

" Plug Exit {
    call plug#end()
" }
