# .bashrc

# If it is a normal terminal, exec tmux
[[ $TERM = "xterm-256color" || $TERM = "xterm-termite" ]] && exec tmux

# Source global definitions
[ -f /etc/bashrc ] && source /etc/bashrc

[ -f ~/.bashrc.alias ] && source ~/.bashrc.alias

[ -f ~/.bashrc.env ] && source ~/.bashrc.env

[ -f ~/.bashrc.prompt ] && source ~/.bashrc.prompt

[ -f ~/.fzf/shell/key-bindings.bash ] && source ~/.fzf/shell/key-bindings.bash
[ -f ~/.fzf/shell/completion.bash ] && source ~/.fzf/shell/completion.bash

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as a single command
shopt -s cmdhist

# Auto-correct misspelled directories
shopt -s dirspell direxpand

# Check the window size after each command and, if necessary,
# Update the values of LINES and COLUMNS.
shopt -s checkwinsize

# The pattern "**" used in a pathname expansion context will
# Match all files and zero or more directories and subdirectories.
shopt -s globstar

# Extended pattern matching features are enabled
shopt -s extglob

# Include filenames beginning with a '.' in the results of pathname expansions
shopt -s dotglob

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Enable programmable completion features
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

function crep {
    grep --color=auto "$@" $(find -L . -type f)
}

function frep {
    grep --color=auto "$@" <(find -L . -type f)
}

function drep {
    grep --color=auto "$@" <(find -L . -type d)
}

function scrop {
    # FIXME don't use --quality > 75, libpng can't handle row overflows when quality increases
    scrot --select --quality 75 "$@.png" -e 'mv $f ~/workspace/notes/algorithms/images/'
}

# Reclaim Ctrl-S and Ctrl-Q used for suspend/resume and use it for modern mapppings
stty -hupcl -ixon -ixoff
stty stop undef
stty susp undef

# Map Ctrl-Q to send EOF
# stty eof ^Q

# Map Alt key to Caps-Lock
# xmodmap -e "clear Lock"
# xmodmap -e "keycode 66 = Alt_L Meta_L"

# Added by travis gem
# [ -f /home/tmrts/.travis/travis.sh ] && source /home/tmrts/.travis/travis.sh

[ -x "$(which dircolors)" ] && eval $(dircolors ~/.dircolors)

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
