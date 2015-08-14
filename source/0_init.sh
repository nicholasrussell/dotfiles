## Init

if [ ! -n "${DOTFILES:+1}" ]; then
    export DOTFILES=~/.dotfiles
fi

## OS detection
function is_macos {
    [[ "$OSTYPE" =~ ^darwin ]] || return 1
}

function is_debian {
    [[ -f "/etc/debian_version" ]] || return 1
}

function is_ubuntu {
    [[ "$(cat /etc/issue 2> /dev/null)" =~ Ubuntu ]] || return 1
}

function get_os {
    for os in macos ubuntu; do
        is_$os; [[ $? == ${1:-0} ]] && echo $os
    done
}

## History

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

## Misc

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

## Editor

# Set global editor
export VISUAL="emacsclient -c -a emacs"
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="vim"
