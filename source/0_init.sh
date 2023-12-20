#!/usr/bin/env bash

## Init
if [ -z "${DOTFILES:+1}" ]; then
    export DOTFILES=~/.dotfiles
fi

INTERACTIVE=false
case $- in
  *i*) INTERACTIVE=true;;
    *) ;;
esac

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

function is_guix {
    [[ -f "/etc/os-release" && "$(cat /etc/os-release 2> /dev/null)" =~ Guix ]] || return 1
}

function is_nix {
    return 1
}

function get_os {
    for os in macos ubuntu guix nix; do
        is_$os; [[ $? == "${1:-0}" ]] && echo $os
    done
}

function in_emacs {
    [[ -v INSIDE_EMACS ]] || return 1
}

## Theme
test -r ~/.dir_colors && eval "$(dircolors ~/.dir_colors)"

## Bash Setup

### General

# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set lang
LANG=en_US.UTF-8

### Completion

# Perform file completion in a case insensitive fashion
if [ "$INTERACTIVE" = true ]; then bind "set completion-ignore-case on"; fi

# Treat hyphens and underscores as equivalent
if [ "$INTERACTIVE" = true ]; then bind "set completion-map-case on"; fi

# Display matches for ambiguous patterns at first tab press
if [ "$INTERACTIVE" = true ]; then bind "set show-all-if-ambiguous on"; fi

# Immediately add a trailing slash when autocompleting symlinks to directories
if [ "$INTERACTIVE" = true ]; then bind "set mark-symlinked-directories on"; fi

### History

# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
if [ "$INTERACTIVE" = true ]; then bind Space:magic-space; fi

# append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL="erasedups:ignoreboth"

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=500000
HISTFILESIZE=100000

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

## Editor

# Set global editor
export VISUAL="emacsclient -c -a nvim"
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="nvim"

