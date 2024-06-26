#!/usr/bin/env/bash

# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return;;
esac

if [ -f /etc/bashrc ]; then
    source /etc/bashrc
fi

if [ ! -n "${DOTFILES:+1}" ]; then
  export DOTFILES=~/.dotfiles
fi

# Source files in the DOTFILES/source dir
for dotfiles_source_file in "$DOTFILES"/source/*; do
  source "$dotfiles_source_file"
done
unset dotfiles_source_file

if [ -d "$XDG_CONFIG_HOME/work/source" ]; then
  for work_source_file in "$XDG_CONFIG_HOME"/work/source/*; do
    source "$work_source_file"
  done
  unset work_source_file
fi
