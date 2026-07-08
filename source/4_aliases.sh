#!/usr/bin/env bash

if is_macos; then
  alias ls="gls --color=auto --group-directories-first --time-style=long-iso -hv"
else
  alias ls="ls --color=auto --group-directories-first --time-style=long-iso -hv"
fi

function _dotenv() {
  local env_file=".env"
  if [ -f ".env.local" ]; then
    env_file=".env.local"
  elif [ -f ".env.dev" ]; then
    env_file=".env.dev"
  fi
  if [ -f "$env_file" ]; then
    set -a && source "$env_file" && set +a
  else
    echo "No env file found" >&2
    return 1
  fi
}

alias dotenv="_dotenv"
alias kill-emacs="emacsclient -e \"(kill-emacs)\""

ctxlink() {
  if [[ -z "$1" ]]; then
    echo "Usage: ctxlink <directory_name>"
    return 1
  fi

  local target="$HOME/dev/$1"

  if [[ ! -d "$target" ]]; then
    echo "Error: $target is not a directory"
    return 1
  fi

  ln -s "$target" "$1"
}

_ctxlink_completions() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=($(cd ~/dev && compgen -d -- "$cur"))
}

complete -F _ctxlink_completions ctxlink
