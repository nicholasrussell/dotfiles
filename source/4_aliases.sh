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
