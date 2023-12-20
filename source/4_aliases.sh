#!/usr/bin/env bash

if is_macos; then
  alias ls="gls --color=auto --group-directories-first --time-style=long-iso -hv"

  alias load-emacs-daemon="launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
  alias unload-emacs-daemon="launchctl unload -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
else
  alias ls="ls --color=auto --group-directories-first --time-style=long-iso -hv"
fi

function _dotenv() {
    env_file=".env"
    if [ -f ".env.local" ]; then
        env_file=".env.local"
    elif [ -f ".env.dev" ]; then
        env_file=".env.dev"
    fi
    set -a && source $env_file && set +a
}

alias dotenv="_dotenv"
alias kill-emacs="emacsclient -e \"(kill-emacs)\""
