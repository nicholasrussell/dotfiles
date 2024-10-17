#!/usr/bin/env bash

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_DIRS="/usr/local/share/:/usr/share/"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

if [ -z "${HOMEBREW_PREFIX}" ]; then
    if is_macos; then
        export HOMEBREW_PREFIX="/opt/homebrew" # /usr/local for macOS on Intel, /opt/homebrew for macOS on Apple Silicon/ARM
    else
        export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
    fi
fi
export CARGO_PATH="$HOME/.cargo"
export JENV_PATH="$HOME/.jenv"
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
export PNPM_HOME="${XDG_DATA_HOME}/pnpm"
export PYENV_ROOT="$HOME/.pyenv"

if [ -d "$HOME/bin" ]; then PATH="$HOME/bin:$PATH"; fi
if [ -d "$PYENV_ROOT" ]; then PATH="$PYENV_ROOT/bin:$PATH"; fi
if [ -d "$JENV_PATH" ]; then PATH="$HOME/.jenv/bin:$PATH"; fi

case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

PATH="$PATH:$DOTFILES/bin"

export PATH
