#!/usr/bin/env bash

function apt_install() {
  sudo apt-get -qq -y install "$1" >/dev/null 2>&1
}

log_header2 "Installing bash tools..."

if is_debian; then
  apt_install shellcheck
  apt_install shfmt
elif is_macos; then
  brew install shellcheck
  brew install shfmt
fi

# TODO install npm first
if ! command -v bash-language-server >/dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
  npm i -g bash-language-server
fi

log_header2 "Finished installing bash tools.\n"
