#!/usr/bin/env bash

log_header2 "Setting up auto completion..."

if is_macos; then
  brew_install bash-completion@2
  brew_install docker-completion
  # brew_install gradle-completion # incompatible with bash@2
  brew_install pip-completion
fi

log_header2 "Finished setting up auto completion.\n"
