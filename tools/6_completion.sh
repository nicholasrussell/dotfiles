#!/usr/bin/env bash

log_header2 "Setting up auto completion..."

if is_macos; then
    idempotent_brew_install bash-completion@2

    idempotent_brew_install docker-completion

    # idempotent_brew_install gradle-completion # incompatible with bash@2

    idempotent_brew_install maven-completion

    idempotent_brew_install pip-completion
fi

log_header2 "Finished setting up auto completion."

