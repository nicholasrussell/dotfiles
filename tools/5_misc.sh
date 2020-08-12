#!/usr/bin/env bash

log_header2 "Installing miscellaneous tools..."

function install_htop_debian {
    apt_install htop
}

function install_htop {
    if is_macos; then
        idempotent_brew_install htop
    else
        install_htop_debian
    fi
}

install_htop

function install_docker_macos {
    idempotent_brew_install cask docker
    idempotent_brew_install docker-machine
    idempotent_brew_install docker-compose
}

function install_docker {
    if is_macos; then
        install_docker_macos
    fi
}

install_docker

log_header2 "Finished installing miscellaneous tools.\n"
