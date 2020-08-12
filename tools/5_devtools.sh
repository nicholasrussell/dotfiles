#!/usr/bin/env bash

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
    brew cask install docker
    brew install docker-machine
    brew install docker-compose
}

function install_docker {
    if is_macos; then
        install_docker_macos
    fi
}

install_docker
