#!/usr/bin/env bash

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
