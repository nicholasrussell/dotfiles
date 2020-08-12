#!/usr/bin/env bash

function install_htop_debian {
    apt_install htop
}

function install_htop_macos {
    brew install htop
}

function install_htop {
    if is_macos; then
        install_htop_macos
    else
        install_htop_debian
    fi
}

install_htop
