#!/usr/bin/env bash

log_header2 "Installing miscellaneous tools..."

function install_fonts {
    if is_macos; then
        idempotent_brew_tap homebrew/cask-fonts
        idempotent_brew_install cask font-source-code-pro --fontdir=/Library/Fonts
    else
        log_warn "Implement for debian"
    fi
}

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

function install_docker_macos {
    idempotent_brew_install cask docker
    idempotent_brew_install docker-machine
    idempotent_brew_install docker-compose
}

function install_docker {
    if is_macos; then
        install_docker_macos
    else
        log_warn "Implement for debian"
    fi
}

function install_kcat {
    if is_macos; then
        idempotent_brew_install kcat
    else
        log_warn "Implement for debian"
    fi
}

function install_ripgrep {
    if is_macos; then
        idempotent_brew_install ripgrep
    else
        log_warn "Implement for debian"
    fi
}

install_fonts
install_htop
install_docker
install_kcat
install_ripgrep

log_header2 "Finished installing miscellaneous tools.\n"
