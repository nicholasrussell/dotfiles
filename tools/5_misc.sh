#!/usr/bin/env bash

log_header2 "Installing miscellaneous tools..."

function install_fonts {
    if is_macos; then
        idempotent_brew_tap homebrew/cask-fonts
        idempotent_brew_install cask font-source-code-pro --fontdir=/Library/Fonts
    else
        wget -q https://fonts.google.com/download?family=Source%20Code%20Pro -O SourceCodePro.zip
        unzip -qq SourceCodePro.zip -d SourceCodePro
        sudo mv SourceCodePro/*.ttf /usr/local/share/fonts
        fc-cache -f -v
        rm -rf SourceCodePro*
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

function install_babashka {
    idempotent_brew_install borkdude/brew/babashka
}

if is_macos; then
    idempotent_brew_install coreutils
    idempotent_brew_install fd
    idempotent_brew_install cmake
    idempotent_brew_install libvterm
fi

install_fonts
install_htop
install_docker
install_kcat
install_ripgrep
install_babashka

log_header2 "Finished installing miscellaneous tools.\n"
