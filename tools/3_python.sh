#!/usr/bin/env bash

log_header2 "Installing Python tools..."

function install_pyenv_debian {
    if [ ! -d ~/.pyenv ]; then
        git clone https://github.com/pyenv/pyenv.git ~/.pyenv
        source ~/.bashrc
    fi
}

function install_pyenv {
    if is_macos; then
        idempotent_brew_install pyenv
    else
        install_pyenv_debian
    fi
}
function install_python3 {
    local version='3.11.1'
    log_info "Installing Python 3..."
    if ! pyenv versions | grep $version > /dev/null 2>&1; then
        pyenv install $version
        pyenv global $version
        log_info "Finished installing Python 3."
    else
        log_info "Python 3 already installed!"
    fi
}

install_pyenv
install_python3

log_header2 "Finished installing Python tools."
