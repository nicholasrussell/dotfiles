#!/usr/bin/env bash

function install_pyenv_debian {
    # TODO
}

function install_pyenv_macos {
    brew install pyenv
}

function install_pyenv {
    if is_macos; then
        install_pyenv_macos
    else
        install pyenv_debian
    fi
}
function install_python3 {
    pyenv install 3.8.5
    pyenv global 3.8.5
}

install_pyenv
install_python3
