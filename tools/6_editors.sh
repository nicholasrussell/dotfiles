#!/usr/bin/env bash

# Emacs (Spacemacs)
function install_emacs_debian {
    # TODO
}

function install_emacs_macos {
    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-spacemacs-icon
    ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
}

function install_emacs {
    if is_macos; then
        install_emacs_macos
    else
        install_emacs_debian
    fi
}

install_emacs

# vim
function install_vim {
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    vim +PluginInstall +qall
}

install_vim

# IntelliJ Idea
# TODO

# VSCode
# TODO
