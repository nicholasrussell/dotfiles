#!/usr/bin/env bash

log_header2 "Installing editors..."

# Emacs
function install_emacs_debian {
    sudo snap install emacs --classic
    # Install Clojure LSP
    sudo bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)
}

function install_emacs_macos {
    log_info "Installing Emacs..."
    idempotent_brew_install cask emacs
    launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist
    log_info "Finished installing Emacs."
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
    log_info "Configuring vim..."
    if [ ! -d ~/.vim/bundle/Vundle.vim ]; then
        git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
        vim +PluginInstall +qall
        log_info "Installed plugins."
    fi
    log_info "Finished configuring vim."
}

install_vim

# IntelliJ Idea
# TODO

# VSCode
# TODO

log_header2 "Finished installing editors.\n"
