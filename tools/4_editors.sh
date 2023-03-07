#!/usr/bin/env bash

log_header2 "Installing editors..."

# Emacs
function install_emacs_debian {
    sudo snap install emacs --edge --classic
    systemctl enable --user emacs
    # Install Clojure LSP
    sudo bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)
}

function install_emacs_macos {
    idempotent_brew_install cask emacs
    launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist
}

function install_emacs {
    log_info "Installing Emacs..."
    if is_macos; then
        install_emacs_macos
    else
        install_emacs_debian
    fi
    log_info "Finished installing Emacs."
}

install_emacs

# nvim
function install_nvim {
    log_info "Installing nvim..."
    if is_macos; then
        idempotent_brew_installbrew install neovim
    else
        add_ppa ppa:neovim-ppa/unstable
        apt_update 
        apt_install neovim
    fi
    log_info "Finished installing nvim."
    
    log_info "Configuring nvim..."
    if [ ! -d ~/.local/share/nvim/site ]; then
        git clone --depth 1 https://github.com/wbthomason/packer.nvim ~/.local/share/nvim/site/pack/packer/start/packer.nvim
    fi
    log_info "Finished configuring nvim."
}

install_nvim

# IntelliJ Idea
# TODO

# VSCode
# TODO

log_header2 "Finished installing editors.\n"
