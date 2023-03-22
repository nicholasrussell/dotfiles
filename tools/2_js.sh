#!/usr/bin/env bash

log_header2 "Installing JavaScript tools..."

function install_nvm {
    log_info "Installing nvm..."
    if ! command -v nvm > /dev/null 2>&1; then
        curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash
        if is_ubuntu; then
            sudo ln -sf ~/.nvm/bash_completion /etc/bash_completion.d/nvm
        fi
        log_info "Finished installing nvm."
    else
        log_info "nvm is already installed!"
    fi
}

function install_node {
    log_info "Installing node..."
    if ! nvm which node > /dev/null 2>&1; then
        nvm install node
        nvm alias default node
        log_info "Finished installing node."
    else
        log_info "node is already installed!"
    fi
}

function install_lsp {
    log_info "Installing Typescript LSP..."
    npm install -g typescript typescript-language-server
    log_info "Finished installing Typescript LSP."
}

install_nvm
install_node

log_header2 "Finished installing JavaScript tools.\n"
