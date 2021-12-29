#!/usr/bin/env bash

log_header2 "Installing JavaScript tools..."

function install_nvm {
    log_info "Installing nvm..."
    if ! command -v nvm > /dev/null 2>&1; then
        curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
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

install_nvm
install_node

log_header2 "Finished installing JavaScript tools.\n"
