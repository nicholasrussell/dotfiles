#!/usr/bin/env bash

log_header2 "Installing Rust tools..."

function install_cargo {
    if [ ! -d $CARGO_PATH ]; then
        log_info "Installing Cargo..."
        curl --proto '=https' --tlsv1.3 https://sh.rustup.rs -sSf | sh
        log_info "Finished installing Cargo."
    else
        log_info "Cargo is already installed."
    fi
}

install_cargo

log_header2 "Finished installing Rust tools.\n"

