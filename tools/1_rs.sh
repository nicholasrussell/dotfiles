#!/usr/bin/env bash

log_header2 "Installing Rust tools..."

function rustup {
    if [ ! -d $CARGO_PATH ]; then
        log_info "Rusting up..."
        curl --proto '=https' --tlsv1.3 https://sh.rustup.rs -sSf | sh
        log_info "Finished Rusting up."
    else
        log_info "Already Rusted up."
    fi
}

function install_rust_tools {
    rustup component add rustfmt
    rustup component add rust-src
    rustup component add clippy

    # lsp
    curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - | sudo tee /usr/local/bin/rust-analyzer > /dev/null
    sudo chmod +x /usr/local/bin/rust-analyzer
}

rustup
install_rust_tools

log_header2 "Finished installing Rust tools.\n"

