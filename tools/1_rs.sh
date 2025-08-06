#!/usr/bin/env bash

log_header2 "Installing Rust tools..."

function install_rust {
  log_info "Rusting up..."
  if [ ! -d "$CARGO_PATH" ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
    curl --proto '=https' --tlsv1.3 https://sh.rustup.rs -sSf | sh -s -- --no-modify-path
    log_info "Finished Rusting up."
  else
    log_info "Already Rusted up!"
  fi
}

function install_rust_tools {
  log_info "Installing Rust components..."
  rustup component add rust-src >/dev/null 2>&1
  rustup component add rustfmt >/dev/null 2>&1
  rustup component add clippy >/dev/null 2>&1
  log_info "Finished installing Rust components."
}

function install_rust_lsp {
  log_info "Installing Rust LSP..."
  if ! command -v rust-analyzer >/dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
    if is_macos; then
      curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-aarch64-apple-darwin.gz | gunzip -c - | sudo tee /usr/local/bin/rust-analyzer >/dev/null
    else
      curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - | sudo tee /usr/local/bin/rust-analyzer >/dev/null
    fi
    sudo chmod +x /usr/local/bin/rust-analyzer
    log_info "Finished installing Rust LSP."
  else
    log_info "Rust LSP is already installed!"
  fi
}

install_rust
install_rust_tools
install_rust_lsp

log_header2 "Finished installing Rust tools.\n"
