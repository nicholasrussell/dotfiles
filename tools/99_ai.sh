#!/usr/bin/env bash

log_header2 "Installing AI tools..."

function install_ollama {
  if is_macos; then
    brew_install ollama
  fi
}

function install_goose {
  if is_macos; then
    brew_install block-goose-cli
    brew_install cask block-goose
  fi
}

function install_cursor_agent {
  if ! command -v agent &>/dev/null; then
    curl https://cursor.com/install -fsS | bash
  fi
}

function install_pi_dev {
  if ! pnpm list -g @earendil-works/pi-coding-agent &>/dev/null; then
    pnpm add -g --ignore-scripts @earendil-works/pi-coding-agent
  fi
}

install_ollama
install_goose
# install_cursor_agent
install_pi_dev

# ollama server
# ollama pull gpt-oss:20b
# goose configure

log_header2 "Finished installing AI tools"

