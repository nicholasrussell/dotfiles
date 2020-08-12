#!/usr/bin/env bash

function install_nvm {
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
}

function install_node {
    nvm install node
}

install_nvm
install_node
