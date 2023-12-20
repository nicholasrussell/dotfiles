#!/usr/bin/env bash

log_header2 "Picking self up by bootstraps..."

function is_brew_formula_installed {
    brew list "$1" > /dev/null 2>&1
}

function brew_install {
    local formula="$1"
    if [ "$1" == "cask" ]; then
        formula="$2"
    fi
    if ! is_brew_formula_installed "$formula"; then
        log_info "Installing Brew formula $formula..."
        if [ "$1" == "cask" ]; then
            local temp_args=("$@")
            brew install --cask "${temp_args[@]:1}"
        else
            brew install "$@"
        fi;
        log_info "Finished installing $formula."
    else
        log_info "Brew formula $formula is already installed!"
    fi
}

function install_homebrew {
    if [ ! -d "${HOMEBREW_PREFIX}" ]; then
        log_info "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        log_info "Finished installing Homebrew."
    fi
}

function bootstrap {
    if is_macos; then
        xcode-select --install
        install_homebrew
        brew update > /dev/null 2>&1
        brew_install wget
        brew_install curl
        brew_install git --build_from_source
        brew_install openssl
        brew_install jq
    elif is_debian; then
        sudo apt-get -qq update
        local tools
        tools=(
            apt-transport-https
            wget
            curl
            git
            build-essential
            software-properties-common
            libssl-dev
            gcc
            procps
            file
        )
        sudo apt-get -qq -y install "${tools[@]}" > /dev/null 2>&1
        install_homebrew
        brew update > /dev/null 2>&1
    fi
    if ! command -v bb > /dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        curl -s https://raw.githubusercontent.com/babashka/babashka/master/install > bb-install
        sudo bash bb-install
        rm bb-install
    fi
}

bootstrap

log_header2 "Picked self up by bootstraps.\n"
