#!/usr/bin/env bash

log_header2 "Installing editors..."

# Emacs
function install_emacs_debian {
    log_info "Installing Emacs..."
    if [ ! -d /opt/emacs ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        if [[ -v DOTFILES_TOOLS_FORCE ]]; then
            cargo install --force tree-sitter-cli
        else
            cargo install tree-sitter-cli
        fi
        sudo rm -rf /opt/emacs
        sudo git clone --depth 1 --branch emacs-29 https://git.savannah.gnu.org/git/emacs.git /opt/emacs
        sudo chown -R nrussell:nrussell /opt/emacs
        cd /opt/emacs
        export CC="gcc-10"
        export CFLAGS="$CFLAGS -O2"
        export NATIVE_FULL_AOT="1"
        ./autogen.sh
        ./configure --with-native-compilation --with-modules --with-json --with-mailutils --with-tree-sitter
        make -j$(nproc)
        sudo make install
        cd -
        log_info "Finished installing Emacs."
    else
        log_info "Emacs already installed!"
    fi
}

function install_emacs_macos {
    idempotent_brew_install cask emacs
    launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist
}

function install_emacs {
    if is_macos; then
        install_emacs_macos
    else
        install_emacs_debian
    fi
}

install_emacs

# nvim
function install_nvim {
    log_info "Installing nvim..."
    if is_macos; then
        idempotent_brew_installbrew install neovim
    else
        if ! [ -x $(command -v nvim) ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
            add_ppa ppa:neovim-ppa/unstable
            apt_update
            apt_install neovim
            log_info "Finished installing nvim."
        else
            log_info "nvim already installed!"
        fi
    fi
    
    log_info "Configuring nvim..."
    if [ ! -d ~/.local/share/nvim/site ] || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        rm -rf ~/.local/share/nvim/site/pack/packer/start/packer.nvim
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
