#!/usr/bin/env bash

log_header2 "Installing editors..."

# Emacs (Spacemacs)
function install_emacs_debian {
    # TODO
    log_warn "Implement for debian"
}

function install_emacs_macos {
    log_info "Installing Emacs..."
    if ! is_brew_formula_installed emacs-plus; then
        brew tap d12frosted/emacs-plus
        brew install emacs-plus --with-spacemacs-icon
        log_info "Finished installing Emacs."
    else
        log_info "Emacs is already installed!"
    fi
    log_info "Configuring Emacs..."
    if [ ! -e /Applications/Emacs.app ]; then
        ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications
    fi
    if [ ! -d ~/.emacs.d/ ]; then
        git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d/
    fi
    log_info "Finished configuring Emacs."
}

function install_emacs {
    if is_macos; then
        install_emacs_macos
    else
        install_emacs_debian
    fi
}

install_emacs

# vim
function install_vim {
    log_info "Configuring vim..."
    if [ ! -d ~/.vim/bundle/Vundle.vim ]; then
        git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
        vim +PluginInstall +qall
    fi
    log_info "Finished configuring vim."
}

install_vim

# IntelliJ Idea
# TODO

# VSCode
# TODO

log_info "Finished installing editors.\n"
