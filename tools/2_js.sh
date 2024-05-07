#!/usr/bin/env bash

log_header2 "Installing JavaScript tools..."

function install_nvm {
    log_info "Installing nvm..."
    if ! command -v nvm > /dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        PROFILE=/dev/null bash -c 'curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash'
        if is_ubuntu; then
            sudo ln -sf ~/.nvm/bash_completion /etc/bash_completion.d/nvm
        elif is_macos; then
            [[ -r $NVM_DIR/bash_completion ]] && \. $NVM_DIR/bash_completion
        fi
        dotfiles_source
        log_info "Finished installing nvm."
    else
        log_info "nvm is already installed!"
    fi
}

function install_node {
    log_info "Installing node..."
    if ! nvm which node > /dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
        nvm install node
        nvm alias default node
        log_info "Finished installing node."
    else
        log_info "node is already installed!"
    fi
}

function install_lsp {
    log_info "Installing TypeScript LSP..."
    if ! npm ls -g | grep typescript@ > /dev/null 2>&1; then
        npm install -g typescript
    elif [[ -v DOTFILES_TOOLS_FORCE ]]; then
         npm update -g typescript
    fi
    if ! npm ls -g | grep typescript-language-server@ > /dev/null 2>&1; then
        npm install -g typescript-language-server
        log_info "Finished installing TypeScript LSP."
    elif [[ -v DOTFILES_TOOLS_FORCE ]]; then
        npm update -g typescript-language-server
        log_info "Finished updating TypeScript LSP."
    else
        log_info "TypeScript LSP is already installed!"
    fi
}

install_nvm
install_node
install_lsp

log_header2 "Finished installing JavaScript tools.\n"
