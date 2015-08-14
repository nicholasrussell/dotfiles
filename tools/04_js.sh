#!/usr/bin/env bash

# npm
dotfiles_npm_modules=(
    bower
    jshint
    phantomjs
    less
    grunt
    grunt-cli
    yo
    gulp
    semver
)

log_header2 "Installing npm modules..."
for dotfiles_npm_module in ${dotfiles_npm_modules[@]}; do
    if ! sudo npm list -g ${dotfiles_npm_module} > /dev/null 2>&1; then
        log_info "Installing npm module ${dotfiles_npm_module}..."
        sudo npm install -g ${dotfiles_npm_module} > /dev/null 2>&1
    else
        log_info "npm module ${dotfiles_npm_module} already installed"
    fi
done
unset dotfiles_npm_module dotfiles_npm_modules

log_success "Finished installing npm modules\n"
