#!/usr/bin/env bash

function log_header1 { echo -e "\r\033[1;36m$@\033[0m"; }
function log_header2 { echo -e "\r\033[1;34m$@\033[0m"; }
function log_info { echo -e "\r\033[0;34m$@\033[0m"; }
function log_warn { echo -e "\r\033[1;33m$@\033[0m"; }
function log_success { echo -e "\r\033[1;32m$@\033[0m"; }
function log_error { echo -e "\r\033[1;31m$@\033[0m"; }

## dotfiles functions
function dotfiles_link {
    if [ -e $DOTFILES/backup/link ]; then
        rm -r $DOTFILES/backup/link
    fi
    mkdir -p $DOTFILES/backup/link

    log_header1 "Linking files..."
    shopt -s dotglob
    shopt -s globstar
    local dotfiles_link
    for dotfiles_link in $DOTFILES/link/**/*; do
        if [ -f $dotfiles_link ]; then
            dotfiles_link=$(echo $dotfiles_link | sed -e 's,'"$DOTFILES"/link/',,')
            if [ -e ~/${dotfiles_link} ]; then
                log_warn "Backing up ~/${dotfiles_link} to $DOTFILES/backup/link..."
                cp -L ~/${dotfiles_link} $DOTFILES/backup/link
            fi
            file_dir="$(dirname ~/${dotfiles_link})"
            if [ ! -d $file_dir ]; then
                mkdir -p $file_dir
            fi
            log_info "Linking ~/${dotfiles_link} to $DOTFILES/link/${dotfiles_link}..."
            ln -sf $DOTFILES/link/${dotfiles_link} ~/${dotfiles_link}
        fi
    done
    shopt -u dotglob
    shopt -u globstar

    log_success "Finished linking files\n"
}

function dotfiles_source {
    # Source files in the DOTFILES/source dir
    local dotfiles_source_file
    for dotfiles_source_file in $DOTFILES/source/*; do
        source "$dotfiles_source_file"
    done
}

function dotfiles_tools {
    log_header1 "Installing tools...\n"

    local dotfiles_tools_file
    for dotfiles_tools_file in $DOTFILES/tools/*; do
        source "$dotfiles_tools_file"
    done

    log_success "Finished installing tools\n"
}

function prompt_sudo {
# Prompt for sudo early
sudo -E -s -- <<EOS
echo -e "\r\033[1;36mBeginning system set up...\n\033[0m";
EOS
}

function dotfiles_main {
    dotfiles_link
    dotfiles_source
}

