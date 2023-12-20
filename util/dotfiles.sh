#!/usr/bin/env bash

function log_header1 { echo -e "\r\033[1;36m$@\033[0m"; }
function log_header2 { echo -e "\r\033[1;34m$@\033[0m"; }
function log_info { echo -e "\r\033[0;34m$@\033[0m"; }
function log_warn { echo -e "\r\033[1;33m$@\033[0m"; }
function log_success { echo -e "\r\033[1;32m$@\033[0m"; }
function log_error { echo -e "\r\033[1;31m$@\033[0m"; }

## dotfiles functions
function dotfiles_link {
    if [ -e "$DOTFILES/backup/link" ]; then
        rm -r "$DOTFILES/backup/link"
    fi
    mkdir -p "$DOTFILES/backup/link"

    source "$DOTFILES/source/0_init.sh"

    log_header1 "Linking files..."
    shopt -s dotglob
    shopt -s globstar
    shopt -s nullglob

    declare -A link_exclude
    local library_list=($DOTFILES/link/Library/**/*)
    local empty_list=()
    link_exclude[guix]=library_list[@]
    link_exclude[ubuntu]=library_list[@]
    link_exclude[macos]=empty_list[@]

    local dotfiles_link
    local os=$(get_os)
    for dotfiles_link in $DOTFILES/link/**/*; do
        let skip=false
        for exclude in "${!link_exclude[$os]}"; do
            if [[ "$exclude" == "$dotfiles_link" ]]; then
                log_info "Skipping ${dotfiles_link}..."
                skip=true
            fi
        done
        if [ "$skip" = true ]; then continue; fi
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
    shopt -u nullglob

    log_success "Finished linking files\n"
}

function dotfiles_source {
    # Source files in the DOTFILES/source dir
    local dotfiles_source_file
    for dotfiles_source_file in $DOTFILES/source/*; do
        source "$dotfiles_source_file"
    done
}

function unshift() {
    local -n ary=$1;
    shift;
    ary=("$@" "${ary[@]}");
}

function dotfiles_tools {
    local scripts included_scripts maybe_match force
    scripts=($DOTFILES/tools/*)
    included_scripts=()

    for script in "$@"; do
        maybe_match=$(printf '%s\n' "${scripts[@]}" | grep -P "_${script}\.sh")
        if [[ $maybe_match ]]; then
            included_scripts+=($maybe_match)
        fi
    done
    if [ ${#included_scripts[@]} -eq 0 ]; then
        included_scripts=("${scripts[@]}")
    else
        included_scripts=("$DOTFILES/tools/0_bootstrap.sh" "${included_scripts[@]}")
    fi

    dotfiles_source
    log_header1 "Installing tools...\n"

    local dotfiles_tools_file
    for dotfiles_tools_file in ${included_scripts[@]}; do
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

