#!/usr/bin/env bash

# Git
log_header2 "Configuring Git..."

if [ -n "${DOTFILES_GIT_USERNAME:+1}" ] && [ -n "${DOTFILES_GIT_EMAIL:+1}" ]; then
    # Replace .gitconfig values
    log_info "Setting .gitconfig user information..."
    sed -i "s/GIT_USERNAME/${DOTFILES_GIT_USERNAME}/g" ~/.gitconfig
    sed -i "s/GIT_EMAIL/${DOTFILES_GIT_EMAIL}/g" ~/.gitconfig
    log_info "Finished setting .gitconfig user information"
fi

# Install git completion
git_completion_url="https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash"
if [ ! -f ~/.git-completion.bash ]; then
    if wget -q --spider --timeout=30 $git_completion_url; then
        log_info "Downloading git-completion..."
        curl $git_completion_url -o ~/.git-completion.bash > /dev/null 2>&1
    else
        log_error "Failed to download git-completion as it does not exist on the remote server"
    fi
fi
unset git_completion_url

log_success "Finished configuring Git\n"
