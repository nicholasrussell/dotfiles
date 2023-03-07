# if is_debian; then
    # /etc/bash_completion.d/ sourced automatically
# el
if is_macos; then
    [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && source "/usr/local/etc/profile.d/bash_completion.sh"
    [ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
elif in_emacs; then
    if is_debian; then
        source "/etc/bash_completion"
    fi
fi
