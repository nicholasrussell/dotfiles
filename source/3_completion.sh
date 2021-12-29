if is_debian; then
    # /etc/bash_completion.d/ sourced automatically
    local bcfile
    for bcfile in ~/.bash_completion.d/* ; do
        source $bcfile
    done
    unset bcfile
elif is_macos; then
    [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && source "/usr/local/etc/profile.d/bash_completion.sh"
    [ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
fi
