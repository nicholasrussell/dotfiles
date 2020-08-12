if is_debian; then
    # /etc/bash_completion.d/ sourced automatically

    for bcfile in ~/.bash_completion.d/* ; do
        source $bcfile
    done
elif is_macos; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        source $(brew --prefix)/etc/bash_completion
    fi
fi
