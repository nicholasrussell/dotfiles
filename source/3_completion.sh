if is_debian; then
    # /etc/bash_completion.d/ sourced automatically

    for bcfile in ~/.bash_completion.d/* ; do
        source $bcfile
    done
elif is_macos; then
    [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
fi
