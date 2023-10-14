[ -f $HOME/.profile ] && . $HOME/.profile

if [ $(get_os) = guix ]; then
    # Merge search-paths from multiple profiles. The order matters.
    eval "$(guix package --search-path \
              -p $HOME/.config/guix/current \
              -p $HOME/.guix-profile \
               -p /run/current-system/profile)"

    # Prepend setuid programs.
    export PATH=/run/setuid-programs:PATH
fi
