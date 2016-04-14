if is_ubuntu; then
    # Add git completion for custom git aliases
    if [ -f ~/.git-completion.bash ]; then
        source ~/.git-completion.bash

        __git_complete g __git_main
    fi

    # Add docker completion
    if [ -f /etc/bash_completion.d/docker ]; then
        source /etc/bash_completion.d/docker
    fi
    if [ -f /etc/bash_completion.d/docker-compose ]; then
        source /etc/bash_completion.d/docker-compose
    fi
fi

if is_osx; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        source $(brew --prefix)/etc/bash_completion
    fi
fi