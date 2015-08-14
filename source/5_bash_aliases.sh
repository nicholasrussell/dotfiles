if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'

alias g='git'

alias update="sudo apt-get update && sudo pip install -U pip && sudo npm update -g && gem update --system && gem update"

# alias docker='sudo -E docker'

alias gradle-java='gradle init --type java-library'

alias java7='sudo update-java-alternatives -s java-7-oracle && echo "Please update JAVA_HOME"'
alias java8='sudo update-java-alternatives -s java-8-oracle && echo "Please update JAVA_HOME"'
