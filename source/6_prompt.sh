export CLR_NONE='\033[0m'
export CLR_BLACK='\033[0;30m'
export CLR_RED='\033[0;31m'
export CLR_GREEN='\033[0;32m'
export CLR_YELLOW='\033[0;33m'
export CLR_BLUE='\033[0;34m'
export CLR_PURPLE='\033[0;35m'
export CLR_CYAN='\033[0;36m'
export CLR_WHITE='\033[0;37m'

export CHR_LAMBDA='\u03bb'

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        color_prompt=yes
    else
        color_prompt=
    fi
fi

# Git PS1 Variables
#export GIT_PS1_SHOWDIRTYSTATE=true
#export GIT_PS1_SHOWSTASHSTATE=false
#export GIT_PS1_SHOWUNTRACKEDFILES=true
#export GIT_PS1_SHOWUPSTREAM="verbose"
#export GIT_PS1_DESCRIBE_STYLE="default"
#export GIT_PS1_SHOWCOLORHINTS=true
#export GIT_PS1_HIDE_IF_PWD_IGNORED=false
#export GIT_PS1_STATESEPARATOR=''

# Set title bar
function set_titlebar {
    case "$TERM" in
    xterm*|rxvt*)
        printf "\033]0;%s\007" "$*"
        ;;
    esac
}

# Ensures that master branch is differently colored
function git_colorize_master() {
    if [[ $PS1 == *"master"* ]]; then
        PS1=${PS1/master/"\[$CLR_RED\]master\[$CLR_NONE\]"}
    fi
}

function git_prompt_customization() {
    git_colorize_master
    # Unstaged Changes
#    if [[ $PS1 == *"*"* ]]; then
#        PS1=${PS1/\*/"\[$CLR_NONE\]$(echo -ne "$CHR_PLUS_MINUS")"}
#    fi
    # Staged Changes
#    if [[ $PS1 == *"+"* ]]; then
#        PS1=${PS1/+/"\[$CLR_NONE\]+"}
#    fi
    # Untracked Files
#    if [[ $PS1 == *"%"* ]]; then
#        PS1=${PS1/\%/"\[$CLR_NONE\]$(echo -ne "$CHR_U_DELTA")"}
#    fi
    # Upstream diff
#    if [[ $PS1 == *" u="* ]]; then
#        PS1=${PS1/ u=/}
#    elif [[ $PS1 =~ (.*)( u\+[0-9]+\-[0-9]+) ]]; then
#        PS1="$(echo -n $PS1 | sed -e "s/\( u+\)\([0-9]\+\)\(\-\)\([0-9]\+\)/$(echo -ne $CLR_GREEN$CHR_UP_ARROW$CLR_NONE) \2$(echo -ne $CLR_RED$CHR_DOWN_ARROW$CLR_NONE) \4/g")"
#        PS1="$PS1 "
#    elif [[ $PS1 =~ (.*)( u\+[0-9]+) ]]; then
#        PS1="$(echo -n $PS1 | sed -e "s/\( u+\)\([0-9]\+\)/$(echo -ne $CLR_GREEN$CHR_UP_ARROW$CLR_NONE) \2/g")"
#        PS1="$PS1 "
#    elif [[ $PS1 =~ (.*)( u\-[0-9]+) ]]; then
#        PS1="$(echo -n $PS1 | sed -e "s/\( u\-\)\([0-9]\+\)/$(echo -ne $CLR_RED$CHR_DOWN_ARROW$CLR_NONE) \2/g")"
#        PS1="$PS1 "
#    fi
}

export PROMPT_CHAR='$'

# Formats the bash prompt
function bash_prompt_command() {
    local EXIT_CODE="$?"
    local title_text="${USER}:${PWD/#$HOME/\~}"
    local prompt_prefix="$(echo -ne "$CHR_LAMBDA")\[$CLR_NONE\] ${debian_chroot:+($debian_chroot)}\[$CLR_GREEN\]\u\[$CLR_NONE\]:\[$CLR_BLUE\]\w\[$CLR_NONE\]"
    local prompt_suffix="\n\[$CLR_WHITEi\]$PROMPT_CHAR\[$CLR_NONE\] "
    local prompt_git=" [\[$CLR_CYAN\]%s\[$CLR_NONE\]]"
    if [ $EXIT_CODE != 0 ]; then
        prompt_prefix="\[$CLR_RED\]${prompt_prefix}"
    else
        prompt_prefix="\[$CLR_CYAN\]${prompt_prefix}"
    fi

    set_titlebar $title_text;
    __git_ps1 "$prompt_prefix" "$prompt_suffix" "$prompt_git";
    git_prompt_customization
}

if [ "$color_prompt" = yes ]; then
    export PROMPT_COMMAND='bash_prompt_command'
else
    export PROMPT_COMMAND='set_titlebar "${USER}:${PWD/#$HOME/\~}"; __git_ps1 "${debian_chroot:+($debian_chroot)}\u:\w" "\n\\\$ " " (%s)"'
fi
unset color_prompt force_color_prompt
