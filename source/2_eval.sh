if is_ubuntu; then
    if [ -d "/home/linuxbrew/" ]; then
        eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    fi
fi

if [ -d $CARGO_PATH ]; then . "$CARGO_PATH/env"; fi
if command -v jenv 1>/dev/null 2>&1; then eval "$(jenv init -)"; fi
if [ -e $NVM_DIR ]; then [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"; fi
if command -v pyenv 1>/dev/null 2>&1; then eval "$(pyenv init -)"; fi

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi

