if command -v jenv 1>/dev/null 2>&1; then eval "$(jenv init -)"; fi
if [ -e $NVM_DIR ]; then [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"; fi
if command -v pyenv 1>/dev/null 2>&1; then eval "$(pyenv init -)"; fi
