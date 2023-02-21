export CARGO_PATH="$HOME/.cargo"
export JENV_PATH="$HOME/.jenv"
export NVM_DIR="$HOME/.nvm"
export PYENV_ROOT="$HOME/.pyenv"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"

if [ -d "$HOME/bin" ]; then PATH="$HOME/bin:$PATH"; fi
if [ -d $PYENV_ROOT ]; then PATH="$PYENV_ROOT/bin:$PATH"; fi
if [ -d $JENV_PATH ]; then PATH="$HOME/.jenv/bin:$PATH"; fi

PATH="$PATH:$DOTFILES/bin"

export PATH
