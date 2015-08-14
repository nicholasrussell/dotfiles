# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

if [ ! -n "${DOTFILES:+1}" ]; then
    export DOTFILES=~/.dotfiles
fi

# enable programmable completion features
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

shopt -s dotglob
# Source files in the DOTFILES/source dir
for dotfiles_source_file in $DOTFILES/source/*; do
    source "$dotfiles_source_file"
done
shopt -u dotglob
unset dotfiles_source_file
