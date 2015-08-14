if is_macos; then
  alias ls="gls --color=auto --group-directories-first --time-style=long-iso -hv"

  alias load-emacs-daemon="launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
  alias unload-emacs-daemon="launchctl unload -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
else
  alias ls="ls --color=auto --group-directories-first --time-style=long-iso -hv"
fi

alias kill-emacs="emacsclient -e \"(kill-emacs)\""
