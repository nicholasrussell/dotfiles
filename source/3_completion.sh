#!/usr/bin/env bash

if is_macos; then
  [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]] && source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
elif in_emacs; then
  if is_debian; then
    source "/etc/bash_completion"
  fi
fi

[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
