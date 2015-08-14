dotfiles
========
Collection of settings and tools for bringing up my development machine.

Warning! This will overwrite your system settings. Run with caution.

Usage
-----
1. Install git, wget, and curl: `sudo apt-get update && sudo apt-get install git wget curl`
2. Fork this git repository and clone: `git clone {repo} ~/.dotfiles`
3. Add pre and post hook scripts _hook/pre\_dotfiles.sh_ and _hook/post\_dotfiles.sh_.
In _post\_dotfiles.sh_, you may want to unset any temporary environment variables you create in the pre-hook.
4. Run `~/.dotfiles/bin/dotfiles`

Examples
--------

_pre\_dotfiles.sh_
```bash
#!/usr/bin/env bash

export DOTFILES_GIT_USERNAME="your_git_username"
export DOTFILES_GIT_EMAIL="your_git_email"
```

_post\_dotfiles.sh_
```bash
#!/usr/bin/env bash

unset DOTFILES_GIT_USERNAME
unset DOTFILES_GIT_EMAIL
```
