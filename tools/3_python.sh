#!/usr/bin/env bash

log_header2 "Installing Python tools..."

function install_pyenv_debian {
	log_info "Installing pyenv..."
	if [ ! -d ~/.pyenv ]; then
		git clone https://github.com/pyenv/pyenv.git ~/.pyenv
		dotfiles_source
		log_info "Finished installing pyenv."
	elif [[ -v DOTFILES_TOOLS_FORCE ]]; then
		cd ~/.pyenv && git pull -q && cd - >/dev/null || return
		log_info "Finished updating pyenv."
	else
		log_info "pyenv already installed!"
	fi
}

function install_pyenv {
	if is_macos; then
		brew_install pyenv
	else
		install_pyenv_debian
	fi
}
function install_python3 {
	local version='3.12.3'
	log_info "Installing Python 3..."
	if ! pyenv versions | grep $version >/dev/null 2>&1 || [[ -v DOTFILES_TOOLS_FORCE ]]; then
		pyenv install -f $version
		pyenv global $version
		log_info "Finished installing Python 3."
	else
		log_info "Python 3 already installed!"
	fi
}

install_pyenv
install_python3

log_header2 "Finished installing Python tools.\n"
