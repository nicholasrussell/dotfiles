#!/usr/bin/env bash

## had to stick these somewhere for now
function apt_tools {
	local dotfiles_apt_tools
	dotfiles_apt_tools=(
		cmake
		libtool
		libtool-bin
		jq
		unzip
		fontconfig
		# Emacs
		libgccjit0
		libgccjit-10-dev
		texinfo
		libxpm-dev
		libjpeg-dev
		libgif-dev
		giflib-tools # instead of libungif-bin
		libtiff-dev
		libtree-sitter-dev
		libjansson4
		libjansson-dev
		libacl1-dev
		libvterm-dev
		xclip
		xdotool
		libmagickwand-dev
		libm17n-dev
		libwebkit2gtk-4.1
		## xprop
		## xwininfo
		# nvim
		neovim
		# Rust
		lld
		clang
		# below are for Python 3, move to tools?
		zlib1g-dev
		libbz2-dev
		libreadline-dev
		libsqlite3-dev
		libncursesw5-dev
		xz-utils
		tk-dev
		libxml2-dev
		libxmlsec1-dev
		libffi-dev
		liblzma-dev
		# lisps
		guile-3.0
		guile-3.0-dev
		# wm
		i3
		# misc
		htop
		ripgrep
		shellcheck
	)

	# neovim
	sudo add-apt-repository -y ppa:neovim-ppa/unstable >/dev/null 2>&1

	# i3
	/usr/lib/apt/apt-helper download-file https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/sur5r-keyring_2023.02.18_all.deb keyring.deb SHA256:a511ac5f10cd811f8a4ca44d665f2fa1add7a9f09bef238cdfad8461f5239cc4
	sudo apt install ./keyring.deb
	rm keyring.deb
	echo "deb http://debian.sur5r.net/i3/ $(grep '^DISTRIB_CODENAME=' /etc/lsb-release | cut -f2 -d=) universe" | sudo tee /etc/apt/sources.list.d/sur5r-i3.lis

	sudo apt-get -qq update

	sudo apt-get -qq -y install "${dotfiles_apt_tools[@]}" >/dev/null 2>&1

	sudo apt-get build-dep emacs
}

if is_debian; then
    apt_tools
elif is_macos; then
    brew_install coreutils
    brew_install fd
    brew_install cmake
    brew_install libvterm
fi
