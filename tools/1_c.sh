#!/usr/bin/env bash

function apt_install() {
  sudo apt-get -qq -y install "$1" >/dev/null 2>&1
}

log_header2 "Installing C tools..."

if is_debian; then
  apt_install clang
  apt_install gdb
  apt_install valgrind
elif is_macos; then
  brew install llvm
  brew install gdb
fi

log_header2 "Finished installing C tools.\n"
