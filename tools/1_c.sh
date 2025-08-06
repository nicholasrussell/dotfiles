#!/usr/bin/env bash

function apt_install() {
  sudo apt-get -qq -y install "$1" >/dev/null 2>&1
}

log_header2 "Installing C tools..."

apt_install clang
apt_install gdb
apt_install valgrind

log_header2 "Finished installing C tools.\n"
