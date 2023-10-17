#!/usr/bin/env bash

log_header2 "Installing Lisp tools..."

function install_guile {
    apt_install guile-3.0
    apt_install guile-3.0-dev
}

install_guile

log_header2 "Finished installing Lisp tools.\n"
