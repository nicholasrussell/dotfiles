#!/usr/bin/env bash

# MIT Scheme
log_header2 "Preparing to install MIT Scheme..."

mit_scheme_version="9.2"
mit_scheme_binary_url="http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/${mit_scheme_version}/mit-scheme-${mit_scheme_version}-x86-64.tar.gz"
if wget -q --spider --timeout=30 ${mit_scheme_binary_url}; then
    mit_scheme_installations=($(ls /opt | grep mit\-scheme\-.* | sed "s/^.*mit-scheme-\([0-9.]*\).*/\1/"))
    if (( ${#mit_scheme_installations[@]} > 0 )); then
        for mit_scheme_installation in "${mit_scheme_installations[@]}"; do
            if [ "$mit_scheme_installation" != "$mit_scheme_version" ]; then
                log_info "Removing MIT Scheme ${mit_scheme_installation}..."
                sudo rm -r /opt/mit-scheme-${mit_scheme_installation}
            fi
        done
        unset mit_scheme_installation
    fi
    unset mit_scheme_installations

    if [ ! -e /opt/mit-scheme-${mit_scheme_version} ]; then
        log_info "Downloading MIT Scheme ${mit_scheme_version}..."
        current_working_dir=$(pwd)
        wget -q -O /tmp/mit-scheme-${mit_scheme_version}.tar.gz ${mit_scheme_binary_url}
        tar xzf /tmp/mit-scheme-${mit_scheme_version}.tar.gz -C /tmp
        cd /tmp/mit-scheme-${mit_scheme_version}/src
        log_info "Configuring MIT Scheme ${mit_scheme_version}..."
        chmod +x configure
        ./configure --prefix=/opt/mit-scheme-${mit_scheme_version} > /dev/null
        log_info "Compiling MIT Scheme ${mit_scheme_version}..."
        make compile-microcode > /dev/null
        log_info "Installing MIT Scheme ${mit_scheme_version}..."
        sudo make install > /dev/null
        cd $current_working_dir
        rm -rf /tmp/mit-scheme-${mit_scheme_version}/
        rm -f /tmp/mit-scheme-${mit_scheme_version}.tar.gz
        unset current_working_dir
        sudo ln -sf /opt/mit-scheme-${mit_scheme_version} /opt/mit-scheme
        sudo ln -sf /opt/mit-scheme/bin/scheme /usr/local/bin/scheme
    else
        log_info "MIT Scheme ${mit_scheme_version} already installed"
    fi

    log_success "Finished installing MIT Scheme ${mit_scheme_version}\n"
else
    log_error "Could not download MIT Scheme ${mit_scheme_version} because the binary does not exist on the remote server\n"
fi
unset mit_scheme_version mit_scheme_binary_url

# SBCL
log_header2 "Preparing to install SBCL..."

sbcl_version="1.2.11"
sbcl_binary_url="http://iweb.dl.sourceforge.net/project/sbcl/sbcl/${sbcl_version}/sbcl-${sbcl_version}-x86-64-linux-binary.tar.bz2"
if wget -q --spider --timeout=30 ${sbcl_binary_url}; then
    sbcl_installations=($(ls /opt | grep sbcl\-.* | sed "s/^.*sbcl-\([0-9.]*\).*/\1/"))
    if (( ${#sbcl_installations[@]} > 0 )); then
        for sbcl_installation in "${sbcl_installations[@]}"; do
            if [ "$sbcl_installation" != "$sbcl_version" ]; then
                log_info "Removing SBCL ${sbcl_installation}..."
                sudo rm -r /opt/sbcl-${sbcl_installation}
            fi
        done
        unset sbcl_installation
    fi
    unset sbcl_installations

    if [ ! -e /opt/sbcl-${sbcl_version} ]; then
        log_info "Downloading SBCL ${sbcl_version}..."
        current_working_dir=$(pwd)
        wget -q -O /tmp/sbcl-${sbcl_version}.tar.bz2 ${sbcl_binary_url}
        tar xjf /tmp/sbcl-${sbcl_version}.tar.bz2 -C /tmp
        log_info "Installing SBCL ${sbcl_version}..."
        cd /tmp/sbcl-${sbcl_version}-x86-64-linux
        unset SBCL_HOME
        INSTALL_ROOT=/opt/sbcl-${sbcl_version} sudo -E sh install.sh > /dev/null
        cd $current_working_dir
        rm -rf /tmp/sbcl-${sbcl_version}-x86-64-linux/
        rm -f /tmp/sbcl-${sbcl_version}.tar.bz2
        unset current_working_dir
        sudo ln -sf /opt/sbcl-${sbcl_version} /opt/sbcl
    else
        log_info "SBCL ${sbcl_version} already installed"
    fi

    log_success "Finished installing SBCL ${sbcl_version}\n"
else
    log_error "Could not download SBCL ${sbcl_version} because the binary does not exist on the remote server\n"
fi
unset sbcl_version sbcl_binary_url

# Quicklisp
log_header2 "Preparing to install Quicklisp..."

quicklisp_binary_url="https://beta.quicklisp.org/quicklisp.lisp"
if wget -q --spider --timeout=30 ${quicklisp_binary_url}; then
    if [ ! -e /opt/quicklisp/quicklisp.lisp ]; then
        sudo mkdir -p /opt/quicklisp
        sudo curl -s -o /opt/quicklisp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
        sudo curl -s -o /opt/quicklisp/quicklisp.lisp.asc https://beta.quicklisp.org/quicklisp.lisp.asc
        # gpg --verify /opt/quicklisp/quicklisp.lisp.asc /opt/quicklisp/quicklisp.lisp
        /opt/sbcl/bin/sbcl --load /opt/quicklisp/quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(exit)"
        # --eval "(ql:add-to-init-file)"
    else
        log_info "Quicklisp already installed"
    fi

    log_success "Finished installing Quicklisp\n"
else
    log_error "Could not download Quicklisp because the binary does not exist on the remote server\n"
fi
unset quicklisp_binary_url
