#!/usr/bin/env bash

# Groovy
log_header2 "Preparing to install Groovy..."

groovy_version="2.4.4"
groovy_binary_url="http://dl.bintray.com/groovy/maven/apache-groovy-binary-${groovy_version}.zip"
if wget -q --spider --timeout=30 ${groovy_binary_url}; then
    groovy_distributions=($(ls /opt | grep groovy\-.* | sed "s/^.*groovy-\([0-9.]*\).*/\1/"))
    if (( ${#groovy_distributions[@]} > 0 )); then
        for groovy_distribution in "${groovy_distributions[@]}"; do
            if [ "$groovy_distribution" != "$groovy_version" ]; then
                log_info "Removing Groovy ${groovy_distribution}..."
                sudo rm -r /opt/groovy-${groovy_distribution}
            fi
        done
        unset groovy_distribution
    fi
    unset groovy_distributions

    if [ ! -e /opt/groovy-${groovy_version} ]; then
        log_info "Downloading Groovy ${groovy_version}..."
        wget -q -O /tmp/groovy-${groovy_version}.zip ${groovy_binary_url}
        sudo 7z x -o/opt /tmp/groovy-${groovy_version}.zip > /dev/null
        rm -f /tmp/groovy-${groovy_version}.zip
        sudo ln -sf /opt/groovy-${groovy_version} /opt/groovy
    else
        log_info "Groovy ${groovy_version} already installed"
    fi

    log_success "Finished installing Groovy ${groovy_version}\n"
else
    log_error "Could not download Groovy ${groovy_version} because the binary does not exist on the remote server\n"
fi
unset groovy_version groovy_binary_url

# Scala
log_header2 "Preparing to install Scala..."

scala_version="2.11.7"
scala_binary_url="http://downloads.typesafe.com/scala/${scala_version}/scala-${scala_version}.tgz"
if wget -q --spider --timeout=30 ${scala_binary_url}; then
    scala_distributions=($(ls /opt | grep scala\-.* | sed "s/^.*scala-\([0-9.]*\).*/\1/"))
    if (( ${#scala_distributions[@]} > 0 )); then
        for scala_distribution in "${scala_distributions[@]}"; do
            if [ "$scala_distribution" != "$scala_version" ]; then
                log_info "Removing Scala ${scala_distribution}..."
                sudo rm -r /opt/scala-${scala_distribution}
            fi
        done
        unset scala_distribution
    fi
    unset scala_distributions

    if [ ! -e /opt/scala-${scala_version} ]; then
        log_info "Downloading Scala ${scala_version}..."
        wget -q -O /tmp/scala-${scala_version}.tgz ${scala_binary_url}
        sudo tar xzf /tmp/scala-${scala_version}.tgz -C /opt
        rm -f /tmp/scala-${scala_version}.tgz
        sudo ln -sf /opt/scala-${scala_version} /opt/scala
    else
        log_info "Scala ${scala_version} already installed"
    fi

    log_success "Finished installing Scala ${scala_version}\n"
else
    log_error "Could not download Scala ${scala_version} because the binary does not exist on the remote server\n"
fi
unset scala_version scala_binary_url

# sbt
log_header2 "Preparing to install sbt..."

sbt_version="0.13.9"
sbt_binary_url="https://dl.bintray.com/sbt/native-packages/sbt/${sbt_version}/sbt-${sbt_version}.tgz"
if wget -q --spider --timeout=30 ${sbt_binary_url}; then
    sbt_distributions=($(ls /opt | grep sbt\-.* | sed "s/^.*sbt-\([0-9.]*\).*/\1/"))
    if (( ${#sbt_distributions[@]} > 0 )); then
        for sbt_distribution in "${sbt_distributions[@]}"; do
            if [ "$sbt_distribution" != "$sbt_version" ]; then
                log_info "Removing sbt ${sbt_distribution}..."
                sudo rm -r /opt/sbt-${sbt_distribution}
            fi
        done
        unset sbt_distribution
    fi
    unset sbt_distributions

    if [ ! -e /opt/sbt-${sbt_version} ]; then
        log_info "Downloading sbt ${sbt_version}..."
        wget -q -O /tmp/sbt-${sbt_version}.tgz ${sbt_binary_url}
        sudo tar xzf /tmp/sbt-${sbt_version}.tgz -C /opt
        sudo mv /opt/sbt /opt/sbt-${sbt_version}
        rm -f /tmp/sbt-${sbt_version}.tgz
        sudo ln -sf /opt/sbt-${sbt_version} /opt/sbt
    else
        log_info "sbt ${sbt_version} already installed"
    fi

    log_success "Finished installing sbt ${sbt_version}\n"
else
    log_error "Could not download sbt ${sbt_version} because the binary does not exist on the remote server\n"
fi
unset sbt_version sbt_binary_url

# Typesafe Activator
log_header2 "Preparing to install Typesafe Activator..."

activator_version="1.3.6"
activator_binary_url="http://downloads.typesafe.com/typesafe-activator/${activator_version}/typesafe-activator-${activator_version}.zip"
if wget -q --spider --timeout=30 ${activator_binary_url}; then
    activator_distributions=($(ls /opt | grep activator\-.* | sed "s/^.*activator-\([0-9.]*\).*/\1/"))
    if (( ${#activator_distributions[@]} > 0 )); then
        for activator_distribution in "${activator_distributions[@]}"; do
            if [ "$activator_distribution" != "$activator_version" ]; then
                log_info "Removing Activator ${activator_distribution}..."
                sudo rm -r /opt/activator-${activator_distribution}
            fi
        done
        unset activator_distribution
    fi
    unset activator_distributions

    if [ ! -e /opt/activator-${activator_version} ]; then
        log_info "Downloading Activator ${activator_version}..."
        wget -q -O /tmp/activator-${activator_version}.zip ${activator_binary_url}
        sudo 7z x -o/opt /tmp/activator-${activator_version}.zip > /dev/null
        sudo mv /opt/activator-dist-${activator_version} /opt/activator-${activator_version}
        rm -f /tmp/activator-${activator_version}.zip
        sudo ln -sf /opt/activator-${activator_version} /opt/activator
        sudo chown -R ${USER}:${USER} /opt/activator-${activator_version}/
    else
        log_info "Activator ${activator_version} already installed"
    fi

    log_success "Finished installing Activator ${activator_version}\n"
else
    log_error "Could not download Activator ${activator_version} because the binary does not exist on the remote server\n"
fi
unset activator_version activator_binary_url

# Clojure
log_header2 "Installing Leiningen..."
if [ ! -e ~/bin/lein ]; then
    if [ ! -e ~/bin ]; then
        mkdir -p ~/bin
    fi
    wget -q -O ~/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
    chmod +x ~/bin/lein
    ~/bin/lein > /dev/null 2>&1
else
    log_info "Leiningen already installed"
fi
log_success "Finished installing Leiningen\n"
