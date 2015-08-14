#!/usr/bin/env bash

# Gradle
log_header2 "Preparing to install Gradle..."

gradle_version="2.6"
gradle_binary_url="https://services.gradle.org/distributions/gradle-${gradle_version}-all.zip"
if wget -q --spider --timeout=30 ${gradle_binary_url}; then
    gradle_distributions=($(ls /opt | grep gradle\-.* | sed "s/^.*gradle-\([0-9.]*\).*/\1/"))
    if (( ${#gradle_distributions[@]} > 0 )); then
        for gradle_distribution in "${gradle_distributions[@]}"; do
            if [ "$gradle_distribution" != "$gradle_version" ]; then
                log_info "Removing Gradle ${gradle_distribution}..."
                sudo rm -r /opt/gradle-${gradle_distribution}
            fi
        done
        unset gradle_distribution
    fi
    unset gradle_distributions

    if [ ! -e /opt/gradle-${gradle_version} ]; then
        log_info "Downloading Gradle ${gradle_version}..."
        wget -q -O /tmp/gradle-${gradle_version}.zip ${gradle_binary_url}
        sudo 7z x -o/opt /tmp/gradle-${gradle_version}.zip > /dev/null
        rm -f /tmp/gradle-${gradle_version}.zip
        sudo ln -sf /opt/gradle-${gradle_version} /opt/gradle
    else
        log_info "Gradle ${gradle_version} already installed"
    fi

    log_success "Finished installing Gradle ${gradle_version}\n"
else
    log_error "Could not download Gradle ${gradle_version} because the binary does not exist on the remote server\n"
fi
unset gradle_version gradle_binary_url


log_header2 "Preparing to install Docker..."

# Docker
if which docker > /dev/null; then
    log_info "Docker already installed"
else
    wget -qO- https://get.docker.com/ | sh
    sudo groupadd docker
    sudo gpasswd -a ${USER} docker
    # sudo usermod -aG docker ${USER}
    sudo service docker restart
fi

# Docker Compose
docker_compose_version="1.1.0"
docker_compose_url="https://github.com/docker/compose/releases/download/${docker_compose_version}/docker-compose-$(uname -s)-$(uname -m)"
#if wget -q --spider --timeout=30 ${docker_compose_url}; then
    if [ ! -e /usr/local/bin/docker-compose ]; then
        log_info "Installing Docker Compose ${docker_compose_version}..."
        log_info "$docker_compose_url"
        sudo curl -s -J -L -o /usr/local/bin/docker-compose $docker_compose_url
        sudo chmod +x /usr/local/bin/docker-compose

        # Bash completion
        log_info "Installing Bash completion for Docker Compose..."
        docker_compose_completion_url="https://raw.githubusercontent.com/docker/compose/${docker_compose_version}/contrib/completion/bash/docker-compose"
        sudo curl -s -L -o /etc/bash_completion.d/docker-compose $docker_compose_completion_url
        unset docker_compose_completion_url
    else
        log_info "Docker Compose ${docker_compose_version} already installed"
    fi
#else
#    log_error "Failed to download Docker Compose ${docker_compose_version} because the binary does not exist on the remote server"
#fi
unset docker_compose_url docker_compose_version

# Docker Machine
docker_machine_version="0.1.0"
docker_machine_url="https://github.com/docker/machine/releases/download/v${docker_machine_version}/docker-machine_linux-amd64"
#if wget -q --spider --timeout=30 ${docker_machine_url}; then
    if [ ! -e /usr/local/bin/docker-machine ]; then
        log_info "Installing Docker Machine ${docker_machine_version}..."
        sudo curl -s -J -L -o /usr/local/bin/docker-machine $docker_machine_url
        sudo chmod +x /usr/local/bin/docker-machine
    else
        log_info "Docker Machine ${docker_machine_version} already installed"
    fi
#else
#    log_error "Failed to download Docker Machine ${docker_machine_version} because the binary does not exist on the remote server"
#fi
unset docker_machine_url docker_machine_version

log_success "Finished installing Docker\n"

# Vim
log_header2 "Configuring Vim..."

if [ ! -e ~/.vim/autoload/pathogen.vim ]; then
    log_info "Installing pathogen..."
    mkdir -p ~/.vim/autoload ~/.vim/bundle && curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
fi

if [ ! -e ~/.vim/bundle/vim-airline ]; then
    log_info "Installing airline..."
    git clone -q https://github.com/bling/vim-airline ~/.vim/bundle/vim-airline
fi

if [ ! -e ~/.vim/bundle/nerdtree ]; then
    log_info "Installing NERDTree..."
    git clone -q https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree
fi

if [ ! -e ~/.vim/bundle/vim-sensible ]; then
    log_info "Installing sensible..."
    git clone -q git://github.com/tpope/vim-sensible.git ~/.vim/bundle/vim-sensible
fi

if [ ! -e ~/.vim/bundle/vim-fugitive ]; then
    log_info "Installing fugitive..."
    git clone -q git://github.com/tpope/vim-fugitive.git ~/.vim/bundle/vim-fugitive
fi

log_success "Finished configuring vim\n"

# Idea
log_header2 "Preparing to install IntelliJ Idea..."

idea_version="14.1.4"
idea_binary_url="http://download-cf.jetbrains.com/idea/ideaIC-${idea_version}.tar.gz"
if wget -q --spider --timeout=30 ${idea_binary_url}; then
    if [ ! -e /opt/idea ]; then
        log_info "Downloading Idea ${idea_version}..."
        wget -q -O /tmp/idea.tar.gz ${idea_binary_url}
        sudo tar xzf /tmp/idea.tar.gz -C /opt
        rm -f /tmp/idea.tar.gz
        sudo mv /opt/idea-IC-* /opt/idea

        idea_desktop="[Desktop Entry]\nName=IntelliJ IDEA\nComment=IntelliJ IDEA IDE\nExec=/opt/idea/bin/idea.sh\nIcon=/opt/idea/bin/idea.png\nTerminal=false\nStartupNotify=true\nType=Application\nCategories=Development;IDE;"
        echo -e $idea_desktop | sudo tee /usr/share/applications/idea.desktop > /dev/null
        sudo ln -sf /opt/idea/bin/idea.png /usr/share/pixmaps/idea.png
        unset idea_desktop
    else
        log_info "Idea already installed"
    fi
    log_success "Finished installing Idea\n"
else
    log_error "Could not download Idea ${idea_version} because the binary does not exist on the remote server\n"
fi
unset idea_version idea_binary_url

# Sublime Text
log_header2 "Preparing to install Sublime Text..."

sublime_text_binary_url="http://c758482.r82.cf2.rackcdn.com/sublime-text_build-3083_amd64.deb"
if wget -q --spider --timeout=30 ${sublime_text_binary_url}; then
    if [ ! -e /opt/sublime_text ]; then
        log_info "Downloading Sublime Text..."
        wget -q -O /tmp/sublime_text.deb ${sublime_text_binary_url}
        sudo dpkg -i /tmp/sublime_text.deb
        rm -f /tmp/sublime_text.deb
    else
        log_info "Sublime Text already installed"
    fi
    log_success "Finished installing Sublime Text\n"
else
    log_error "Could not download Sublime Text because the binary does not exist on the remote server\n"
fi
unset sublime_text_binary_url

# Eclipse
log_header2 "Preparing to install Eclipse..."

eclipse_version="mars"
eclipse_binary_url="https://eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/${eclipse_version}/R/eclipse-java-${eclipse_version}-R-linux-gtk-x86_64.tar.gz&mirror_id=1"
if wget -q --spider --timeout=30 ${eclipse_binary_url}; then
    if [ ! -e /opt/eclipse ]; then
        log_info "Downloading Eclipse ${eclipse_version}..."
        wget -q -O /tmp/eclipse.tar.gz ${eclipse_binary_url}
        sudo tar xzf /tmp/eclipse.tar.gz -C /opt
        rm -f /tmp/eclipse.tar.gz
    else
        log_info "Eclipse already installed"
    fi
    log_success "Finished installing Eclipse\n"
else
    log_error "Could not download Eclipse ${eclipse_version} because the binary does not exist on the remote server\n"
fi
unset eclipse_version eclipse_binary_url

# Meld
log_header2 "Installing Meld..."

meld_git_url="https://git.gnome.org/browse/meld"
if wget -q --spider --timeout=30 ${meld_git_url}; then
    if [ ! -e /opt/meld ]; then
        log_info "Cloning Meld..."
        sudo git clone -q $meld_git_url /opt/meld
        sudo chown -R ${USER}:${USER} /opt/meld
    else
        log_info "Meld already installed"
    fi

    log_success "Finished installing Meld\n"
else
    log_error "Could not clone Meld because the remote repository does not exist\n"
fi
unset meld_git_url

# fpp
log_header2 "Installing PathPicker..."

fpp_git_url="https://github.com/facebook/PathPicker.git"
if wget -q --spider --timeout=30 ${fpp_git_url}; then
    if [ ! -e /opt/pathpicker ]; then
        log_info "Cloning PathPicker..."
        sudo git clone -q $fpp_git_url /opt/pathpicker
        sudo ln -sf /opt/pathpicker/fpp /usr/local/bin/fpp
    else
        log_info "PathPicker already installed"
    fi

    log_success "Finished installing PathPicker\n"
else
    log_error "Could not clone PathPicker because the remote repository does not exist\n"
fi
unset fpp_git_url
