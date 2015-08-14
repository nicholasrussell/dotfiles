# Define utility functions

if [ ! -n "${DOTFILES:+1}" ]; then
    export DOTFILES=~/.dotfiles
fi

function log_header1() { echo -e "\r\033[1;36m$@\033[0m"; }
function log_header2() { echo -e "\r\033[1;34m$@\033[0m"; }
function log_info() { echo -e "\r\033[0;34m$@\033[0m"; }
#function log_info() { echo -ne "\r$@"; }
function log_warn() { echo -e "\r\033[1;33m$@\033[0m"; }
function log_success() { echo -e "\r\033[1;32m$@\033[0m"; }
function log_error() { echo -e "\r\033[1;31m$@\033[0m"; }

# https://github.com/cowboy/dotfiles/blob/bbb73a2143737f996913da7379ed20fbd2348d6b/bin/dotfiles#L134
function setdiff() {
    local debug skip a b
    if [[ "$1" == 1 ]]; then debug=1; shift; fi
    if [[ "$1" ]]; then
        local setdiffA setdiffB setdiffC
        setdiffA=($1); setdiffB=($2)
    fi
    setdiffC=()
    for a in "${setdiffA[@]}"; do
        skip=
        for b in "${setdiffB[@]}"; do
          [[ "$a" == "$b" ]] && skip=1 && break
        done
        [[ "$skip" ]] || setdiffC=("${setdiffC[@]}" "$a")
    done
    [[ "$debug" ]] && for a in setdiffA setdiffB setdiffC; do
        echo "$a ($(eval echo "\${#$a[*]}")) $(eval echo "\${$a[*]}")" 1>&2
    done
    [[ "$1" ]] && echo "${setdiffC[@]}"
}

function add_ppa() {
    grep -h "^deb.*$1" /etc/apt/sources.list.d/* > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        log_info "Adding ppa:$1"
        sudo add-apt-repository -y ppa:$1
        return 0
    fi

    # log_warn "Not adding ppa:$1, it already exists"
    return 1
}

function apt_install() {
    local pkg
    pkg="$1"
    sudo apt-get -qq install "$pkg" > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        log_error "Failed to install apt package $pkg"
    fi
}

function apt_update() {
    sudo apt-get -qq update
}

function modify_numbered_files() {
    if [ $# -eq 2 ] && [ ! -z "$1" ]; then
        local start_n="$1"
        local modification="$2"
        local file_regex="[0-9]+_.*"
        local increment_file
        for increment_file in ./*; do
            if [[ "$increment_file" =~ $file_regex ]]; then
                local no_path="${increment_file:2}"
                local file_num="${no_path//_*/}"
                local rest_of_file="${no_path//$file_num/}"
                if [ "$file_num" -ge "$start_n" ]; then
                    local new_num
                    if [ "$modification" == "inc" ]; then
                        new_num="$(($file_num + 1))"
                    elif [ "$modification" == "dec" ]; then
                        new_num="$(($file_num - 1))"
                    fi
                    mv "$increment_file" "./${new_num}${rest_of_file}"
                fi
            fi
        done
    else
        log_error "Incorrect arguments"
        return 1
    fi
}

function increment_numbered_files() {
    modify_numbered_files "$1" "inc"
}

function decrement_numbered_files() {
    modify_numbered_files "$1" "dec"
}

function use_java_version() {
    # TODO Update JAVA_HOME
    sudo update-java-alternatives -s java-$1-oracle
}
