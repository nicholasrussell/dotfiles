# Path Exports

if is_ubuntu; then
    export ACTIVATOR_HOME=/opt/activator
    export ECLIPSE_HOME=/opt/eclipse
    export GRADLE_HOME=/opt/gradle
    export GROOVY_HOME=/opt/groovy
    export IDEA_HOME=/opt/idea
    export JAVA_HOME=/usr/lib/jvm/java-8-oracle
    export MELD_HOME=/opt/meld
    export MIT_SCHEME_HOME=/opt/mit-scheme
    export PLAY_HOME=/opt/play
    export QUICKLISP_HOME=/opt/quicklisp
    export RBENV_HOME=$HOME/.rbenv
    export SBCL_ROOT=/opt/sbcl
    export SBCL_HOME=/opt/sbcl/lib/sbcl
    export SBT_HOME=/opt/sbt
    export SCALA_HOME=/opt/scala

    if [ -d "$HOME/bin" ]; then PATH="$HOME/bin:$PATH"; fi
    if [ -d "$GROOVY_HOME" ]; then PATH="$PATH:$GROOVY_HOME/bin"; fi
    if [ -d "$SCALA_HOME" ]; then PATH="$PATH:$SCALA_HOME/bin"; fi
    if [ -d "$SBT_HOME" ]; then PATH="$PATH:$SBT_HOME/bin"; fi
    if [ -d "$ACTIVATOR_HOME" ]; then PATH="$PATH:$ACTIVATOR_HOME"; fi
    if [ -d "$GRADLE_HOME" ]; then PATH="$PATH:$GRADLE_HOME/bin"; fi
    if [ -d "$MELD_HOME" ]; then PATH="$PATH:$MELD_HOME/bin"; fi
    if [ -d "$MIT_SCHEME_HOME" ]; then PATH="$PATH:$MIT_SCHEME_HOME/bin"; fi
    if [ -d "$SBCL_ROOT" ]; then PATH="$PATH:$SBCL_ROOT/bin"; fi
    if [ -d "$PLAY_HOME" ]; then PATH="$PATH:$PLAY_HOME/bin"; fi
    if [ -d "$IDEA_HOME" ]; then PATH="$PATH:$IDEA_HOME/bin"; fi
    if [ -d "$ECLIPSE_HOME" ]; then PATH="$PATH:$ECLIPSE_HOME"; fi
    if [ -d "$RBENV_HOME/bin" ]; then
        PATH="$RBENV_HOME/bin:$PATH"
        eval "$(rbenv init -)"
    fi

    PATH="$PATH:$DOTFILES/bin"

    export PATH
elif is_osx; then
    export GROOVY_HOME=/usr/local/opt/groovy/libexec
    export JAVA_HOME="$(/usr/libexec/java_home -v 1.8)"

    if [ -d "$HOME/bin" ]; then PATH="$HOME/bin:$PATH"; fi

    PATH="$PATH:$DOTFILES/bin"

    export PATH

    if which jenv > /dev/null; then eval "$(jenv init -)"; fi
fi