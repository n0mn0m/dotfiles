export PATH=/Applications/Keybase.app/Contents/SharedSupport/bin:/Applications/Racket/bin:/Applications/exercism:/Users/n0mn0m/.cargo/bin:/Users/n0mn0m/.npm-global/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:

# export LDFLAGS="-L/opt/local/libexec/llvm-9.0/lib -Wl,-rpath/opt/local/libexec/llvm-9.0/lib"
# export CPPFLAGS="-I/opt/local/libexec/llvm-9.0//include"
export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk13/Contents/Home

export EDITOR=vim
export LANG=en_US.UTF-8

# Always work in a virtual environment by default for Python.
source $HOME/.virtualenvs/38/bin/activate
export PYTHONBREAKPOINT="pudb.set_trace"


# Aliases
alias emacs="/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs "
alias tmacs="emacs -nw"
alias nux="TERM=xterm-256color tmux new -s"
alias bkup="rsync -av --files-from=$HOME/.bkup.list $HOME /media/n0mn0m/bkup/;"
alias local-bkup="tar -cvf bkup.tar -T .bkup.list"
alias dot="rsync -av --files-from=$HOME/.dot.list $HOME $HOME/projects/dotfiles/;"
alias wg-up="sudo wg-quick up wg0;"
alias wg-down="sudo wg-quick down wg0;"
alias nvenv="function nvenv(){ python3 -m venv ~/.virtualenvs/$1; }; nvenv"
alias mute="amixer set Master 0%"
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias zola="/Applications/zola"
alias markdown="grip"
alias adf="export ADF_PATH=$HOME/projects/esp-adf"
alias on-air="curl -X PUT 'https://signal.unexpectedeof.casa/on-air'"
alias status="curl -X GET 'https://signal.unexpectedeof.casa/on-air'"
alias idf-who="deactivate && source $HOME/.virtualenvs/who/bin/activate && . $HOME/projects/esp-who/esp-idf/export.sh"
alias idf-refresh="rm -rf $HOME/projects/esp-idf && git clone --recursive git@github.com:espressif/esp-idf.git $HOME/projects/esp-idf && $HOME/projects/esp-idf/install.sh"
alias idf=". $HOME/projects/esp-idf/export.sh"
alias idf3="pushd $HOME/projects/esp-idf && git checkout release/v3.3 && popd && . $HOME/projects/esp-idf/export.sh"
alias idf4x="pushd $HOME/projects/esp-idf && git checkout release/v4.0 && popd && . $HOME/projects/esp-idf/export.sh"
alias idf4="pushd $HOME/projects/esp-idf && git checkout release/v4.1 && popd && . $HOME/projects/esp-idf/export.sh"
alias idf-test="idf.py --port /dev/cu.SLAB_USBtoUART flash monitor"
alias remotes-update="git branch -r | grep -v '\->' | while read remote; do git branch --track ${remote#origin/} $remote; done && git fetch --all && git pull --all && git submodule update --init --recursive"
alias submodules-update="git submodule update --init --recursive"


# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
