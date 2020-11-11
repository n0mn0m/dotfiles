export PATH=/Applications/Keybase.app/Contents/SharedSupport/bin:/Applications/Racket/bin:/Applications/exercism:/Users/n0mn0m/.cargo/bin:/Users/n0mn0m/.npm-global/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:

export EDITOR=vim
export LANG=en_US.UTF-8

# Always work in a virtual environment by default for Python.
source $HOME/.virtualenvs/38/bin/activate
export PYTHONBREAKPOINT="pudb.set_trace"

export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk13/Contents/Home

# Aliases
if [ -r $HOME/.aliasrc ]; then
    . $HOME/.aliasrc
fi

export PATH="$HOME/.cargo/bin:$PATH"
