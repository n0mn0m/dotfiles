#!/usr/bin/env bash

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# Path to the bash it configuration
export BASH_IT="/home/alex/.bash_it"

# Lock and Load a custom theme file.
# Leave empty to disable theming.
# location /.bash_it/themes/
export BASH_IT_THEME='envy'

# (Advanced): Change this to the name of your remote repo if you
# cloned bash-it with a remote other than origin such as `bash-it`.
# export BASH_IT_REMOTE='bash-it'

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Change this to your console based IRC client of choice.
export IRC_CLIENT='irssi'

# Set this to the command you use for todo.txt-cli
export TODO="t"

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=true

# Set Xterm/screen/Tmux title with only a short hostname.
# Uncomment this (or set SHORT_HOSTNAME to something else),
# Will otherwise fall back on $HOSTNAME.
#export SHORT_HOSTNAME=$(hostname -s)

# Set Xterm/screen/Tmux title with only a short username.
# Uncomment this (or set SHORT_USER to something else),
# Will otherwise fall back on $USER.
#export SHORT_USER=${USER:0:8}

# Set Xterm/screen/Tmux title with shortened command and directory.
# Uncomment this to set.
#export SHORT_TERM_LINE=true

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/djl/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# (Advanced): Uncomment this to make Bash-it reload itself automatically
# after enabling or disabling aliases, plugins, and completions.
# export BASH_IT_AUTOMATIC_RELOAD_AFTER_CONFIG_CHANGE=1

# Uncomment this to make Bash-it create alias reload.
# export BASH_IT_RELOAD_LEGACY=1

# Load Bash It
source "$BASH_IT"/bash_it.sh

# Aliases
# examples: https://opensource.com/article/18/9/handy-bash-aliases

alias untar='tar -zxvf '
alias wget='wget -c '
alias getpass="openssl rand -base64 20"
alias sha='shasum -a 256 '
alias ping='ping -c 5'
alias www='python -m SimpleHTTPServer 8000'
alias speed='speedtest-cli --server 2406 --simple'
alias ipe='curl ipinfo.io/ip'
alias ipi='ipconfig getifaddr en0'
alias c='clear'
alias mtmux='~/.dev-tmux'

# Custom paths and versions
export PATH="$PATH:/opt/mssql-tools/bin"
. /home/alex/miniconda3/etc/profile.d/conda.sh
conda activate

# Check for custom vim install
if [ -d "$HOME/.local/vim/bin/" ] ; then
  PATH="$HOME/.local/vim/bin/:$PATH"
fi
