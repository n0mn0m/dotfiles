# My custom aliases
alias emacs="emacs -nw"
alias bkup="rsync -av --files-from=$HOME/.bkup.list $HOME /media/n0mn0m/bkup/; && dot;"
alias dot="rsync -av --files-from=$HOME/.dot.list $HOME $HOME/projects/dotfiles/;"
alias shieldsup="sudo wg-quick up wg0;"
alias shieldsdown="sudo wg-quick down wg0;"

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
