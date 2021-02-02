setopt no_beep

bindkey -v
export LANG='en_US.UTF-8'
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode


typeset -U path
path=(/usr/local/opt/openjdk/bin /usr/local/opt/ruby/bin /Users/n0mn0m/.cargo/bin /Users/n0mn0m/.npm-global/bin /opt/local/bin /opt/local/sbin/ /usr/local/lib/ruby/gems/2.7.0/bin /usr/local/bin /usr/local/sbin /usr/local/opt/openjdk/bin $path)

# Navigation
setopt AUTO_CD PUSHD_MINUS PUSHD_SILENT
DIRSTACKSIZE='25'

# Completion
autoload -Uz compinit
compinit -u

# Correction
setopt correct

# Prompt
setopt prompt_subst
autoload -U promptinit
promptinit
autoload -Uz vcs_info
autoload +X VCS_INFO_nvcsformats

functions[VCS_INFO_nvcsformats]=${functions[VCS_INFO_nvcsformats]/local -a msgs/}

# Style outside of git repo
zstyle ':vcs_info:*' nvcsformats '%{%F{magenta}%}%~%{%f%}'

# base git formatting in a repo
zstyle ':vcs_info:*' formats '%{%F{magenta}%}%R %{%f%}%{%F{green}%}(%25>…>%b%<<) %{%f%}%c%u'

# show unsubmitted changes
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr '%{%F{red}%B%}!%{%b%f%}'
zstyle ':vcs_info:*' stagedstr '%{%F{green}%B%}+%{%b%f%}'

# Handle special git states such as rebase or merge replacing branch with state information
zstyle ':vcs_info:*' actionformats '%{%F{cyan}%}%45<…<%R%<</%{%f%}%{%F{red}%}(%a|%m)%{%f%}%{%F{cyan}%}%S%{%f%}%c%u'
zstyle ':vcs_info:git:*' patch-format '%10>…>%p%<< (%n applied)'
zstyle ':vcs_info:*+set-message:*' hooks home-path
zstyle ':vcs_info:git+post-backend:*' hooks git-remote-staged

function precmd() { vcs_info }
PROMPT='[ %D{%y-%m-%f %H:%M:%S} | %F{cyan}%n%F{white}@%F{yellow}%m %F{white}| ${vcs_info_msg_0_} %{%b%E%}] %# '

# History
setopt INC_APPEND_HISTORY EXTENDED_HISTORY SHARE_HISTORY HIST_IGNORE_ALL_DUPS HIST_IGNORE_SPACE HIST_EXPIRE_DUPS_FIRST NO_HIST_BEEP
HISTSIZE='1000'
SAVEHIST='1000'
HISTFILE="$HOME/.history"

# Aliases
if [[ -r ~/.aliasrc ]]; then
	. ~/.aliasrc
fi

# Always work in a virtual environment by default for Python.
source $HOME/.virtualenvs/39/bin/activate
export PYTHONBREAKPOINT='pudb.set_trace'

export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk13/Contents/Home

# Turn off dotnet telemetry
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# zsh parameter completion for the dotnet CLI

_dotnet_zsh_complete()
{
  local completions=("$(dotnet complete "$words")")

  reply=( "${(ps:\n:)completions}" )
}

compctl -K _dotnet_zsh_complete dotnet

source /Users/alexander/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /Users/alexander/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
fpath=(/usr/local/share/zsh/site-functions /Users/alexander/.zsh/zsh-completions/src $fpath)

# For docker and cross platform compat
export USERPROFILE=$HOME

# AWS complete
autoload bashcompinit && bashcompinit
complete -C '/usr/local/bin/aws_completer' aws

eval "$(starship init zsh)"
