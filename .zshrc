setopt no_beep

bindkey -v
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -c"                  # $EDITOR opens in GUI mode
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
export LANG='en_US.UTF-8'

typeset -U path

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

# Directory shortcuts e.g.: ~proj
hash -d projects=$HOME/projects

# Always work in a virtual environment by default for Python.
source $HOME/.virtualenvs/39/bin/activate
export PYTHONBREAKPOINT='ipdb.set_trace'
export PYTHONSTARTUP=~/bin/pystrt.py

export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk13/Contents/Home

zstyle ':completion:*:*:git:*' script ~/.zsh/git-completion.bash
fpath=(~/.zsh/git $fpath)
fpath=(~/.zsh/zsh-completions/src $fpath)
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Turn off dotnet telemetry
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# zsh parameter completion for the dotnet CLI
_dotnet_zsh_complete()
{
  local completions=("$(dotnet complete "$words")")

  reply=( "${(ps:\n:)completions}" )
}
compctl -K _dotnet_zsh_complete dotnet

autoload bashcompinit && bashcompinit
complete -C '/usr/local/bin/aws_completer' aws


# Starship should be last
eval "$(starship init zsh)"
