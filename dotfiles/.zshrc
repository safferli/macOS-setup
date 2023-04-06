# syntax highlighting
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export CLICOLOR=1


source "/opt/homebrew/opt/zsh-git-prompt/zshrc.sh"

# autocomplete
# get rid of insecure directories warning: 
# compaudit | xargs chmod g-w
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

  autoload -Uz compinit
  compinit
fi


# brackets magic
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic


# url magic
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# SSH Config File Completion
h=()
if [[ -r ~/.ssh/config ]]; then
  h=($h ${${${(@M)${(f)"$(cat ~/.ssh/config)"}:#Host *}#Host }:#*[*?]*})
fi
if [[ $#h -gt 0 ]]; then
  zstyle ':completion:*:ssh:*' hosts $h
  zstyle ':completion:*:slogin:*' hosts $h
fi

# starship cross-shell prompt 
eval "$(starship init zsh)"




## aliases
alias ls='exa -lh --git --icons'
alias battery_stat='pmset -g batt'


## rust support
fpath+=~/.zfunc


# recursive chmod
#  $ chmod 700 **/(.) # Only files
#  $ chmod 700 **/(/) # Only directories




test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

