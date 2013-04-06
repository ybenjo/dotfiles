# .zshrc
# mickey24
export LANG=ja_JP.UTF-8

export PATH=/opt/local/bin:/usr/local/bin:/opt/local/sbin:/usr/X11R6/bin:/usr/bin:/bin:/opt:local/sbin:/usr/sbin:/sbin:$HOME/go/bin:$PATH
export MANPATH=/opt/local/man:$MANPATH

export EDITOR='emacs'
export CPATH=$CPATH:$HOME/usr/local/bin/include:$HOME/local/include
export CC=gcc-mp-4.7
export CXX=g++-mp-4.7

export LIBRARY_PATH=/opt/local/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=/opt/local/lib:$LD_LIBRARY_PATH
export C_INCLUDE_PATH=/opt/local/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=/opt/local/include:$CPLUS_INCLUDE_PATH
export DYLD_FALLBACK_LIBRARY_PATH=/opt/local/lib
export BOOST_ROOT=/opt/local/include/boost

# History
HISTFILE=${HOME}/.zsh_history
SAVEHIST=10000
HISTSIZE=10000
setopt append_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_nodups
setopt share_history

# Directory
DIRSTACKSIZE=8
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_minus
setopt pushd_silent
setopt pushd_to_home

# Completion
LISTMAX=0
setopt complete_aliases
setopt complete_in_word
setopt extendedglob
unsetopt list_ambiguous
setopt list_packed
setopt list_types
setopt mark_dirs
setopt numeric_glob_sort
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:warnings' format 'No matches'

autoload -U compinit
compinit -u

# Prompt
autoload -U colors; colors
setopt prompt_subst
unsetopt transient_rprompt

# git
# http://d.hatena.ne.jp/uasi/20091025/1256458798
autoload -Uz VCS_INFO_get_data_git; VCS_INFO_get_data_git 2> /dev/null

function rprompt-git-current-branch {
  local name st color gitdir action
  if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
      return
  fi
  name=`git branch 2> /dev/null | grep '^\*' | cut -b 3-`
  if [[ -z $name ]]; then
      return
  fi
  
  gitdir=`git rev-parse --git-dir 2> /dev/null`
  action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"
  
  st=`git status 2> /dev/null`
  if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
      color=%F{green}
  elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
      color=%F{yellow}
  elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
      color=%B%F{red}
  else
      color=%F{red}
  fi
  
  echo "[$color$name$action%f%b]"
}

if [ $SSH_CONNECTION ] || [ $REMOTEHOST ]; then
  PROMPT='%{%(!.$bg[default].%(?.$bg[blue].$bg[red]))%}%n@%m:%(5~,%-2~/.../%2~,%~)%#%{$reset_color%} '
  RPROMPT='`rprompt-git-current-branch`%{%(!.$bg[default].%(?.$bg[blue].$bg[red]))%}[`date +%m/%d` %T]%{$reset_color%}'
else
  PROMPT='%{%(!.$bg[default].%(?.$bg[green].$bg[yellow]))%}%n@%m:%(5~,%-2~/.../%2~,%~)%#%{$reset_color%} '
  RPROMPT='`rprompt-git-current-branch`%{%(!.$bg[default].%(?.$bg[green].$bg[yellow]))%}[`date +%m/%d` %T]%{$reset_color%}'
fi

# Misc
umask 022
limit coredumpsize 0
stty erase '^h'
stty kill '^g'
stty stop 'undef'

bindkey -e

setopt bad_pattern
unsetopt beep
setopt c_bases
setopt check_jobs
unsetopt clobber
unsetopt flow_control
setopt ignore_eof
setopt long_list_jobs
setopt print_eightbit

autoload -U tetris; zle -N tetris

# History search
autoload -U  up-line-or-beginning-search
zle      -N  up-line-or-beginning-search
bindkey '^P' up-line-or-beginning-search

autoload -U  down-line-or-beginning-search
zle      -N  down-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search

# Abbreviation
typeset -A myAbbrev
myAbbrev=(
"L" "| less"
"P" "| pbcopy"
"G" "| grep"
"H" "| head"
"T" "| tail"
"W" "| wc -l"
"A" "| awk"
"S" "| sed"
"Y" "yes |"
"...." "../.."
"R" "| ruby -e ''"
)
function my-expand-abbrev() {
  emulate -L zsh
  setopt extendedglob
  typeset MATCH
  LBUFFER="${LBUFFER%%(#m)[^[:blank:]]#}${myAbbrev[${MATCH}]:-${MATCH}}${KEYS}"
}
zle -N my-expand-abbrev
bindkey " " my-expand-abbrev

# For GNU screen
if [ "$TERM" = "screen" ]; then
  chpwd () { echo -n "_`dirs`\\" }
  preexec() {
    emulate -L zsh
    local -a cmd; cmd=(${(z)2})
    case $cmd[1] in
      fg)
      if (( $#cmd == 1 )); then
        cmd=(builtin jobs -l %+)
      else
        cmd=(builtin jobs -l $cmd[2])
      fi
      ;;
      %*)
      cmd=(builtin jobs -l $cmd[1])
      ;;
      cd)
      if (( $#cmd == 2)); then
        cmd[1]=$cmd[2]
      fi
      ;&
      *)
      echo -n "k$cmd[1]:t\\"
      return
      ;;
    esac

    local -A jt; jt=(${(kv)jobtexts})

    $cmd >>(read num rest
    cmd=(${(z)${(e):-Â¥$jt$num}})
    echo -n "k$cmd[1]:t\\") 2>/dev/null
  }
  chpwd
fi

if [ "$TERM" = "screen" ]; then
  precmd(){
    screen -X title $(basename $(print -P "%~"))
  }
fi

# Aliases
setopt aliases
alias emacs='/opt/local/bin/emacs'
alias l='gls -atlFh --color=auto'
alias ls='gls -atlFh --color=auto'
alias la='gls -aFh --color=auto'
alias lla='gls -laFh --color=auto'
alias ll='ls | head'
alias x='exit'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
alias dirs='dirs -v'
alias pd='popd'
alias ud='cd ../'
alias r='R'
alias S='screen -R'
alias less='lv'
alias e='emacs'
alias g++='g++-mp-4.7'
alias gcc='gcc-mp-4.7'
alias irb='pry'
alias grep='grep --color -n'

# gtest
# GTEST_COLOR=1
# export GTEST_COLOR

# rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

# zaw-zsh
# cd workspace && git clone https://github.com/nakamuray/zaw.git

# zaw-src-zsh
# http://d.hatena.ne.jp/shiba_yu36/20120130/1327937835
source $HOME/workspace/dotfiles/zaw/zaw.zsh
zstyle ':filter-select' case-insensitive yes
bindkey '^@' zaw-cdr
