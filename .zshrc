# Hist config
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
# Edit style (emacs)
bindkey -e

zstyle :compinstall filename '/home/rafael049/.zshrc'

#-------------------
#-- Auto complete --
#-------------------
# active
autoload -Uz compinit
compinit
# complete aliases
setopt COMPLETE_ALIASES
# complete for sudo commands
zstyle ':completion::complete:*' gain-privileges 1

#-----------------
#-- History Search
#-----------------
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

#-----------------
#-- Theme --------
#-----------------
autoload -Uz promptinit
promptinit
# prompt colors
# information at
# http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
PROMPT='%F{#71bfff}%B%1d $ %b%f'

#-------------------------------
#-- Remember last directories --
#-------------------------------
autoload -Uz add-zsh-hook

DIRSTACKFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/dirs"
if [[ -f "$DIRSTACKFILE" ]] && (( ${#dirstack} == 0 )); then
	dirstack=("${(@f)"$(< "$DIRSTACKFILE")"}")
	[[ -d "${dirstack[1]}" ]] && cd -- "${dirstack[1]}"
fi
chpwd_dirstack() {
	print -l -- "$PWD" "${(u)dirstack[@]}" > "$DIRSTACKFILE"
}
add-zsh-hook -Uz chpwd chpwd_dirstack

DIRSTACKSIZE='20'

setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME

## Remove duplicate entries
setopt PUSHD_IGNORE_DUPS

## This reverts the +/- operators.
setopt PUSHD_MINUS

#-----------------------
#-- Shell from ranger --
#-----------------------
cd "$AUTOCD"


#----------------------
#-- Syntax Highlight --
#----------------------
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#----------------------
#-- Auto Suggestions --
#----------------------
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh


#-------------
#-- Aliases --
#-------------
alias ls='exa -la --group-directories-first'
alias rm='rm -i'
alias rmm='rmtrash'
alias mv='mv -i'
alias chmod='chmod --preserve-root'
alias reboot='sudo systemctl reboot now'
alias suspend='sudo systemctl suspend'
alias py='python3'
alias ghc='stack ghc'
alias ghci='stack ghci'
alias runhaskell='stack runhaskell'

alias xmc='vim ~/.xmonad/xmonad.hs ~/.config/xmobar/xmobarrc -p'

#-----------
#-- Paths --
#-----------
PROG="/media/Rafael/Programacao/"
RAF="/media/Rafael/"
UNI="/media/Rafael/Unifei"
PER="/media/Rafael/Unifei/7º Periodo"

#--------------------
#-- Default Editor --
#--------------------
export VISUAL="/usr/bin/nvim"
export EDITOR="$VISUAL"
#--------------------
#-- Default Pager  --
#--------------------
export PAGER="nvimpager"
#--------------
#-- Qt theme --
#--------------
export QT_QPA_PLATFORMTHEME=qt5ct
