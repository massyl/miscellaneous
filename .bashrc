# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


# some intelligent method to set the prompt
prompt_command () {
    RET=$?
    if [ $RET -eq 0 ]; then # set an error string for the prompt, if applicable
        ERRPROMPT=""
    else
        ERRPROMPT='($RET) '
    fi

    if [ "\$(type -t __git_ps1)" ]; then # if we're in a Git repo, show current branch
        BRANCH=" \$(__git_ps1 '(%s) ')"
    fi

    # setup preferences
   # local TIME=`fmt_time` # format time for prompt string
   # local LOAD=`uptime|awk '{min=NF-2;print $min}'`
    # set the titlebar to the last 2 fields of pwd
    local TITLEBAR='\[\e]2;`pwdtail`\a\]'

    # colors
    local GREEN="\[\033[0;32m\]"
    local DKGREEN="\[\033[1;32m\]"
    local CYAN="\[\033[0;36m\]"
    local DKCYAN="\[\033[1;36m\]"
    local BLUE="\[\033[0;34m\]"
    local DKBLUE="\[\033[1;34m\]"
    local GRAY="\[\033[0;37m\]"
    local DKGRAY="\[\033[1;30m\]"
    local WHITE="\[\033[1;37m\]"
    local RED="\[\033[0;31m\]"
    local DKRED="\[\033[1;31m\]"

    # return color to Terminal setting for text color
    local DEFAULT="\[\033[0;39m\]"
    # delete ${TITLEBAR} because it doesn't work inside the shell emacs or in the tty screen
    #export PS1="${DKBLUE}\u${DKBLUE}@${DKBLUE} \h${DKCYAN}(${LOAD}) ${WHITE}${TIME} ${DKRED}$ERRPROMPT${DKBLUE} \w${DKGREEN}${BRANCH}${DEFAULT}$ "
    export PS1="${DKBLUE}\u${DKBLUE}@${DKBLUE} ${DKRED}$ERRPROMPT${DKBLUE} \w${DKGREEN}${BRANCH}${DEFAULT}$ "
}


#if [ "$color_prompt" = yes ]; then
 #   PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#else
 #   PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
   # PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
   prompt_command
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias glsdrm='git ls-files -d|xargs git rm'
alias grs='git remote show origin'
#alias veewee='bundle exec veewee'
alias gpr='git pull --recruse-submodules'
alias gcr='git clone --recursive'
alias gfa='git fetch --all'
alias gco='git commit -m'
# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


# List all network connection (nm-applet = to have list of available network)
# xev : to get a keycode of keyboard key
# nmcli dev wifi con SSID password pwd name customName
# nmcli dev status
#alias nl = 'nmcli -p con list'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias ldir="ls -l | egrep  '^d'"
alias lf="ls -l | egrep -v '^d'"

#chmod 400  ~/webadeo-wikeo/wikeo/wikeo-deployer/src/main/resource/id_rsa-universal-kp
#chmod 400  ~/dashboard.pem
chmod 400  ~/.ssh/id_rsa

eval $(ssh-agent)

#ssh-add ~/webadeo-wikeo/wikeo/wikeo-deployer/src/main/resource/id_rsa-universal-kp
#ssh-add ~/dashboard.pem
ssh-add ~/.ssh/id_rsa

#export MULTIBYON1="ec2-54-247-145-107.eu-west-1.compute.amazonaws.com"
#export MULTIBYON2="ec2-46-137-18-182.eu-west-1.compute.amazonaws.com"
#export GANGLIA="ec2-46-137-65-168.eu-west-1.compute.amazonaws.com"
#export WSTATION_GANG_METR="ec2-54-247-10-252.eu-west-1.compute.amazonaws.com"
#export WIKEO_PROPERTIES_DIR="file:///home/sfeir/webadeo-wikeo/wikeo/wikeo-properties/local-dev"
#export WIKEO_PROPERTIES_DIR="file:///home/sfeir/webadeo-wikeo/wci/scripts"
#export LUC_REPO_ROOT="repo.wikeo.webadeo.net"


export RUBYLIB=.:$RUBYLIB

alias vm1="vagrant ssh vm1"
alias vm2="vagrant ssh vm2"
alias vm3="vagrant ssh vm3"
alias vm4="vagrant ssh vm4"
alias emc='emacsclient'
alias gfs='git flow feature start'
alias gff='git flow feature finish'

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
export _JAVA_AWT_WM_NONREPARENTING=1
export SCALA_HOME=/home/massyl/softs/scala/scala
PATH=$SCALA_HOME/bin:"/home/massyl/softs/scala/sbt/bin":$HOME/bin:$PATH
PATH=$PATH:"/home/massyl/softs/playframework/play2"
nitrogen --restore
#emacs --daemon

#disables the Menu key
xmodmap -e "keycode 135 = 0xffeb"
xmodmap -e "remove lock = 0x0000"
xmodmap -e "keycode 66 = 0xff0d"
#xmodmap -e "keycode 135 = 0x0000"

#disable the right mouse button
#xmodmap -e 'pointer = 1 2 0 4 5 6 7 8 9'
#restaure the default mouse confirguration
#xmodmap -e 'pointer = default'
