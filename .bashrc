# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias emacs="emacs -nw"

export EDITOR="emacs -nw"

export TERM=tmux-256color
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export HISTSIZE="INFINITE"

#PROMPT_COMMAND='if [ -n "$TMUX" ]; then ps -q $$ -o comm= >> ~/test.txt; fi;'

export PS1='\[\033[32m\]$(pwd | sed "s/Users\/taylorconor/~/") \[\033[00m\]\$ '
export LC_CTYPE="en_US.UTF-8"

bazel() {
    if [[ $1 == "test" || $1 == "build" || $1 == "run" ]]; then
	command bazel $@ --test_output=all --cxxopt='-std=c++14'
    else
	command bazel
    fi
}

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/conor/google-cloud-sdk/path.bash.inc' ]; then . '/Users/conor/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/conor/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/conor/google-cloud-sdk/completion.bash.inc'; fi

# Used to enable bazel in clion on macos.
export DEVELOPER_DIR="/Applications/Xcode.app/Contents/Developer"
export BAZEL_USE_CPP_ONLY_TOOLCHAIN=1
