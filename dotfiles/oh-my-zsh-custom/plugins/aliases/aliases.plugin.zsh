# :O colorful grep!
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ls='ls -G'

# alias open='gnome-open'
alias uncut='cut --complement'

# colors
alias tree='tree -C'

# kill clients, kill daemon, let em know
# needs `;` after first command because pkill returns 1 if no processes
alias emacskill="pkill emacsclient; emacsclient -e '(kill-emacs)' && echo 'killed emacs daemon'"

alias opsignin='eval $(op signin my)'

## GitHub

alias dev="vc github-iad-devvpn"

alias nr='gpen nr'
