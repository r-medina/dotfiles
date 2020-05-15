if [[ -z "$SSH_CLIENT" ]]; then
    prompt_host=""
else
    prompt_host="@$(hostname -s)"
fi

PROMPT=$'
%{$fg_bold[green]%}%n$prompt_host%{$fg[blue]%}%D{[%H:%M:%S]}%{$reset_color%}$(battery_pct_prompt) in %{$fg[white]%}[%~]%{$reset_color%}$(git_prompt_info)\
%{$fg[blue]%}->%{$fg_bold[blue]%}%#%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}["
ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
