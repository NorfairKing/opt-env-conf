_opt_env_conf_completion_example_executable()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--query-opt-env-conf-completion)
    CMDLINE+=(--completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --completion-word $arg)
    done

    COMPREPLY=( $(/usr/bin/example-executable "${CMDLINE[@]}") )
    echo "${COMPREPLY[@]}" > hm.log
}

complete -o filenames -F _opt_env_conf_completion_example_executable example-executable
