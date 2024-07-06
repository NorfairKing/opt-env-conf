 function _opt_env_conf_completion_example_executable
    set -l cl (commandline --tokenize --current-process)
    # Hack around fish issue #3934
    set -l cn (commandline --tokenize --cut-at-cursor --current-process)
    set -l cn (count $cn)
    set -l tmpline --query-opt-env-conf-completion --completion-enriched --completion-index $cn
    for arg in $cl
      set tmpline $tmpline --completion-word $arg
    end
    for opt in (/usr/bin/example-executable $tmpline)
      if test -d $opt
        echo -E "$opt/"
      else
        echo -E "$opt"
      end
    end
end

complete --no-files --command /usr/bin/example-executable --arguments '(_opt_env_conf_completion_example_executable)'
