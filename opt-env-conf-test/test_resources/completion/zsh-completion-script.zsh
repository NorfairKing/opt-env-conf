#compdef example-executable

local request
local completions
local word
local index=$((CURRENT - 1))

request=(--query-opt-env-conf-completion --completion-enriched --completion-index $index)
for arg in ${words[@]}; do
  request=(${request[@]} --completion-word $arg)
done

IFS=$'\n' completions=($( /usr/bin/example-executable "${request[@]}" ))

for word in $completions; do
  local -a parts

  # Split the line at a tab if there is one.
  IFS=$'\t' parts=($( echo $word ))

  if [[ -n $parts[2] ]]; then
     if [[ $word[1] == "-" ]]; then
       local desc=("$parts[1] ($parts[2])")
       compadd -d desc -- $parts[1]
     else
       local desc=($(print -f  "%-019s -- %s" $parts[1] $parts[2]))
       compadd -l -d desc -- $parts[1]
     fi
  else
    compadd -f -- $word
  fi
done
