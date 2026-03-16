{ runCommand
, writeShellApplication
, ...
}:

let
  completionTestHarness = ''
    passes=0
    fails=0
    assert_contains() {
      local label="$1"; shift
      local haystack="$1"; shift
      local needle="$1"; shift
      if printf '%s' "$haystack" | grep -qF -- "$needle"; then
        passes=$((passes + 1))
      else
        echo "FAIL: $label: expected to find $(printf '%q' "$needle") in output:"
        echo "$haystack"
        fails=$((fails + 1))
      fi
    }
    assert_not_contains() {
      local label="$1"; shift
      local haystack="$1"; shift
      local needle="$1"; shift
      if printf '%s' "$haystack" | grep -qF -- "$needle"; then
        echo "FAIL: $label: expected NOT to find $(printf '%q' "$needle") in output:"
        echo "$haystack"
        fails=$((fails + 1))
      else
        passes=$((passes + 1))
      fi
    }
    assert_empty() {
      local label="$1"; shift
      local value="$1"; shift
      if [ -z "$value" ]; then
        passes=$((passes + 1))
      else
        echo "FAIL: $label: expected empty output but got:"
        echo "$value"
        fails=$((fails + 1))
      fi
    }
    finish() {
      echo ""
      echo "Results: $passes passed, $fails failed"
      if [ "$fails" -gt 0 ]; then
        exit 1
      fi
    }
  '';

  # File path completion tests.
  #
  # We cannot test the shell's trailing-space behaviour (i.e. whether the
  # shell appends a space after a completed file name) because that is
  # handled by the shell's interactive line editor (readline/ZLE/fish),
  # which requires a PTY-based test harness.
  #
  # What we *can* test is the convention that the completion engine uses to
  # signal the difference to the shell: directory completions end in "/" and
  # file completions do not.  Bash (with `-o filenames`), zsh (`compadd -f`)
  # and fish all use this trailing "/" to decide whether to add a space.
  fileCompletionTests = ''
    echo "Test: file path completion (directories end in /, files do not)"
    TESTDIR=$(mktemp -d)
    mkdir -p "$TESTDIR/mysubdir"
    touch "$TESTDIR/myfile.txt"
    pushd "$TESTDIR" > /dev/null
    OUT=$(query 2 --completion-word prog --completion-word --config-file --completion-word "")
    popd > /dev/null
    assert_contains "file in file completion" "$OUT" "myfile.txt"
    assert_not_contains "file has no trailing slash" "$OUT" "myfile.txt/"
    assert_contains "directory in file completion" "$OUT" "mysubdir/"
    rm -rf "$TESTDIR"

    echo "Test: directory path completion (only directories, all end in /)"
    TESTDIR=$(mktemp -d)
    mkdir -p "$TESTDIR/basedir"
    touch "$TESTDIR/basefile.txt"
    pushd "$TESTDIR" > /dev/null
    OUT=$(query 2 --completion-word prog --completion-word --base --completion-word "")
    popd > /dev/null
    assert_contains "directory in dir completion" "$OUT" "basedir/"
    assert_not_contains "file excluded from dir completion" "$OUT" "basefile.txt"
    rm -rf "$TESTDIR"
  '';

  makeCompletionCheckScript = name: exe: enriched: shell:
    let
      enrichedFlag = if enriched then "--completion-enriched" else "";
      isBashIntegration = !enriched && shell == "bash";
    in
    writeShellApplication {
      inherit name;
      text =
        if isBashIntegration then ''
          ${completionTestHarness}

          BINARY="${exe}"
          SCRIPT=$("$BINARY" --bash-completion-script "$BINARY")

          complete_bash() {
            local index="$1"; shift
            local words=("$@")
            bash -c "
              eval $(printf '%q' "$SCRIPT")
              COMP_WORDS=($(printf '%q ' "''${words[@]}"))
              COMP_CWORD=$index
              _opt_env_conf_completion_opt_env_conf_example
              printf '%s\n' \"\''${COMPREPLY[@]}\"
            "
          }

          # We also need query() for the file completion tests
          query() {
            local index="$1"; shift
            "$BINARY" --query-opt-env-conf-completion \
              --completion-index "$index" "$@"
          }

          echo "=== Bash completion integration tests ==="

          echo "Test: command listing"
          OUT=$(complete_bash 1 prog "")
          assert_contains "create in commands" "$OUT" "create"
          assert_contains "read in commands" "$OUT" "read"
          assert_contains "update in commands" "$OUT" "update"
          assert_contains "delete in commands" "$OUT" "delete"

          echo "Test: command prefix filtering"
          OUT=$(complete_bash 1 prog cr)
          assert_contains "create with prefix cr" "$OUT" "create"
          assert_not_contains "read with prefix cr" "$OUT" "read"

          echo "Test: options after command"
          OUT=$(complete_bash 2 prog create "")
          assert_contains "--file after create" "$OUT" "--file"

          echo "Test: no completions for unknown command"
          OUT=$(complete_bash 2 prog unknown "")
          assert_empty "unknown command" "$OUT"

          ${fileCompletionTests}

          finish
        ''
        else ''
          ${completionTestHarness}

          BINARY="${exe}"

          query() {
            local index="$1"; shift
            "$BINARY" --query-opt-env-conf-completion ${enrichedFlag} \
              --completion-index "$index" "$@"
          }

          echo "=== Completion tests (${name}) ==="

          echo "Test: command listing"
          OUT=$(query 1 --completion-word prog)
          assert_contains "create in commands" "$OUT" "create"
          assert_contains "read in commands" "$OUT" "read"
          assert_contains "update in commands" "$OUT" "update"
          assert_contains "delete in commands" "$OUT" "delete"

        '' + (if enriched then ''
          echo "Test: enriched output has descriptions"
          assert_contains "create description" "$OUT" "$(printf 'create\tCreate')"
          assert_contains "read description" "$OUT" "$(printf 'read\tRead')"
        '' else "") + ''

          echo "Test: prefix filtering"
          OUT=$(query 1 --completion-word prog --completion-word cr)
          assert_contains "create with prefix cr" "$OUT" "create"
          assert_not_contains "read with prefix cr" "$OUT" "read"

          echo "Test: options after command"
          OUT=$(query 2 --completion-word prog --completion-word create)
          assert_contains "--file after create" "$OUT" "--file"

          echo "Test: prefix filtering for de"
          OUT=$(query 1 --completion-word prog --completion-word de)
          assert_contains "delete with prefix de" "$OUT" "delete"
          assert_not_contains "create with prefix de" "$OUT" "create"

          ${fileCompletionTests}

          finish
        '';
    };

  makeCompletionCheck = name: exe: enriched: shell: runCommand name { } ''
    ${makeCompletionCheckScript name exe enriched shell}/bin/${name} > "$out"
  '';

in
{
  inherit makeCompletionCheckScript makeCompletionCheck;
}
