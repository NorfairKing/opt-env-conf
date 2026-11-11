{ lib, runCommand }:
# What a module gets out of the type of a parser's settings: the settings that
# were defined, nested along the paths that opt-env-conf builds itself.
let
  type = import ../opt-env-conf-test/test_resources/docs/sub-settings/nix-option-type.nix { inherit lib; };
  defined = definition:
    (lib.evalModules {
      modules = [
        { options.value = lib.mkOption { inherit type; }; }
        { value = definition; }
      ];
    }).config.value.__defined;
  cases = {
    "leaves out every setting that was not defined" = {
      definition = { };
      expected = { };
    };
    "keeps a nested setting that was defined" = {
      definition = { foo.bar.quux = "q"; };
      expected = { foo.bar.quux = "q"; };
    };
  };
  failures = lib.filterAttrs (_: c: defined c.definition != c.expected) cases;
in
if failures == { }
then runCommand "nix-option-type-check" { } "touch $out"
else
  throw (builtins.toJSON (lib.mapAttrs
    (_: c: {
      actual = defined c.definition;
      inherit (c) expected;
    })
    failures))
