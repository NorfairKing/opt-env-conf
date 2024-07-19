{ lib }:
with lib;
with types;
{
  foo.bar.quux = mkOption {
    description = "Example with sub-settings";
    type = types.oneOf [
      types.anything
      types.str
    ];
  };
}
