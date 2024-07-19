{ lib }:
with lib;
with types;
{
  example = mkOption {
    description = "Example of a yes/no switch";
    type = types.oneOf [
      types.anything
      types.bool
    ];
  };
}
