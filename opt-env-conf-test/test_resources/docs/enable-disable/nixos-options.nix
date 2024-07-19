{ lib }:
with lib;
with types;
{
  example = mkOption {
    description = "Example of an enable/disable switch";
    type = types.oneOf [
      types.anything
      types.bool
    ];
  };
}
