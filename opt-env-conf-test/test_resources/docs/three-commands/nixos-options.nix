{ lib }:
with lib;
with types;
{
  enable = mkOption {
    description = "enable extra";
    type = types.oneOf [
      types.anything
      types.bool
    ];
  };
  number = mkOption {
    description = "number";
    type = types.oneOf [
      types.anything
      types.s64
    ];
  };
}
