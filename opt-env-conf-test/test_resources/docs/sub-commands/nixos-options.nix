{ lib }:
with lib;
with types;
{
  name = mkOption {
    description = "name";
    type = types.oneOf [
      types.anything
      types.str
    ];
  };
}
