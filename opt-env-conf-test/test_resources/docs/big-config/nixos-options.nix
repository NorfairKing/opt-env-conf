{ lib }:
with lib;
with types;
{
  big = mkOption {
    description = "multi-line config codec explanation, the same option twice.";
    type = types.oneOf [
      types.anything
      (types.attrsOf (types.attrsOf types.s64))
    ];
  };
}
