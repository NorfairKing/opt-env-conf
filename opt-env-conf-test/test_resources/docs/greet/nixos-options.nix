{ lib }:
with lib;
with types;
{
  greeting = mkOption {
    description = "Greeting to use";
    type = types.oneOf [
      types.anything
      types.str
    ];
  };
  polite = mkOption {
    description = "Whether to be polite";
    type = types.oneOf [
      types.anything
      types.bool
    ];
  };
}
