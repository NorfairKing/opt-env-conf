{ lib }:
{
  big = lib.mkOption {
    default = null;
    description = "big configuration object";
    type = lib.types.nullOr (lib.types.submodule {
      options = {
        map = lib.mkOption {
          default = null;
          type = lib.types.nullOr (lib.types.attrsOf (lib.types.attrsOf lib.types.int));
        };
        sub = lib.mkOption {
          default = null;
          type = lib.types.nullOr lib.types.str;
        };
      };
    });
  };
}
