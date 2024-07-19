{ lib }:
{
  foo = lib.mkOption {
    default = {};
    type = lib.types.submodule {
      options = {
        bar = lib.mkOption {
          default = {};
          type = lib.types.submodule {
            options = {
              quux = lib.mkOption {
                default = null;
                description = "Example with sub-settings";
                type = lib.types.nullOr lib.types.str;
              };
            };
          };
        };
      };
    };
  };
}
