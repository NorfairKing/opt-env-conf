{ lib }:
{
  log-level = lib.mkOption {
    default = null;
    description = "minimal severity of log messages";
    type = lib.types.nullOr lib.types.str;
  };
  payment = lib.mkOption {
    default = {};
    type = lib.types.submodule {
      options = {
        currency = lib.mkOption {
          default = null;
          description = "Currency";
          type = lib.types.nullOr lib.types.str;
        };
        public-key = lib.mkOption {
          default = null;
          description = "Public key";
          type = lib.types.nullOr lib.types.str;
        };
        secret-key = lib.mkOption {
          default = null;
          description = "Secret key";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
}
