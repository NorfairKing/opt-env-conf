let
  system = "x86_64-linux";
in
{
  cachix = {
    name = "opt-env-conf";
    public-key = "opt-env-conf.cachix.org-1:gkENPxoLqJMYgYsFOCCbA3wr3MkNfN5bdDQPjs4QHlU=";
  };
  deploy = {
    release-to-hackage = {
      package = "packages.${system}.release-to-hackage";
      secrets = [ "HACKAGE_API_KEY" ];
    };
  };
}
