{
  allowUnfree = true;
  allowBroken = false;
  packageOverrides = pkgs: rec {
    rabbitmq_server = pkgs."rabbitmq-server";
  };
}
