{
  nix = {
    settings.substituters = [
      "https://datakurre.cachix.org"
    ];
    settings.trusted-public-keys = [
      "datakurre.cachix.org-1:ayZJTy5BDd8K4PW9uc9LHV+WCsdi/fu1ETIYZMooK78="
    ];
    settings.trusted-users = [ "root" "datakurre" "atsoukka" ];
  };
}
