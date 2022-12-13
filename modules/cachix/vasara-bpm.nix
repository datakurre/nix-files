{
  nix = {
    settings.substituters = [
      "https://nix-community.cachix.org"
    ];
    settings.trusted-public-keys = [
      "vasara-bpm.cachix.org-1:T18iQZQvYDy/6VdGmttnkkq7rYi3JP0S1RjjdnXNu/4="
    ];
    settings.trusted-users = [ "root" "datakurre" "atsoukka" ];
  };
}
