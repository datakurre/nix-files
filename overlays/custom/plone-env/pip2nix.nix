{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/b2b5c1d5af2a3e743a388ffb807db64d6b776a5d.tar.gz";
    sha256 = "0zg58h2j55aq8pr5b16h2jlns1215rnc2q9ap4bmdcxcqidm62ra";
  }) {}
, setup ? import (fetchTarball {
    url = "https://github.com/datakurre/setup.nix/archive/a05ef605ae476a07ba1f8b0c2e1ce95d0eca8355.tar.gz";
    sha256 = "0ih9ccy54hcij7z49mfxpyvl1wdsh00kr9714scza9b101s4gpap";
 })
, pythonPackages ? pkgs.python2Packages
}:

let overrides = self: super: {
};

in setup {
  inherit pkgs pythonPackages overrides;
  src = ./requirements.nix;
}
