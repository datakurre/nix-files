{
  permittedInsecurePackages = [
    "webkitgtk-2.4.11"
  ];
  allowUnfree = true;
  packageOverrides = pkgs: rec {
    rustRegistry = pkgs.rustRegistry.overrideDerivation(old: {
      src = pkgs.fetchFromGitHub {
        owner = "rust-lang";
        repo = "crates.io-index";
        rev = "65a7439eceaca5ffa02f9198fb38f0a5c4f55e56";
        sha256 = "0rzffj89w7y8dk23fsvdgbjkk6q0mcj8l4fkgsp1y0rxgnzhf1zp";
      };
    });
  };
}
