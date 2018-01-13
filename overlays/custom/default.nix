self: super:

{
  afew = super.pythonPackages.afew.overrideAttrs(old: {
    postPatch = ''
      sed -i "s|'notmuch', 'new'|'test', '1'|g" afew/MailMover.py
    '';
  });

  aspellDicts = super.recurseIntoAttrs (super.callPackages ./aspell/dictionaries.nix {});

  sikulix = super.callPackage ./sikuli {};

  findimagedupes = super.callPackage ./findimagedupes {};

  zest-releaser-python2 = (super.callPackage ./zest-releaser {
    pythonPackages = self.python2Packages;
  }).build."zest.releaser";

  zest-releaser-python3 = (super.callPackage ./zest-releaser {
    pythonPackages = self.python3Packages;
  }).build."zest.releaser";

  gmime = super.gmime.overrideAttrs(old: {
    propagatedBuildInputs = old.propagatedBuildInputs ++ [ self.gpgme.dev ];
  });

  jetbrains = (super.recurseIntoAttrs (super.callPackages ./jetbrains {
    jdk = self.oraclejdk8;
  }));

  pidgin-with-plugins = super.pidgin-with-plugins.override {
    plugins = [ self.pidginsipe ];
  };
}
