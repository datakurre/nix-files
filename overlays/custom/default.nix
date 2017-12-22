self: super:

{
  afew = super.pythonPackages.afew.overrideAttrs(old: {
    postPatch = ''
      sed -i "s|'notmuch', 'new'|'test', '1'|g" afew/MailMover.py
    '';
  });

  aspellDicts = super.recurseIntoAttrs (super.callPackages ./aspell/dictionaries.nix {});

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
