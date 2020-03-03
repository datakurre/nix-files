{ pkgs ? import (fetchTarball {
    # branches nixos-20.03
    url = "https://github.com/NixOS/nixpkgs-channels/archive/61cc1f0dc07c2f786e0acfd07444548486f4153b.tar.gz";
    sha256 = "1gvfl80vpb2wi4v5qnllfs8z0kzj6bmyk4aqxk6n0z2g41yxpf18";
  }) {}
, python ? "python37"
, pythonPackages ? builtins.getAttr (python + "Packages") pkgs
, requirements ?  ./. + "/requirements-${python}.nix"
}:

with builtins;
with pkgs;
with pkgs.lib;

let

  # Load generated requirements
  requirementsFunc = import requirements {
    inherit pkgs;
    inherit (builtins) fetchurl;
    inherit (pkgs) fetchgit fetchhg;
  };

  # List package names in requirements
  requirementsNames = attrNames (requirementsFunc {} {});

  # Return base name from drv name
  nameFromDrvName = name:
    let parts = tail (split "([0-9]-)" (parseDrvName name).name);
    in if length parts > 0 then elemAt parts 1 else name;

  # Merge named input list from nixpkgs drv with input list from requirements drv
  mergedInputs = old: new: inputsName: self: super:
    (attrByPath [ inputsName ] [] new) ++ map
    (x: attrByPath [ (nameFromDrvName x.name) ] x self)
    (filter (x: !isNull x) (attrByPath [ inputsName ] [] old));

  # Merge package drv from nixpkgs drv with requirements drv
  mergedPackage = old: new: self: super:
    if isString new.src
       && !isNull (match ".*\.whl" new.src)  # do not merge build inputs for wheels
       && new.pname != "wheel"               # ...
    then new.overridePythonAttrs(old: rec {
      propagatedBuildInputs =
        mergedInputs old new "propagatedBuildInputs" self super;
    })
    else old.overridePythonAttrs(old: rec {
      inherit (new) pname version src;
      name = "${pname}-${version}";
      checkInputs =
        mergedInputs old new "checkInputs" self super;
      buildInputs =
        mergedInputs old new "buildInputs" self super;
      nativeBuildInputs =
        mergedInputs old new "nativeBuildInputs" self super;
      propagatedBuildInputs =
        mergedInputs old new "propagatedBuildInputs" self super;
      doCheck = false;
    });

  # Build python with manual aliases for naming differences between world and nix
  buildPython = (pythonPackages.python.override {
    packageOverrides = self: super:
      listToAttrs (map (name: {
        name = name; value = getAttr (getAttr name aliases) super;
      }) (filter (x: hasAttr (getAttr x aliases) super) (attrNames aliases)));
  });

  # Build target python with all generated & customized requirements
  targetPython = (buildPython.override {
    packageOverrides = self: super:
      # 1) Merge packages already in pythonPackages
      let super_ = (requirementsFunc self buildPython.pkgs);  # from requirements
          results = (listToAttrs (map (name: let new = getAttr name super_; in {
        inherit name;
        value = mergedPackage (getAttr name buildPython.pkgs) new self super_;
      })
      (filter (name: hasAttr "overridePythonAttrs"
                     (if (tryEval (attrByPath [ name ] {} buildPython.pkgs)).success
                      then (attrByPath [ name ] {} buildPython.pkgs) else {}))
       requirementsNames)))
      // # 2) with packages only in requirements or disabled in nixpkgs
      (listToAttrs (map (name: { inherit name; value = (getAttr name super_); })
      (filter (name: (! ((hasAttr name buildPython.pkgs) &&
                         (tryEval (getAttr name buildPython.pkgs)).success)))
       requirementsNames)));
      in # 3) finally, apply overrides (with aliased drvs mapped back)
      (let final = (super // (results //
        (listToAttrs (map (name: {
          name = getAttr name aliases; value = getAttr name results;
        }) (filter (x: hasAttr x results) (attrNames aliases))))
      )); in (final // (overrides self final)));
    self = buildPython;
  });

  # Alias packages with different names in requirements and in nixpkgs
  aliases = {
  };

  # Final overrides to fix issues all the magic above cannot fix automatically
  overrides = self: super: {

  # check-manifest requires Python interpreter able to import setup.py
  "check-manifest" =
    let manifest_python = self.python.withPackages(ps: [
      self.setuptools self.wheel
    ]); in super."check-manifest".overridePythonAttrs(old: {
    postPatch = ''
      substituteInPlace check_manifest.py \
        --replace "os.path.abspath(python)" \
                  "\"${manifest_python.interpreter}\""
    '';
    propagatedBuildInputs = old.propagatedBuildInputs ++ [ manifest_python ];
  });

  # building wheels require SOURCE_DATE_EPOCH
  "zest.releaser" = super."zest.releaser".overridePythonAttrs(old: {
    postInstall = ''
      for prog in $out/bin/*; do
        mv $prog $prog-python${pythonPackages.python.pythonVersion}
        wrapProgram $prog-python${pythonPackages.python.pythonVersion} \
          --set SOURCE_DATE_EPOCH 315532800
      done
    '';
  });
  };

in {

  # shell with 'pip2nix' for resolving requirements.txt into requirements.nix
  pip2nix = mkShell {
    buildInputs = [ nix nix-prefetch-git cacert ] ++ [
      (pythonPackages.python.withPackages(ps: with ps; [
        (getAttr python
          ( import (fetchTarball {
              url = "https://github.com/datakurre/pip2nix/archive/7557e61808bfb5724ccae035d38d385a3c8d4dba.tar.gz";
              sha256 = "0rwxkbih5ml2mgz6lx23p3jgb6v0wvslyvscki1vv4hl3pd6jcld";
          } + "/release.nix") { inherit pkgs; }).pip2nix)
      ]))
    ];
  };

  inherit buildPython targetPython;

  # final env with packages in requirements.txt
  env = pkgs.buildEnv {
    name = "env";
    paths = [
      (targetPython.withPackages(ps: map (name: getAttr name ps) requirementsNames))
    ];
  };

}
