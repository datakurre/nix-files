{ pkgs ? import (fetchTarball {  # NixOS 20.09
    url = "https://github.com/NixOS/nixpkgs/archive/cd63096d6d887d689543a0b97743d28995bc9bc3.tar.gz";
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  }) {}
, python ? "python27"
, pythonPackages ? builtins.getAttr (python + "Packages") pkgs
, requirements ?  ./. + "/requirements-${python}.nix"
, buildInputs ? []
, bash ? pkgs.bash
}:

with builtins;
with pkgs;
with pkgs.lib;

let

  # Requirements for generating requirements.nix
  requirementsBuildInputs = [ cacert nix nix-prefetch-git niv libyaml
                              cyrus_sasl libffi libxml2 libxslt openldap ];
  buildoutPythonPackages = [ "cython" "pillow" "setuptools" "wheel" "pip" ];

  # Load generated requirements
  requirementsFunc = import requirements {
    inherit pkgs;
    inherit (builtins) fetchurl;
    inherit (pkgs) fetchgit fetchhg;
  };

  # List package names in requirements
  requirementsNames = attrNames (requirementsFunc {} {});

  # Return base name from python drv name or name when not python drv
  pythonNameOrName = drv:
    if hasAttr "overridePythonAttrs" drv then drv.pname else drv.name;

  # Merge named input list from nixpkgs drv with input list from requirements drv
  mergedInputs = old: new: inputsName: self: super:
    (attrByPath [ inputsName ] [] new) ++ map
    (x: attrByPath [ (pythonNameOrName x) ] x self)
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
    "Pillow" = "pillow";
    "Pygments" = "pygments";
    "python-ldap" = "ldap";
  };

  # Final overrides to fix issues all the magic above cannot fix automatically
  overrides = self: super: {
    "zc.buildout" = pythonPackages.zc_buildout_nix.overridePythonAttrs(old: {
      name = super."zc.buildout".name;
      pname = super."zc.buildout".pname;
      version = super."zc.buildout".version;
      src = super."zc.buildout".src;
      postInstall = "";
    });
  };

in rec {

  # shell with 'pip2nix' for resolving requirements.txt into requirements.nix
  pip2nix = mkShell {
    buildInputs = requirementsBuildInputs ++ [
      (getAttr 
        ("python" + replaceStrings ["."] [""] pythonPackages.python.pythonVersion)
        (((import ((fetchTarball { # Branch for NixOS 20.09
          url = "https://github.com/nix-community/pip2nix/archive/192878974de06f7b0e7f9c6502d2296b2bea2093.tar.gz";
          sha256 = "1bpi6cxy12d5b47634vywfysdi6l1ica68izgal37qyjpxsinqx5";
        }) + "/release.nix")) { inherit pkgs; }).pip2nix)
      )
      (pythonPackages.python.withPackages(ps: with ps; [
      ] ++ map (name: getAttr name ps) buildoutPythonPackages))
    ];
  };

  # shell with 'buildout' and all packages in requirements.txt
  shell = mkShell {
    buildInputs = requirementsBuildInputs ++ [
      (targetPython.withPackages(ps: with ps; [
        pip wheel
      ] ++ map (name: getAttr name ps) requirementsNames))
    ];
  };

  # buildable env with 'buildout' and all packages in requirements.txt
  env = buildEnv {
    name = "plone-env";
    paths = requirementsBuildInputs ++ [
      (targetPython.withPackages(ps: with ps; [
        pip wheel
      ] ++ map (name: getAttr name ps) requirementsNames))
    ];
  };

  inherit buildPython targetPython;
}
