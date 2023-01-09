{ buildGoPackage, go_1_18, go-bindata, rake, zip, micromamba, src, version }:

(buildGoPackage.override { go = go_1_18; }) rec {
  name = "rcc-${version}";
  goPackagePath = "github.com/robocorp/rcc";
  nativeBuildInputs = [ go-bindata rake zip ];
  propagatedBuildInputs = [ micromamba ];
  goDeps = ./deps.nix;
  postPatch = ''
    source $stdenv/setup
    substituteInPlace Rakefile --replace "\$HOME/go/bin/" ""
    # Fix issue where rcc holotree variables did unset bash prompt
    substituteInPlace conda/activate.go \
      --replace 'if !ok {' 'if !ok && key != "PS1" {'
    rake assets
  '';
  inherit src version;
}
