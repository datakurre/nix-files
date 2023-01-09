{ pkgs, callPackage, buildFHSUserEnv, buildGoPackage, go-bindata, rake, zip, micromamba
, which, cacert, name ? "rcc", runScript ? "rcc", src, version }:

let rcc = callPackage ./rcc.nix {
  inherit buildGoPackage go-bindata rake zip src version;
};

in buildFHSUserEnv {
  targetPkgs = pkgs: (with pkgs; [
    micromamba
    rcc
    # Inject enough dependencies to make robotframework-browser work
    pkgs.alsa-lib
    pkgs.at-spi2-atk
    pkgs.cairo
    pkgs.cups
    pkgs.dbus
    pkgs.expat
    pkgs.glib
    pkgs.libdrm
    pkgs.libudev0-shim
    pkgs.libxkbcommon
    pkgs.mesa
    pkgs.nspr
    pkgs.nss
    pkgs.pango
    pkgs.wayland
    pkgs.xorg.libX11
    pkgs.xorg.libXcomposite
    pkgs.xorg.libXdamage
    pkgs.xorg.libXext
    pkgs.xorg.libXfixes
    pkgs.xorg.libXrandr
    pkgs.xorg.libxcb
  ]);
  inherit name runScript;
}
