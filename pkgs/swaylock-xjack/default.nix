{
  lib,
  swaylock,
}:

swaylock.overrideAttrs (old: {
  pname = "swaylock";
  patches = (old.patches or [ ]) ++ [
    ./patches/0001-add-effect-api.patch
    ./patches/0002-add-xjack-effect.patch
    ./patches/0003-add-xjack-cli-options.patch
    ./patches/0004-incremental-render.patch
    ./patches/0007-restart-on-scroll.patch
  ];
})
