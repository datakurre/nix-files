# swaylock-xjack on standalone RHEL

This directory contains the xjack patch stack used for `swaylock` in this flake.
On standalone Home Manager hosts (RHEL), use this workflow to build and install
`swaylock` against the host PAM libraries so screen locking can authenticate the
local user account.

## Build and install on host RHEL

From repository root:

```console
cd pkgs/swaylock-xjack
make deps-rhel
make install
make install-pam-service
make auth-check
```

Default install path is `/usr/local/bin/swaylock`.

## Common targets

- `make print-rhel-deps` — show required system packages.
- `make source` — clone upstream swaylock and checkout `SWAYLOCK_REF`.
- `make apply-patches` — apply all patches from `./patches`.
- `make build` — compile into `./build/swaylock-build`.
- `make install` — install to `PREFIX` (default `/usr/local`).
- `make auth-check` — verify installed binary links to PAM.

If patching was interrupted, `make source` resets the source tree and clears
previous patch state before `make apply-patches` runs again.

## Configurable variables

Override Make variables as needed:

```console
make SWAYLOCK_REF=v1.9.0 PREFIX=/opt/swaylock-xjack install
```

Variables:

- `SWAYLOCK_REPO` (default `https://github.com/swaywm/swaylock.git`)
- `SWAYLOCK_REF` (default `v1.8.6`)
- `PREFIX` (default `/usr/local`)

## Standalone River integration

The Home Manager River module now calls a `river-lock` helper for lock keybinds
and `swayidle` lock events. On standalone hosts it defaults to:

`/usr/local/bin/swaylock`

Override at runtime if needed:

```console
export RIVER_STANDALONE_SWAYLOCK=/opt/swaylock-xjack/bin/swaylock
```

If the binary is missing, `river-lock` emits a critical notification and exits
with an error instead of silently skipping locking.
