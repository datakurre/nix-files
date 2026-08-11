# nix-files

Personal [NixOS](https://nixos.org/) and [Home Manager](https://github.com/nix-community/home-manager)
configuration, managed as a single [Nix flake](flake.nix). It declaratively
describes whole machines (NixOS) as well as a standalone user environment
(Home Manager) for non-NixOS hosts.

> This is a personal configuration. It is public as a reference — feel free to
> borrow ideas, but expect hardcoded users, hosts and hardware.

## What's inside

- **Desktop:** River dynamic tiling Wayland compositor, greetd + tuigreet
  login manager, waybar panel (tray + tags + battery + audio), nm-applet,
  blueman-applet, pasystray.
- **Shell:** Nushell as the login shell, with bash integration, starship
  prompt and carapace completions.
- **Programs:** Vim, Git (GPG-signed commits), OBS, Minecraft, SSH,
  YubiKey (U2F/PAM) support.
- **Services:** Finnish FineID/VRK root CA, virtualization (libvirt,
  VirtualBox, rootless podman), gammastep, battery notifier.
- **Nix setup:** flakes enabled, channels disabled, weekly GC, store
  auto-optimisation.

## Repository layout

```
flake.nix               Flake entry: inputs, overlays, all configurations
default-configuration.nix   Shared NixOS config used by every machine
home-configuration.nix      Standalone Home Manager config (non-NixOS hosts)
machines/<host>/            Per-machine hardware & host-specific config
modules/nixos/              NixOS modules (env-*, hw-*, programs-*, services-*)
modules/home/               Home Manager modules, shared by both setups
Makefile                    Convenience targets (see `make help`)
default.nix, shell.nix      flake-compat shims for non-flake Nix commands
```

## Machines

| Flake output                     | Host    | Hardware                                  | User       |
| -------------------------------- | ------- | ----------------------------------------- | ---------- |
| `nixosConfigurations.albemuth`   | albemuth | Lenovo ThinkPad X1 Gen 9 (LUKS, IPU6 cam) | `datakurre` |
| `nixosConfigurations.makondo`    | makondo  | Desktop, NVIDIA PRIME offload, LUKS+FIDO2 | `atsoukka`  |
| `homeConfigurations."atsoukka"`  | —        | Standalone Home Manager (any Linux)       | `atsoukka`  |

## Usage

Requirements: Nix with `nix-command` and `flakes` experimental features
enabled (already configured by this repo on NixOS).

### Apply the system configuration (NixOS)

```console
$ sudo nixos-rebuild switch --flake .#albemuth    # on albemuth
$ sudo nixos-rebuild switch --flake .#makondo     # on makondo
```

or via make (note the space in the target name):

```console
$ make "switch albemuth"
$ make "switch makondo"
```

### Apply the user environment (Home Manager, non-NixOS)

The standalone configuration targets a **single-user Nix install** (no
daemon), e.g. on RHEL.

Apply/update it with:

```console
$ home-manager switch --flake .#atsoukka
# or
$ make atsoukka
# or
$ make "switch atsoukka"
```

One-time root prerequisites on such a host:

- Add `<user>:100000:65536` to `/etc/subuid` and `/etc/subgid` (rootless
  podman). `newuidmap`/`newgidmap` from the shadow package must be setuid
  (already the case on RHEL).
- Enable River as a Wayland session in GDM — see
  [Enabling River on RHEL 9 with GDM](#enabling-river-on-rhel-9-with-gdm)
  below.

### Update dependencies

```console
$ nix flake update                              # all inputs
$ nix flake lock --update-input nixpkgs         # a single input
$ ./modules/home/update-devenv.sh                 # update datakurre.devenv extension pin
```

### Check and format

```console
$ make check      # nix flake check
$ make format     # format all *.nix with nixfmt
```

Run `make help` to list all targets.

## Enabling River on RHEL 9 with GDM

RHEL 9 ships GDM with Wayland support enabled by default. polkit and
logind already provide the necessary seat access for wlroots compositors —
no extra system packages are required for River to start.

### 1. Apply the Home Manager config

```console
$ make atsoukka
```

This writes `~/.config/river/init`, `~/.config/foot/`, `~/.config/fuzzel/`,
and `~/.nix-profile/bin/river-session`. Do this **before** logging in to a
River session for the first time.

### 2. River session registration (required for GDM on RHEL 9)

Home Manager writes a user-local session file at
`~/.local/share/wayland-sessions/river.desktop`, but on RHEL 9 GDM the reliable
discovery path is system-wide:

`/usr/share/wayland-sessions/river.desktop`

Create it as root:

```console
$ sudo tee /usr/share/wayland-sessions/river.desktop <<'EOF'
[Desktop Entry]
Name=River
Comment=Dynamic tiling Wayland compositor
Exec=/home/atsoukka/.nix-profile/bin/river-session
Type=Application
DesktopNames=river
EOF
```

> **Note:** The `Exec` path is user-specific, and the wrapper handles NVIDIA
> launch with `nixGLNvidia` when available, with a `pixman` fallback if the
> preferred renderer fails early.

### 3. Log in to River

Restart GDM (or reboot), select **River** from the session menu in GDM, and
log in. River reads `~/.config/river/init` automatically on startup.

To verify you are on Wayland:

```sh
echo $XDG_SESSION_TYPE   # should print "wayland"
echo $WAYLAND_DISPLAY     # should be set (e.g. "wayland-0")
```

### Troubleshooting

**XDG portals** (file picker, screen share) are provided by
`xdg-desktop-portal-wlr` from the Nix profile. The portal daemon is
socket-activated via systemd user session. If portals are not working,
check:

```sh
journalctl --user -u xdg-desktop-portal -b
journalctl --user -u xdg-desktop-portal-wlr -b
```

The `~/.config/xdg-desktop-portal/portals.conf` written by Home Manager
routes River sessions to `wlr;gtk` backends. On RHEL 9, the system's
`xdg-desktop-portal-gnome` serves as the GTK fallback backend.

**NVIDIA (proprietary driver): River does not start**

If logs show EGL/renderer failures such as:
`EGL_EXT_platform_base not supported` or `RendererCreateFailed`, keep the
desktop entry `Exec` as:

```ini
Exec=/home/atsoukka/.nix-profile/bin/river-session
```

The wrapper will:
1. Prefer `nixGLNvidia` on NVIDIA hosts when available.
2. Start with `WLR_RENDERER=${WLR_RENDERER:-gles2}`.
3. Fall back to `WLR_RENDERER=pixman` if startup fails immediately.

To force software rendering for diagnosis, temporarily use:

```ini
Exec=env WLR_RENDERER=pixman /home/atsoukka/.nix-profile/bin/river-session
```

Then restart GDM (or reboot) and try again.

Before debugging renderer issues further, verify River is actually launched
as Wayland (not Xorg):

```sh
echo $XDG_SESSION_TYPE   # must be "wayland"
echo $WAYLAND_DISPLAY    # must be set
```

Useful logs:

```sh
sudo journalctl -b -u gdm --no-pager
journalctl --user -b --no-pager
journalctl -b --no-pager | grep -Ei 'river|wlroots|wayland|vulkan|nvidia'
```

See [river.md](river.md) for keybindings, layout, and further
troubleshooting.

## Home Manager-only differences (`atsoukka`)

Compared to NixOS hosts in this repo:

- `make atsoukka` only updates user-space Home Manager state; it does not
  manage system services, boot, users, PAM, display manager or `/etc`.
- River compositor config, keybindings and user applets are managed by
  Home Manager (`modules/home/services-river.nix`), but the login manager and
  Wayland session registration on RHEL remain manual system configuration.
- The standalone profile still includes the legacy XMonad/X11 session module
  as a fallback, while River is available as a Wayland session.
- dconf/gsettings management is disabled (`dconf.enable = false`) because
  the dconf D-Bus service is not set up by standalone Home Manager on
  non-NixOS hosts. GTK HiDPI scaling from dconf is therefore not managed;
  the Wayland compositor scale (kanshi) handles HiDPI instead.
- `services.swayidle` is intentionally disabled for standalone `atsoukka`
  until seat/activity behavior is reliable on that stack; re-enable only after
  following the validation steps documented in [river.md](river.md#screen-locks-surprisingly).

## Adding a new machine

1. Generate hardware config on the target host:
   `nixos-generate-config --show-hardware-config > machines/<host>/generated.nix`
2. Create `machines/<host>/manual.nix` with host-specific settings
   (hostname, bootloader, LUKS devices, …) and `machines/<host>/default.nix`
   importing both.
3. Add a `nixosConfigurations.<host>` output in [flake.nix](flake.nix),
   setting `user.name`, `user.description` and `user.home`.
4. Verify with `nix flake check`, then deploy with
   `nixos-rebuild switch --flake .#<host>`.
