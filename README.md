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
daemon), e.g. on RHEL. One-time root prerequisites on such a host:

- Add `<user>:100000:65536` to `/etc/subuid` and `/etc/subgid` (rootless
  podman). `newuidmap`/`newgidmap` from the shadow package must be setuid
  (already the case on RHEL).

```console
$ home-manager switch --flake .#homeConfigurations.atsoukka
# or
$ make "switch atsoukka"
```

### Update dependencies

```console
$ nix flake update                              # all inputs
$ nix flake lock --update-input nixpkgs         # a single input
```

### Check and format

```console
$ make check      # nix flake check
$ make format     # format all *.nix with nixfmt
```

Run `make help` to list all targets.

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
