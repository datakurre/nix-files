# River — Wayland desktop environment

River is a dynamic tiling Wayland compositor with a keyboard-driven workflow,
replacing the XMonad/X11 setup on NixOS machines.

## How to tell you're on Wayland

```sh
echo $XDG_CURRENT_DESKTOP   # should output "river"
echo $XDG_SESSION_TYPE       # should output "wayland"
echo $WAYLAND_DISPLAY         # should be set (e.g. "wayland-0")
```

Firefox `about:support` → Graphics → "Window Protocol" should say "wayland".

## Tools

| Role | Tool | Notes |
|---|---|---|
| Terminal | **foot** | `Ctrl+Shift+C` copy, `Ctrl+Shift+V` paste, middle-click paste from primary selection |
| Launcher | **fuzzel** | `Super+Shift+X` or `Super+P` — app/drun launcher |
| Bar | **waybar** | Top bar: tags (left), pulseaudio / battery / tray icons (right) |
| Lock screen | **swaylock** | `Super+F12` (Favorites key) locks; also after 10 min idle or before suspend |
| Night light | **gammastep** | Auto color temperature; config in `modules/home/services-gammastep.nix` |
| Clipboard | **wl-clipboard** | `wl-copy` / `wl-paste` from scripts; GUI apps share clipboard via wayland protocols |
| Screen layout | **kanshi** | Auto-apply HiDPI scale per output; config per machine `manual.nix` |
| Notification | none by default | `notify-send` needs a daemon (e.g. `services.mako.enable = true`); the battery-notifier uses it |

## Layout engine — rivertile

River itself is a compositor; window layout is handled by `rivertile`, a
separate layout generator started from `~/.config/river/init`. It provides a
**master–stack** layout (one "master" window on the left, remaining windows
stacked on the right):

- Default: master at 50 % width, 1 master window
- `Super+H` / `Super+L` — shrink/expand master area by 1 %
- `Super+Shift+A` / `Super+Shift+Z` — increase/decrease master count
- `Super+Space` — rotate layout direction (left → top → right → bottom)
- `Super+Shift+Space` — reset layout to defaults

The layout helpers are separate scripts at `/run/current-system/sw/bin/river-layout-rotate`
and `river-layout-reset`.

## Keybindings

Super = Windows/Mod4 key.

### Window management

| Key | Action |
|---|---|
| `Super+Shift+Return` | Open terminal (foot) |
| `Super+Shift+X` | App launcher (fuzzel) |
| `Super+P` | App launcher (fuzzel) |
| `Super+Shift+C` | Close focused window |
| `Super+Shift+Q` | Exit river (logout) |
| `Super+J` / `K` | Focus next/previous window |
| `Super+Tab` | Focus next window |
| `Super+Return` | Swap focused window to master (zoom) |
| `Super+Shift+J` / `K` | Swap window with next/previous |
| `Super+T` | Toggle floating |
| `Super+F` | Toggle fullscreen |
| `Super+Shift+F` | Toggle floating (alt binding) |

### Layout

| Key | Action |
|---|---|
| `Super+H` / `L` | Shrink/expand master area (±1 %) |
| `Super+Shift+H` / `L` | Shrink/expand master area (±1 %) (mirror axis) |
| `Super+Shift+A` / `Z` | Increase/decrease master count |
| `Super+,` / `Super+.` | Increase/decrease master count |
| `Super+Space` | Cycle layout direction (left→top→right→bottom) |
| `Super+Shift+Space` | Reset layout to defaults |

### Workspaces (tags)

River uses tags (bitmask): a window can be on multiple tags simultaneously.

| Key | Action |
|---|---|
| `Super+1` … `9` | Switch to tag 1–9 |
| `Super+Shift+1` … `9` | Move focused window to tag 1–9 |
| `Super+Ctrl+1` … `9` | Toggle tag 1–9 on focused window |
| `Super+Shift+Ctrl+1` … `9` | Toggle view of tag 1–9 |

### Multi-monitor

| Key | Action |
|---|---|
| `Super+W` | Focus next output (screen) |
| `Super+Shift+W` | Move window to next output |

### Media keys

| Key | Action |
|---|---|
| Brightness ↑↓ | `brightnessctl set 5%+` / `5%-` |
| Volume ↑↓ | `amixer set Master 5%+` / `5%- unmute` |
| Volume mute | `amixer set Master toggle` |
| Mic mute | `amixer set Capture toggle` |

### System

| Key | Action |
|---|---|
| `Favorites` (XF86Favorites) | Lock screen (swaylock) |
| `Cancel` (break/pause) | Suspend |

### Mouse

| Mouse button | Action |
|---|---|
| `Super + left button` + drag | Move floating window |
| `Super + right button` + drag | Resize floating window |

Pointer devices are configured from `~/.config/river/init`. The Logitech
trackball uses button-scroll (hold `BTN_SIDE` and roll). On **makondo** and **atsoukka**,
it expects the host OS to remap the small buttons to `BTN_TASK` via a udev hwdb rule,
and uses `BTN_TASK` for scrolling instead to prevent accidental "Back" navigation in
browsers. On standalone Home Manager hosts (like **atsoukka**), this udev rule must
be installed manually:

```ini
# /etc/udev/hwdb.d/99-logitech-trackball.hwdb
evdev:name:Logitech USB Trackball:*
 KEYBOARD_KEY_90004=btn_task
 KEYBOARD_KEY_90005=btn_task
```
*(Run `sudo systemd-hwdb update && sudo udevadm trigger` to apply it.)*

To apply phantom click preventions on standalone hosts, install `interception-tools`
on the host system and configure `udevmon` to use the `evdev-debounce` binary
provided by this flake:

```yaml
# /etc/interception/udevmon.yaml
- JOB: "intercept -g $DEVNODE | /home/atsoukka/.nix-profile/bin/evdev-debounce | uinput -d $DEVNODE"
  DEVICE:
    NAME: "Logitech USB Trackball"
```
*(Run `sudo systemctl enable --now udevmon` to apply it.)*

Additionally, on **makondo** and **atsoukka** every
other pointer and touch device is muted with `riverctl input <dev> events
disabled` — the touchpad, its trackpoint node, the ELAN touchscreen, a phantom
`PS/2 Generic Mouse` and the Ergodox's spurious pointer endpoints all inject
stray motion otherwise. List what the compositor sees with
`riverctl list-inputs`.

## Configuration

### Session (bindings, tool startup, layout)

`~/.config/river/init` — generated by `modules/home/services-river.nix` from
this repo. Edit the Nix module and rebuild to change keybindings.

### Terminal, launcher, bar, lock

All configured via Home Manager module options in
`modules/home/services-river.nix`:
- `programs.foot` — terminal (font, colors)
- `programs.fuzzel` — launcher (font, theme)
- `programs.waybar` — top bar (modules, style)
- `programs.swaylock` — lock screen (colors)
- `services.swayidle` — idle timeout (seconds) and lock on sleep

### Night light (gammastep)

`modules/home/services-gammastep.nix` — latitude, longitude, color temperatures,
brightness. If gammastep looks too bright or dim compared to the old redshift,
adjust `brightness-day` and `brightness-night`.

### Screen layout (HiDPI)

Per-machine in `machines/<host>/manual.nix` via `services.kanshi`:
```nix
home-manager.users.${config.user.name}.services.kanshi = {
  enable = true;
  settings = [{
    profile.name = "internal";
    profile.outputs = [{
      criteria = "eDP-1";
      scale = 2.0;
    }];
  }];
};
```
Run `wlr-randr` to discover output names if the connector name is wrong. Get
this wrong and *nothing* happens: kanshi silently applies no profile and every
output stays at scale 1, which looks like "HiDPI is broken for half my apps".
Check the connectors the kernel actually sees with
`grep -l '^connected$' /sys/class/drm/card*-*/status`; internal panels are `eDP-1`,
and an unconnected `DP-1` often exists alongside it.

### Tray applets

Started from `~/.config/river/init`:
- `nm-applet --indicator` (Wi-Fi)
- `blueman-applet` (Bluetooth)
- `pasystray` (volume per application)

Their icons appear in the waybar tray module (top right of the bar).

## Troubleshooting

### Swaylock fails to unlock with Yubikey on standalone hosts (RHEL/SELinux)

On NixOS machines like **makondo**, Yubikey authentication for swaylock is configured declaratively. On standalone hosts like **atsoukka**, swaylock uses host PAM and SELinux policy, so `/etc/pam.d/swaylock` must be configured manually.

Use the same model that already works for `xsecurelock` on this host:

```pam
# /etc/pam.d/swaylock
#%PAM-1.0
auth       sufficient   pam_u2f.so     authfile=/etc/pam.d/u2f_keys cue
auth       include      system-auth
account    required     pam_permit.so
```

Host prerequisites:

```sh
sudo dnf install -y pam-u2f
```

- Enroll key mappings into `/etc/pam.d/u2f_keys` (same file used by `xsecurelock`).
- Keep fallback (`auth include system-auth`) to avoid lockout if key is missing.

SELinux checks on RHEL:

```sh
sudo restorecon -v /etc/pam.d/swaylock /etc/pam.d/u2f_keys
sudo ls -lZ /etc/pam.d/swaylock /etc/pam.d/u2f_keys
```

If unlock still fails, inspect AVC denials:

```sh
sudo ausearch -m AVC -ts recent | grep -Ei 'pam_u2f|swaylock|u2f' || true
sudo journalctl -t setroubleshoot --since "10 min ago" --no-pager
```

This setup keeps U2F-first behavior while preserving password fallback.

### Screen locks surprisingly

On River hosts, the screen is normally locked after 10 minutes of inactivity via `swayidle` (configured in `modules/home/services-river.nix`).

For the standalone Home Manager profile (`switch atsoukka`), `swayidle` is currently **temporarily disabled** in `home-configuration.nix` because activity-reset behavior has been intermittently incorrect on that host/session stack.

What was observed during probing on `atsoukka`:

- timeout callbacks can fire even while keyboard/pointer activity is ongoing
- `swayidle -S seat0` reports `Seat seat0 not found` although logind seat is `seat0`
- this points more to seat/activity integration (River/wlroots/session stack) than to `swaylock` itself

So yes, `swaylock` runs in user space, but that is unlikely to be the root cause here: `swaylock` is only executed after `swayidle` decides the session is idle.

When re-enabling `swayidle` for standalone hosts, validate in this order:

1. Confirm session environment is Wayland-native:
   ```sh
   echo "$XDG_SESSION_TYPE $XDG_CURRENT_DESKTOP $WAYLAND_DISPLAY"
   systemctl --user show-environment | grep -E 'WAYLAND_DISPLAY|XDG_CURRENT_DESKTOP'
   ```
2. Confirm idle daemon runs in user session and is not crash-looping:
   ```sh
   systemctl --user status swayidle --no-pager
   journalctl --user -u swayidle -b --no-pager
   ```
3. Verify lock path independently before idle path:
   ```sh
   swaylock -f
   ```
4. Then verify idle-triggered lock and before-sleep lock:
   ```sh
   loginctl lock-session
   systemctl suspend
   ```

### River fails on proprietary NVIDIA (`ERROR_INCOMPATIBLE_DRIVER` / EGL errors)

If River exits with:

`error(wlroots): ... Could not create instance: ERROR_INCOMPATIBLE_DRIVER (-9)`

wlroots failed renderer initialization on the current NVIDIA stack.
Common signatures include:

- `ERROR_INCOMPATIBLE_DRIVER (-9)`
- `EGL_EXT_platform_base not supported`
- `RendererCreateFailed`

On RHEL 9 GDM, use a system session entry and keep `Exec` pointed at the
managed wrapper:

```ini
# /usr/share/wayland-sessions/river.desktop
Exec=/home/atsoukka/.nix-profile/bin/river-session
```

The wrapper prefers `nixGLNvidia` on NVIDIA hosts when available, starts with
`WLR_RENDERER=${WLR_RENDERER:-gles2}`, and retries with `pixman` if startup
fails immediately.

To force software rendering temporarily, set:

```ini
Exec=env WLR_RENDERER=pixman /home/atsoukka/.nix-profile/bin/river-session
```

Restart GDM (or reboot), then retry.

Verify you are in a Wayland session before deeper debugging:

```sh
echo $XDG_SESSION_TYPE   # expect: wayland
echo $WAYLAND_DISPLAY    # expect: wayland-*
```

Collect logs:

```sh
sudo journalctl -b -u gdm --no-pager
journalctl --user -b --no-pager
journalctl -b --no-pager | grep -Ei 'river|wlroots|wayland|vulkan|nvidia'
```

### Firefox / GUI apps look tiny

Scaling is owned entirely by kanshi (see above). `GDK_SCALE` and
`QT_SCALE_FACTOR` are deliberately **not** set anywhere: native Wayland clients
read the scale from the compositor, and forcing those variables on top makes
them scale twice.

Check the compositor is actually reporting a scale before blaming the app:
```
wlr-randr | grep -A3 eDP-1   # expect "Scale: 2.000000"
```
If it says 1, the kanshi profile is not matching — fix `criteria`, then
`journalctl --user -u kanshi -b`.

If the scale is right and one app is still tiny, it is on XWayland, which
wlroots does not scale. Verify Firefox: `about:support` → "Window Protocol"
should be "wayland"; if it says "x11", run `MOZ_ENABLE_WAYLAND=1 firefox` from
the terminal. `layout.css.devPixelsPerPx` is a last resort and must be cleared
again once the compositor scale is correct, or Firefox ends up at 4x.

Qt apps launched from systemd user services (notably `pinentry-qt` from
gpg-agent) pick XWayland when they start before the river init imports
`WAYLAND_DISPLAY` into the user manager. The init restarts gpg-agent after the
import to close that race; check with
`systemctl --user show-environment | grep WAYLAND_DISPLAY`.

### Tray icons missing

Waybar is started directly from `~/.config/river/init`, not via systemd.
Check it's running:

```sh
pgrep -a waybar                 # should show waybar
pgrep -a nm-applet              # should show nm-applet --indicator
pgrep -a blueman-applet         # should show blueman-applet
```

If waybar isn't running, check stderr: run `waybar` manually from a
terminal. Common issues: missing river/tags module (waybar built without
river support) or Wayland display not set.

If waybar IS running but no tray icons, restart it:
```sh
pkill waybar; waybar &
```
The tray apps register via DBus, so DBus session must be active (check
`echo $DBUS_SESSION_BUS_ADDRESS`).

### Gammastep too bright

Tune brightness values live without rebuilding:
```sh
# Test daytime: gammastep -m wayland -b 0.85:0.60 -t 5500:3700 -l 25.4449:62.1435 -O 5500
# Reset:        gammastep -x
```

Once satisfied, update `modules/home/services-gammastep.nix`:
```nix
settings.general = {
  brightness-day = "1.0";
  brightness-night = "0.7";
};
```

If gammastep can't be tuned to match redshift, consider switching to
`wlsunset` (simpler, only adjusts color temperature, no fake brightness).

### terminal font

Foot font is configured in `modules/home/services-river.nix`:
```nix
programs.foot.settings.main.font = "DejaVu Sans Mono for Powerline:size=9";
```
Adjust `size` up or down, then rebuild and restart foot.

### clipboard between Wayland and XWayland

Plain text copy/paste works out of the box. For images or rich text between
wayland and X11 apps, `wl-clipboard` bridges the gap: `wl-copy` / `wl-paste`.

### screen capture / OBS

Requires PipeWire (not configured in this repo). To enable for OBS:
```nix
services.pipewire = {
  enable = true;
  audio.enable = true;
  pulse.enable = true;
};
```
Then OBS can use the Portal/Wayland capture source.
