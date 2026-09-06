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
| Lock screen | **swaylock-xjack** | `Super+F12` locks via `river-lock`; standalone expects host-installed `/usr/local/bin/swaylock` |
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

### Presentation stage

| Key | Action |
|---|---|
| `Super+S` | Send focused window to the stage and follow it |
| `Super+Shift+S` | Return focus to the panel, leave the window on the stage |
| `Super+Ctrl+S` | Show/hide the stage mirror window (`wl-mirror`) |
| `Super+Shift+Ctrl+S` | Re-apply stage geometry (1920x1080) |

See [Presentation output (recording with OBS)](#presentation-output-recording-with-obs)
for the mechanism, and [river-obs.md](river-obs.md) for the streaming workflow.

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
| `Favorites` (XF86Favorites) | Lock screen (`river-lock` helper) |
| `Cancel` (break/pause) | Suspend |

### Mouse

| Mouse button | Action |
|---|---|
| `Super + left button` + drag | Move floating window |
| `Super + right button` + drag | Resize floating window |

Pointer devices are configured from `~/.config/river/init`. The Logitech
trackball uses button-scroll (hold `BTN_TASK` and roll). On **albemuth**,
**makondo**, and **atsoukka**, it expects the host OS to remap the small buttons
to `BTN_TASK` via a udev hwdb rule, and uses `BTN_TASK` for scrolling instead
to prevent accidental "Back" navigation in browsers. On NixOS (**albemuth** and
**makondo**), this is declared in `manual.nix`. On standalone Home Manager hosts
(like **atsoukka**), this udev rule must be installed manually:

```ini
# /etc/udev/hwdb.d/99-logitech-trackball.hwdb
evdev:name:Logitech USB Trackball:*
 KEYBOARD_KEY_90004=btn_task
 KEYBOARD_KEY_90005=btn_task
```
*(Run `sudo systemd-hwdb update && sudo udevadm trigger` to apply it.)*

The standalone `atsoukka` Home Manager profile installs `interception-tools`
and `evdev-debounce`, and starts a user-level `udevmon` service on graphical
session startup. Apply it with:

```console
$ make "switch atsoukka"
```

The service runs:

```text
intercept -g $DEVNODE | evdev-debounce 300 | uinput -d $DEVNODE
```

The standalone profile uses a 300 ms release debounce window. This covers the
observed release-to-repress gap on the trackball; genuine button releases can
therefore be delayed by up to 300 ms.

This remains userspace, but the host must grant the user read access to the
trackball's `/dev/input/event*` node and write access to `/dev/uinput` (usually
by adding the user to the `input` and `uinput` groups, or by a udev rule).
Check the service with `systemctl --user status evdev-debounce`.

Additionally, every other pointer and touch device is muted with `riverctl
input <dev> events disabled` — the touchpad, its trackpoint node, the ELAN
touchscreen, a phantom `PS/2 Generic Mouse` and the Ergodox's spurious pointer
endpoints all inject stray motion otherwise. List what the compositor sees with
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
- `programs.swaylock` — lock screen config on NixOS-backed profiles
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

### Screen locks surprisingly

On River hosts, the screen is locked after 10 minutes of inactivity via `swayidle`
(configured in `modules/home/services-river.nix`) using the `river-lock` helper.

For standalone Home Manager (`switch atsoukka`), `river-lock` expects a host
binary at `/usr/local/bin/swaylock` (or `RIVER_STANDALONE_SWAYLOCK` override).
Build/install it from `pkgs/swaylock-xjack`:

```sh
cd pkgs/swaylock-xjack
make deps-rhel
make install
make install-pam-service
make auth-check
```

Then validate in this order:

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
Then OBS can use the Portal/Wayland capture source. See
[Presentation output (recording with OBS)](#presentation-output-recording-with-obs)
for the recording setup itself.

Two limits of `xdg-desktop-portal-wlr` are worth knowing before designing
scenes around it:

- **There is no window capture.** wlr-screencopy copies whole outputs, so
  "Window Capture (PipeWire)" hands OBS the entire output regardless of what
  you pick in the chooser. Several window-capture sources in one scene are
  several identical full-screen streams, not separate windows.
- **You cannot capture a tag you are not looking at.** An unmapped tag is not
  composited, so there is nothing to copy. Recording one workspace while
  working in another needs a second output — which is what the presentation
  stage is for.

## Presentation output (recording with OBS)

A single-monitor machine cannot record "the other workspace": wlr-screencopy
copies a composited output, and an unmapped tag is never composited. The fix is
a second output that exists but is never scanned out.

`modules/nixos/services-river.nix` starts river with the headless wlroots
backend, giving a virtual output `HEADLESS-1` alongside the panel:

```
WLR_BACKENDS=libinput,drm,headless WLR_HEADLESS_OUTPUTS=1
```

kanshi pins it to exactly 1920x1080 at scale 1, placed right of the panel's
1920x1200 logical area (`machines/*/manual.nix`). The size is deliberate: an
OBS canvas of 1920x1080 then captures it **1:1**, with no downscaling and none
of the letterboxing a 16:10 panel forces on a 16:9 canvas.

Workflow:

1. Slides/demo go to the stage with `Super+S`, which follows focus there so the
   keyboard drives them. `set-cursor-warp on-output-change` brings the pointer
   along, so clicks land where the keyboard went.
2. `Super+Shift+S` returns focus to the panel. The stage keeps rendering, so
   the recording is unaffected.
3. `Super+Ctrl+S` mirrors the stage into a window (`wl-mirror`, another
   wlr-screencopy consumer) — the stage is never scanned out, so this is the
   only way to see it. Watch that beside OBS rather than OBS's own preview.
4. OBS stays on the panel, capturing `HEADLESS-1`, and adds the webcam on top
   as a V4L2 source. Composition splits cleanly: River arranges the content,
   OBS overlays only the camera.

The day-to-day workflow is written up for the presenter in
[river-obs.md](river-obs.md).

> **kanshi profiles must list `HEADLESS-1`.** kanshi applies a profile only
> when it matches the *whole* connected output set. A profile naming `eDP-1`
> alone silently stops matching once the headless backend is on, and a
> non-matching profile leaves every output at scale 1 — the "GTK/Qt apps look
> tiny" bug, arriving from an unexpected direction.

If OBS shows the stage at something other than 1920x1080, kanshi lost the
startup race against river registering the output; `Super+Ctrl+S` re-applies
the geometry.

The portal's restore token is bound to a specific output, so after enabling
this the first time, re-pick the source in OBS (Screen Capture → Properties →
Select Monitor → `HEADLESS-1`); it will reconnect to the panel otherwise.
