# Streaming and recording with River + OBS

How to record or stream slides, a screen share and a webcam from this laptop,
at 1920x1080 for YouTube.

The setup exists because of one constraint: on Wayland, OBS captures a whole
**output** — never a window, and never a workspace you are not looking at. So
we give River a second output that has no physical screen behind it, put the
presentation there, and keep the panel for yourself.

- **Panel** (`eDP-1`) — what you see. OBS, notes, chat. Never recorded.
- **Stage** (`HEADLESS-1`) — 1920x1080, invisible, recorded. Slides live here.

The audience sees only the stage. OBS adds the webcam on top.

For keybindings and River itself, see [river.md](river.md). For why it is built
this way, see the same file's "Presentation output" section.

---

## Before you start

1. **Check the stage exists.**
   ```sh
   wlr-randr | grep -A2 HEADLESS-1
   ```
   Expect `1920x1080`. Nothing at all means River started without the headless
   backend — log out and back in. Wrong size means kanshi lost a startup race:
   press `Super+Ctrl+Shift+S`.

2. **Open OBS on the panel** and confirm the preview shows the stage, not your
   desktop. If it shows your desktop, the capture is on the wrong output:
   Sources → `Screen Capture` → Properties → Select Monitor → `HEADLESS-1`.
   This is expected the first time — the portal remembers a specific output.

3. **Check the webcam is live** in the `Content + Webcam` scene. It should be
   smooth, not a slideshow and not black. The `Webcam` source is a **Video
   Capture Device (V4L2)** on `/dev/video0`, MJPEG, 1920x1080, 30 — do not
   replace it with the PipeWire camera source. See the webcam entries under
   "When something looks wrong" if it is dead.

4. **Mirror the stage** with `Super+Ctrl+S`. A window opens showing the stage
   live. Put it next to OBS. **This is how you see what you are presenting** —
   do not rely on OBS's small preview.

---

## The three keys

| Key | What it does |
|---|---|
| `Super+S` | Send this window to the stage and go with it |
| `Super+Shift+S` | Come back to the panel, leave the window on the stage |
| `Super+Ctrl+S` | Show/hide the stage mirror window |

`Super+S` moves your keyboard **and** pointer to the stage, so you drive the
slides normally while watching the mirror window. `Super+Shift+S` brings you
back to the panel; whatever is on the stage keeps rendering and keeps being
recorded.

That is the whole workflow. Everything else is ordinary River.

---

## Running a session

**Set up**

1. Open your slides on any tag.
2. `Super+S` — the slides move to the stage and your focus follows.
3. `Super+Shift+S` — back to the panel.
4. `Super+Ctrl+S` — mirror the stage so you can see it.
5. Arrange OBS and the mirror side by side (`Super+H` / `Super+L` to adjust the
   split).

**Present**

- Advance slides: `Super+S` to move focus to the stage, then drive as usual.
- Check notes or chat: `Super+Shift+S`, read, `Super+S` to go back.
- Add a terminal or browser to the stage: focus it, `Super+S`. The stage is a
  normal River output, so tags and `rivertile` work there — `Super+H`/`Super+L`
  to split slides and terminal side by side on the stage itself.

**Switch look** — in OBS, click a scene:

| Scene | Use it for |
|---|---|
| `Content Only` | Dense slides, code, anything needing the full frame |
| `Content + Webcam` | Default. Small camera, bottom right |
| `Content + Big Webcam` | Explaining, storytelling, Q&A |
| `Webcam Full` | Intro, outro, talking directly to the audience |

The scenes differ **only** in the webcam. Content is whatever is on the stage,
so you change content by moving windows in River, not by switching OBS scenes.

**Finish** — Stop Recording in OBS, then `Super+Ctrl+S` to close the mirror.

---

## Rules of thumb

**Anything on the stage is being recorded.** There is no "preview" state. Move
a window there and it is live. Sort out notifications, secrets and private tabs
*before* sending a window across.

**Nothing on the panel is ever recorded.** Notes, chat, email, the OBS window
itself — all invisible to the audience. This is the point of the setup.

**Never put OBS on the stage.** Mirroring a capture of itself gives the
infinite-tunnel effect. If it happens, `Super+Shift+W` moves it back.

**Watch the mirror, not the preview.** The mirror is full size and current.
OBS's preview is small and lags slightly.

**Stage tags are separate from panel tags.** `Super+1..9` switches tags on the
output you are focused on. On the stage, that switches what the audience sees.

---

## When something looks wrong

**You sent a window to the stage and OBS shows an empty desktop** — the most
confusing failure, and nothing is actually broken. Tags are **per-output**: the
stage has its own focused tags, so a window sent from tag 3 arrives still
tagged 3 on an output displaying tag 1. It is there, just not on a visible tag.
`river-present` passes `-current-tags` to retag on arrival, so this should not
happen; if it does, either press `Super+1..9` on the stage until the window
appears, or run `riverctl send-to-output -current-tags next` by hand.

**Black or frozen stage in OBS** — distinct from empty. The capture died,
usually after a suspend/resume. Sources → `Screen Capture` → Properties →
Select Monitor → `HEADLESS-1`.

**Stage is not 1920x1080** — `Super+Shift+Ctrl+S` re-applies the geometry.
Everything else in the frame will look soft or letterboxed until it is right.

**Webcam is black and the log repeats `select timed out` / `stream reset`** —
OBS is on the wrong node or resetting faster than the camera can start. The
camera exposes four nodes that all report the identical name
`Integrated_Webcam_FHD`, so the OBS dropdown cannot distinguish them:
`/dev/video0` is the RGB camera (MJPG + YUYV), `/dev/video2` is the infrared
sensor (GREY only — it opens fine and never sends a frame), and `video1`/
`video3` are metadata. Set the device back to `/dev/video0`. Confirm the camera
itself is healthy, with OBS closed:

```sh
v4l2-ctl -d /dev/video0 --stream-mmap --stream-count=10   # ten "<" = fine
for d in /dev/video*; do echo "== $d"; v4l2-ctl -d "$d" --list-formats | grep "\[[0-9]\]"; done
```

The node listing `MJPG` is the one you want. Also leave **Autoreset on Timeout
off**: at a few frame periods it fires before a UVC camera can produce its
first MJPG frame, so the stream resets forever and never starts.

**Webcam is choppy or 5 fps** — Properties → Video Format → **MJPEG**. At
1920x1080 the raw YUYV mode only offers 5 fps; MJPEG gives a real 30.

**Typing goes nowhere** — your keyboard is on the other output. `Super+W`
cycles. `Super+Shift+S` always lands you on the panel.

**Desktop text suddenly tiny, and the stage is 1280x720** — these two always
happen together, and mean kanshi applied *nothing*. It applies a profile all or
nothing, so one bad output entry takes the panel's scaling down with it. Two
causes: the profile does not list every connected output, or it asks for a mode
the output does not advertise (the stage only ever advertises 1280x720, so its
mode must be written `--custom`). Both live in `machines/<host>/manual.nix`.
Confirm with `journalctl --user -u kanshi -b | tail`.

**`glEGLImageTargetTexture2DOES failed` in the log — ignore it.** It appears
once at startup, immediately followed by `Renegotiating stream`, and the
capture then works. OBS runs its GL context on the NVIDIA GPU (for NVENC) while
River composites on the Intel iGPU, so the first DMA-BUF format offered cannot
be imported; OBS renegotiates and the second one succeeds. It is noise, not a
fault, and recording with NVENC works alongside it. Only worry if the stream
never reaches `streaming` afterwards.

---

## Output settings

Already configured, listed so you can recognise a wrong value:

| | |
|---|---|
| Canvas / output | 1920x1080, 30 fps |
| Encoder | NVENC (hardware, on the dGPU) |
| Recording | `hybrid_mp4` into `/home/atsoukka` |
| Video bitrate | 6000 kbps |
| Audio | 48 kHz stereo, 160 kbps |

`hybrid_mp4` is crash-safe — a recording that is interrupted stays playable, so
there is no need to record MKV and remux afterwards.

## Publishing to YouTube

Paste your key into Settings → Stream (Service: **YouTube - RTMPS**); it is
deliberately left blank in the repo. Streaming uses the same 1920x1080/30 and
NVENC settings as recording.

Do a 30-second test recording first and **watch it back**. It is the only way
to catch a silent microphone, an empty stage, or a webcam that died — all three
look fine in the OBS preview.

## The virtual camera

`Start Virtual Camera` publishes the current scene to `/dev/video9` as "OBS
Virtual Camera", so Chromium or Firefox can use the full composition — stage
plus webcam overlay — as a webcam in a video call. It needs no setup; the
`v4l2loopback` module is configured in
[modules/nixos/programs-obs.nix](modules/nixos/programs-obs.nix).
