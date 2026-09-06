# Streaming and Recording with River + OBS

How to record or stream slides, screen share, and webcam from this laptop at 1920x1080 (16:9) for YouTube.

## The Concept

Wayland screen capture copies a whole **output** — never an individual window or a hidden workspace. To keep your notes and OBS controls private while presenting, River provides a virtual second output:

- **Panel** (`eDP-1`): Your physical laptop screen (3840x2400 @ 2x scaling). Holds OBS, notes, chat, and the stage mirror. **Never recorded.**
- **Stage** (`HEADLESS-1`): A headless 1920x1080 virtual output running at 60 Hz. Holds your slides and apps. **This is what OBS captures 1:1.**

OBS captures the Stage and overlays your Webcam on top.

---

## Happy-Path Recording Walkthrough

### 1. Launch OBS
Open a terminal or the app launcher (`Super+P`) and launch:
```sh
obs
```
> **Note:** Always launch standard `obs` for stage recordings. It runs on the Intel iGPU using QuickSync (QSV) / VA-API hardware encoding, allowing zero-copy capture of the headless stage. (See [Launchers](#launchers-obs-vs-obs-nvenc) below).

- Verify the preview shows the stage (black or wallpaper until apps are moved there).
- In the `Content + Webcam` scene, verify your webcam is live and smooth.

### 2. Send your presentation to the Stage
1. Open your slides, code editor, or browser on any tag on your panel.
2. Press **`Super+S`**.
   - The focused window immediately moves to the stage.
   - Your keyboard and mouse pointer follow it there.

### 3. Open the Stage Mirror
Press **`Super+Ctrl+S`**.
- A live mirror window (`wl-mirror`) opens on your panel showing the stage.
- Tile it next to OBS (`Super+H` / `Super+L` to adjust split).
- **This mirror is how you see and interact with your presentation full size** without relying on OBS's smaller preview.

### 4. Choose your Scene
In OBS, click the scene that matches your presentation segment:

| Scene | Composition | Best For |
|---|---|---|
| `Content Only` | Stage fullscreen (no webcam) | Dense slides, code demos, full diagrams |
| `Content + Webcam` | Stage fullscreen + small PiP (bottom-right) | Default presentation mode |
| `Content + Big Webcam` | Stage fullscreen + ~40% PiP (bottom-right) | Explaining concepts, Q&A, storytelling |
| `Webcam Full` | Webcam fullscreen | Intro, outro, direct address to audience |

> **Tip:** OBS scenes differ *only* in webcam layout. Content is controlled by River on the stage.

### 5. Record and Present
1. Click **Start Recording** in OBS.
2. Present your slides:
   - Type or click in the stage window (your cursor will be in the mirror).
   - Need to check notes or OBS? Press **`Super+Shift+S`** to return focus to the panel.
   - Ready to resume slides? Press **`Super+S`** to focus the stage again.
   - Need multiple apps on stage? Move another window there with `Super+S`. River's tiling (`Super+H`/`Super+L`) works on the stage just like on the panel!

### 6. Wrap Up
1. Click **Stop Recording** in OBS.
2. Press **`Super+Ctrl+S`** to close the mirror window.
3. Bring your presentation window back to the panel whenever you want using `Super+Shift+W` (or close it with `Super+Shift+C`).

Your video is saved in `~/` in crash-safe `hybrid_mp4` format (playable even if interrupted).

---

## Quick Reference: Presentation Stage Keys

| Key | Action | What Happens |
|---|---|---|
| `Super+S` | **Present Window** | Sends focused window to the stage, retags it to match the stage, and moves keyboard + pointer focus to the stage. |
| `Super+Shift+S` | **Return to Panel** | Moves keyboard and pointer focus back to the panel. Windows on the stage remain there and keep recording. |
| `Super+Ctrl+S` | **Toggle Mirror** | Opens/closes a live `wl-mirror` window on your panel showing the stage. |
| `Super+Shift+Ctrl+S` | **Reset Stage Mode** | Re-applies 1920x1080@60Hz custom geometry to `HEADLESS-1` if kanshi ever loses state. |
| `Super+W` | **Cycle Outputs** | Switches focus between panel (`eDP-1`) and stage (`HEADLESS-1`). |

---

## Rules of Thumb

1. **Anything on the stage is live.** There is no "staging" or preview buffer. The instant a window is moved to the stage, the audience sees it. Close private tabs and mute notifications beforehand.
2. **The panel is always private.** OBS controls, presenter notes, chat, email, and scratchpads on `eDP-1` are never recorded.
3. **Never send OBS to the stage.** Capturing OBS inside OBS creates an infinite mirror tunnel. If this happens by accident, press `Super+Shift+W` to return it to the panel.
4. **Watch the mirror, not the OBS preview.** `wl-mirror` (`Super+Ctrl+S`) runs 1:1 at full refresh rate with zero lag.
5. **Stage tags are independent.** Just like your panel has tags 1–9, the stage has its own tags. `Super+S` uses `-current-tags` to automatically match whatever tag the stage is currently showing.

---

## Launchers: `obs` vs `obs-nvenc`

- **`obs`** *(Default / Recommended)*:
  - Runs OBS on the **Intel iGPU** using **Intel QuickSync Video (QSV)** or VA-API hardware encoding (`intel-media-driver` + `vpl-gpu-rt`).
  - Shares the GPU with River's compositor, enabling zero-copy DMA-BUF imports.
  - **Required for presentation stage (`HEADLESS-1`) capture.**
- **`obs-nvenc`** *(Alternative Launcher)*:
  - Launches OBS offloaded to the **NVIDIA dGPU** with hardware **NVENC** enabled.
  - Suffixes `LD_LIBRARY_PATH` with `/run/opengl-driver/lib` so the `obs-nvenc-test` helper probes CUDA successfully.
  - Use this for heavy local encoding where screen capture is taken from physical displays (`eDP-1`) or camera-only streams where DMA-BUF import errors do not apply.

---

## Output Settings

Configured out of the box in the `Nimetön` profile:

| Setting | Value | Notes |
|---|---|---|
| **Canvas & Output** | 1920x1080, 30 fps | Zero canvas scaling, 1:1 match with stage |
| **Encoder** | Hardware (QSV, H.264) | Uses Intel iGPU (`obs_qsv11_v2` / `qsv`) |
| **Recording Format** | `hybrid_mp4` | Crash-safe MP4; no remuxing needed |
| **Video Bitrate** | 6000 kbps | YouTube 1080p recommended bitrate |
| **Audio** | 48 kHz stereo, 160 kbps | AAC |

### Publishing to YouTube
To stream to YouTube:
1. Open **Settings → Stream**.
2. Service: **YouTube - RTMPS** (Server: *Primary YouTube ingest server*).
3. Paste your YouTube stream key and click **Apply**.
4. Run a 30-second test stream/recording and verify audio levels and framing.

### OBS Virtual Camera
Click **Start Virtual Camera** in OBS to publish the current scene to `/dev/video9`.
Applications like Chromium, Firefox, Google Meet, or Zoom will detect "OBS Virtual Camera" as a webcam device, allowing you to share your combined presentation stage and camera in web meetings.

---

## When Something Looks Wrong

### Stage in OBS is an empty desktop after `Super+S`
River tags are per-output. `Super+S` passes `-current-tags` to ensure windows land on the stage's visible tag. If a window appears missing:
- Press `Super+S` to focus the stage, then press `Super+1` (or `Super+2`..`9`) to switch to the tag containing the window.
- Or run `riverctl send-to-output -current-tags next` in a terminal.

### Black stage preview or `glEGLImageTargetTexture2DOES failed` loop
- Make sure you launched standard **`obs`**, not `obs-nvenc`. Capturing `HEADLESS-1` requires OBS on the Intel iGPU.
- If OBS was already open before River configured the stage, select the source:
  *Sources → `Screen Capture` → Properties → Select Monitor → `HEADLESS-1`*.

### Stage is not 1920x1080 or panel text became tiny
- Press **`Super+Shift+Ctrl+S`** to force re-apply the 1920x1080 custom mode to `HEADLESS-1`.
- If eDP-1 dropped to scale 1.0 (tiny fonts), kanshi profile failed to match. Verify with:
  ```sh
  wlr-randr | grep -E "HEADLESS-1|eDP-1|Scale|px"
  ```
  Both outputs must be listed in kanshi (`HEADLESS-1` requires `--custom 1920x1080@60Hz`).

### Webcam is black or log shows `select timed out`
- Dell FHD webcams expose 4 nodes under the same card name:
  - `/dev/video0`: RGB camera (**use this one**)
  - `/dev/video1`, `/dev/video3`: UVC metadata
  - `/dev/video2`: Infrared / Windows Hello camera (GREY-only, never delivers frames)
- Double-click **`Webcam`** in OBS sources:
  - Device: Ensure it resolves to `/dev/video0`.
  - Video Format: **MJPEG** (provides real 30 fps; raw YUYV is capped at 5 fps).
  - Resolution: `1920x1080`, Framerate: `30`.
  - **Autoreset on Timeout**: Keep **unchecked** (a short timeout livelocks UVC camera initialization).

### Keyboard/mouse input goes nowhere
- Your focus is on the other output.
- Press **`Super+Shift+S`** to bring focus back to the panel, or **`Super+W`** to cycle outputs.
