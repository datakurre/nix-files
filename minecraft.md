# Minecraft Bedrock Dedicated Server

This repository includes packaging and a multi-instance NixOS service for Mojang's *Minecraft: Bedrock Edition* dedicated server:

- **Package definition:** [`modules/nixos/pkgs-minecraft-bedrock-server.nix`](modules/nixos/pkgs-minecraft-bedrock-server.nix)
- **Service module:** [`modules/nixos/services-minecraft-bedrock.nix`](modules/nixos/services-minecraft-bedrock.nix)
- **Host configuration:** [`machines/albemuth-x1g9/manual.nix`](machines/albemuth-x1g9/manual.nix) (or machine `manual.nix`)

---

## How Upgrades Work (and Why Bumping `version` is Not Enough)

Mojang's Minecraft EULA prohibits automated redistribution of the server binary, and Mojang's download endpoint blocks non-browser automated fetchers. Therefore:

1. The package uses Nix's `requireFile` to reference `bedrock-server.zip`.
2. `requireFile` requires the zip file to be **manually added to the Nix store** with the fixed name `bedrock-server.zip` and matching the exact cryptographic `hash` declared in the Nix expression.
3. If you only bump `version` in `pkgs-minecraft-bedrock-server.nix` without updating the `hash` and adding the new zip to the store, Nix will find the **old cached zip** matching the old `hash` in `/nix/store` and silently rebuild the derivation using the previous version's binary and data.

---

## Step-by-Step Server Upgrade Procedure

### 1. Download the new release zip

Visit the official Mojang Bedrock server download page in a browser:
- [https://www.minecraft.net/en-us/download/server/bedrock](https://www.minecraft.net/en-us/download/server/bedrock)

Download the Linux server zip (e.g. `bedrock-server-1.26.45.1.zip`).

### 2. Add the zip to the Nix store & get the new hash

Add the downloaded file to the Nix store with the expected fixed name `bedrock-server.zip`:

```bash
nix-prefetch-url --type sha256 --name bedrock-server.zip file:///path/to/downloaded/bedrock-server-1.26.45.1.zip
```

To get the SRI formatted SHA-256 hash required by Nix (`sha256-...`), compute:

```bash
nix hash file --sri /path/to/downloaded/bedrock-server-1.26.45.1.zip
```

*(Alternatively, if you rename or symlink the download to `bedrock-server.zip`, you can also use `nix-store --add-fixed sha256 bedrock-server.zip`)*.

### 3. Update the package derivation

Edit [`modules/nixos/pkgs-minecraft-bedrock-server.nix`](modules/nixos/pkgs-minecraft-bedrock-server.nix):

```nix
  version ? "1.26.45.1", # Set to exact upstream release version
  hash ? "sha256-...",     # Set to new SRI hash from step 2
```

### 4. Verify the package build

Build the package locally without switching the system:

```bash
nix build .#nixosConfigurations.albemuth.config.services.minecraft-bedrock.package
# or build for a specific instance:
nix build .#nixosConfigurations.albemuth.config.services.minecraft-bedrock.servers.\"crazy-land\".package
```

Verify that the unpacked files match the new version:

```bash
ls ./result/lib/minecraft-bedrock-server/behavior_packs/
```

### 5. Deploy and restart the server

Apply the updated NixOS configuration:

```bash
make "switch albemuth"
```

The systemd service `minecraft-bedrock-<server-name>.service` will restart automatically and update all store symlinks (`definitions`, `config`, `data`, `behavior_packs`, `resource_packs`) pointing to the new release.

Check the server logs to verify that the new version started properly:

```bash
journalctl -u minecraft-bedrock-crazy-land -e
```

---

## Server Management & Administration

### Interacting with the Server Console

A shell wrapper `minecraft-bedrock-console` is provided in system packages to write directly to the server's stdin FIFO:

```bash
# Send a command to the "crazy-land" instance:
minecraft-bedrock-console crazy-land "say Server restarting in 5 minutes"
minecraft-bedrock-console crazy-land "list"
minecraft-bedrock-console crazy-land "op SomeGamertag"
```

### Service Status and Logs

```bash
# Check status
systemctl status minecraft-bedrock-crazy-land

# Follow logs in real time
journalctl -u minecraft-bedrock-crazy-land -f
```

### Data and State Directory

- Instance state directories are located in `/var/lib/minecraft-bedrock/<server-name>/`.
- Symlinked immutable data (`definitions`, `config`, `data`, vanilla packs) points directly into the `/nix/store`.
- Mutable world saves and LevelDB data reside under `/var/lib/minecraft-bedrock/<server-name>/worlds/<level-name>/`.

### World Migration and Backups

- **Creating a fresh world:** Starting an instance without an existing `worlds/<level-name>` directory automatically generates a new world using `serverProperties.level-name` and `serverProperties.level-seed`.
- **Copying/Restoring an existing world:**
  1. Stop the instance: `sudo systemctl stop minecraft-bedrock-crazy-land`
  2. Copy the world folder (containing `level.dat` and `db/`) into `/var/lib/minecraft-bedrock/<server-name>/worlds/<level-name>/`.
  3. Ensure `serverProperties.level-name` matches the directory name under `worlds/`.
  4. Start the instance: `sudo systemctl start minecraft-bedrock-crazy-land`.
  *(With `fixStateOwnership = true` (default), systemd automatically fixes file ownership for the server's dynamic user on startup).*
