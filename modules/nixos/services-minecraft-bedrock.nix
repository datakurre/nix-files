# Multi-instance NixOS module for the Minecraft: Bedrock Edition dedicated
# server.
#
# Because Mojang does not allow automated redistribution of the server binary,
# the `bedrock-server.zip` archive must be manually downloaded and added to the
# Nix store before building:
#
#   1. Download the Bedrock Dedicated Server zip from:
#      https://www.minecraft.net/en-us/download/server/bedrock
#   2. Add it to the Nix store:
#      nix-store --add-fixed sha256 bedrock-server.zip
#      # or if downloaded with a versioned filename:
#      nix-prefetch-url --type sha256 --name bedrock-server.zip file:///path/to/bedrock-server-1.26.44.3.zip
#
# Usage (see the `example` blocks on the options below for more):
#
#   imports = [ ./services-minecraft-bedrock.nix ];
#
#   services.minecraft-bedrock.servers = {
#     # A LAN-only survival world, discoverable from the "Friends" tab.
#     koti = {
#       openFirewall = true;
#       serverProperties = {
#         online-mode = false;            # no Microsoft account required
#         gamemode = "creative";
#         enable-lan-visibility = true;   # only defaulted on for a lone server
#       };
#     };
#     # A second, internet-facing world tied to Microsoft accounts.
#     julkinen = {
#       openFirewall = true;
#       serverProperties = {
#         server-port = 19134;            # 19132/19133 are taken by `koti`
#         server-portv6 = 19135;
#       };
#       allowList.SomeGamertag = { };
#       permissions."2535000000000000".permission = "operator";
#     };
#   };
#
# Module conventions used here (NixOS module best practices):
#
#   * Instances live under `services.minecraft-bedrock.servers.<name>` as an
#     `attrsOf (submodule ...)`.  This is the idiomatic way to make a service
#     multi-instance: every instance gets its own systemd unit, state
#     directory and user, and options can be set per instance while the
#     top-level attrset holds the shared defaults.
#   * The whole `config` body is guarded by `mkIf`, so importing this file has
#     no effect until at least one server is enabled.
#   * `serverProperties` is a settings submodule with a `freeformType`: every
#     property of the packaged release is declared as a typed, documented
#     sub-option carrying upstream's default, while the free-form type still
#     accepts anything not on that list -- so a property added by a newer
#     Bedrock release works without touching this file.  `pkgs.formats.keyValue`
#     both provides that type and renders the file.
#   * Values the module itself reads back (ports, transport, allow-list, ...)
#     are ordinary sub-options with defaults, so the module can compute
#     firewall rules and assertions from the *effective* configuration.
#   * Everything the server needs at runtime is either a symlink into the Nix
#     store (immutable game data) or a file installed on every start
#     (declarative config), so a fresh, empty state directory is a valid
#     starting point: the server generates its world on first launch.
#
# Copying a world into the state directory by hand works with the service stopped.
# `<dataDir>` is `/var/lib/minecraft-bedrock/<name>`, but note that with
# `dynamicUser` (the default) systemd keeps the real directory in
# `/var/lib/private/...`, which is only reachable as root.  Ownership does not
# need fixing afterwards: `fixStateOwnership` re-owns the directory as root on
# every start.  `behavior_packs` and `resource_packs` are real directories
# holding one symlink per packaged pack, so copied-in packs coexist with them.
#
# The remaining directories -- `definitions`, `config`, `data` -- are symlinks
# into the Nix store.  A leftover *copy* of one of them (from a single-instance
# setup that unpacked the whole server tree into /var/lib/minecraft-bedrock) is
# refused with a message naming the path; delete it and start again.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    attrNames
    attrValues
    concatStringsSep
    escapeShellArg
    filterAttrs
    hasPrefix
    length
    literalExpression
    mapAttrs'
    mapAttrsToList
    mkDefault
    mkIf
    mkOption
    nameValuePair
    optional
    optionalString
    optionals
    removePrefix
    types
    ;

  cfg = config.services.minecraft-bedrock;

  # `server.properties` is a flat `key=value` file, which is exactly what
  # `pkgs.formats.keyValue` generates -- including bool/int/float rendering.
  # `listsAsDuplicateKeys` is deliberately *not* enabled: it would wrap every
  # value in a list, and the one property that accepts several values
  # (`server-udp-ports`) also takes them comma-separated on a single line.
  propertiesFormat = pkgs.formats.keyValue { };
  jsonFormat = pkgs.formats.json { };

  # Upstream ships its read-only game data below this prefix; see
  # ./pkgs-minecraft-bedrock-server.nix.
  serverRoot = server: "${server.package}/lib/minecraft-bedrock-server";

  # Behaviour/resource packs: when the instance adds none we can point the
  # state directory straight at the package, which avoids an extra
  # derivation.  Otherwise vanilla packs and user packs are merged into one
  # symlink tree; a user pack with the same name shadows the vanilla one.
  mkPackDir =
    server: kind: extra:
    if extra == { } then
      "${serverRoot server}/${kind}"
    else
      pkgs.runCommand "minecraft-bedrock-${kind}" { } ''
        mkdir -p $out
        shopt -s nullglob
        for pack in ${serverRoot server}/${kind}/*; do
          ln -s "$pack" "$out/$(basename "$pack")"
        done
        ${concatStringsSep "\n" (
          mapAttrsToList (
            packName: path: "ln -sfn ${escapeShellArg "${path}"} $out/${escapeShellArg packName}"
          ) extra
        )}
      '';

  # allowlist.json is a JSON array of objects; the attribute name is the
  # Xbox gamertag unless `name` is set explicitly.
  allowListFile =
    server:
    jsonFormat.generate "allowlist.json" (
      mapAttrsToList (
        _: entry:
        {
          inherit (entry) name ignoresPlayerLimit;
        }
        // lib.optionalAttrs (entry.xuid != null) { inherit (entry) xuid; }
      ) server.allowList
    );

  # permissions.json maps XUIDs (not gamertags -- the server resolves these
  # only for Xbox-authenticated players) to a permission level.
  permissionsFile =
    server:
    jsonFormat.generate "permissions.json" (
      mapAttrsToList (_: entry: { inherit (entry) permission xuid; }) server.permissions
    );

  # `null` means "upstream ships this commented out and it was left alone",
  # so the key must not appear in the file at all.
  propertiesFile =
    server:
    propertiesFormat.generate "server.properties" (
      filterAttrs (_: value: value != null) server.serverProperties
    );

  # Instances that are actually turned on.  Note this only forces `.enable`
  # of each submodule, never the rest of its config, so it is safe to use
  # while computing other instances' defaults.
  enabledServers = filterAttrs (_: server: server.enable) cfg.servers;

  unitName = name: "minecraft-bedrock-${name}";
  fifoFor = name: "/run/minecraft-bedrock/${name}.stdin";

  # Shorthand for the properties the module itself reads back.  Every one of
  # them is a declared sub-option, so it always has a value.
  prop = server: key: server.serverProperties.${key};

  ###########################################################################
  # server.properties schema
  ###########################################################################
  # Every property of the packaged release is declared below, which is what
  # makes them type-checked (enums, port and range types) and documented in
  # `man configuration.nix`.  The declarations sit behind the `freeformType`
  # of the option, so unknown keys keep working: a property introduced by a
  # future Bedrock release can be set immediately, without waiting for this
  # list to catch up.
  #
  # The list and the defaults are taken from the packaged version's own
  # `server.properties` and `bedrock_server_how_to.html`; re-check them when
  # bumping the server.  Properties that upstream ships commented out are
  # declared with `unset` and are then left out of the generated file
  # entirely, so the server keeps applying its own built-in default.
  serverPropertyOptions =
    { name, server }:
    let
      # `opt <type> <default> <description>` -- always written to the file.
      opt =
        type: default: description:
        mkOption { inherit type default description; };
      # `unset <type> <description>` -- omitted from the file until set.
      unset =
        type: description:
        mkOption {
          type = types.nullOr type;
          default = null;
          inherit description;
        };
      inherit (types)
        bool
        either
        enum
        str
        port
        ;
      inherit (types.ints) unsigned;
      number = types.numbers;
    in
    {
      ## Identity and world ------------------------------------------------

      server-name = opt str name ''
        Name of the server as shown in the client's server list.  Must not
        contain a semicolon.  Defaults to the instance name.
      '';

      level-name = opt str name ''
        Name of the world, which is also its directory name under `worlds/`.
        Only used when the world is created -- changing it later makes the
        server generate a *new* world instead of renaming the old one.
        Defaults to the instance name.
      '';

      level-seed = opt str "" ''
        Seed for world generation; empty picks a random one.  Only read when
        the world is created.
      '';

      gamemode =
        opt
          (enum [
            "survival"
            "creative"
            "adventure"
          ])
          "survival"
          ''
            Game mode for new players.
          '';

      force-gamemode = opt bool false ''
        When `false`, the game mode saved with the world at creation time
        wins and a later change to {option}`gamemode` is not sent to clients.
        Set to `true` to make the current {option}`gamemode` take effect on
        an existing world.
      '';

      difficulty =
        opt
          (enum [
            "peaceful"
            "easy"
            "normal"
            "hard"
          ])
          "easy"
          ''
            World difficulty.  Can also be changed at runtime with the
            `changesetting difficulty <value>` console command, which does not
            write back to this file.
          '';

      allow-cheats = opt bool false ''
        Allow cheats, i.e. commands such as `/gamemode` or `/tp`.  Also
        changeable at runtime with `changesetting allow-cheats <value>`.
      '';

      max-players = opt unsigned 10 ''
        Maximum number of players allowed to connect.  Higher values cost
        performance.  Players allowlisted with `ignoresPlayerLimit` may join
        past this limit.
      '';

      view-distance = opt types.ints.positive 32 ''
        Maximum view distance in chunks; must be 5 or greater.  The single
        biggest performance knob.
      '';

      tick-distance = opt (types.ints.between 4 12) 4 ''
        The world is ticked this many chunks away from any player.
      '';

      player-idle-timeout = opt unsigned 30 ''
        Kick players after this many minutes of inactivity; `0` lets them
        idle forever.
      '';

      max-threads = opt unsigned 8 ''
        Maximum number of threads the server tries to use.  `0` means as many
        as the machine has.
      '';

      texturepack-required = opt bool false ''
        Force clients to use the texture packs of the current world.
      '';

      ## Networking and transport -------------------------------------------

      server-port = opt port 19132 ''
        IPv4 port to listen on.  With `transport = "raknet"` this is a UDP
        port; with `transport = "nethernet"` it is the TCP port that carries
        the signalling handshake.
      '';

      server-portv6 = opt port 19133 ''
        IPv6 port to listen on.  Ignored with `transport = "nethernet"`,
        which opens a single dual-stack socket on {option}`server-port`
        instead.
      '';

      transport =
        opt
          (enum [
            "raknet"
            "nethernet"
          ])
          "raknet"
          ''
            Network transport for client connections.

            `raknet` is the classic Bedrock UDP transport: clients connect
            straight to {option}`server-port`/{option}`server-portv6`.

            `nethernet` is WebRTC-based: the server speaks HTTP over TCP on
            {option}`server-port` for signalling and then negotiates a UDP
            connection per client, which is what {option}`udpPortRange` pins
            down.  Only the signalling port needs to be reachable directly, so it
            can be put behind a reverse proxy.
          '';

      server-ip = unset str ''
        Local address to bind the listening socket to; empty (the default)
        binds all interfaces.  Ignored with `transport = "raknet"`.
      '';

      server-udp-ports = unset str ''
        UDP port configuration for the NetherNet transport, either
        `internal` / `start-end` to constrain the local allocation window, or
        `[ip:]external[-external]:internal[-internal]` to also publish a
        NAT/port-forwarding mapping to clients.  Several entries may be given
        comma-separated.  Ignored with `transport = "raknet"`.

        Prefer setting {option}`udpPortRange`, which fills this in *and*
        opens the matching firewall range.
      '';

      enable-lan-visibility = opt bool (length (attrNames cfg.servers) == 1) ''
        Answer LAN discovery probes, so the server shows up automatically in
        the client's "Friends" tab on the same network.
      '';

      online-mode = opt bool true ''
        Require every connecting player to be authenticated with Xbox Live,
        i.e. signed in to a Microsoft account.  Clients connecting from
        outside the LAN always authenticate regardless of this setting.

        Turning it off is only reasonable on a trusted network: anyone who
        can reach the port may then join under any name, and XUID-based
        {option}`permissions` and the allowlist stop working.
      '';

      allow-list = opt bool (server.allowList != null) ''
        Only let players listed in `allowlist.json` connect.  Defaults to
        `true` when {option}`allowList` is set.

        Requires {option}`online-mode`; the server refuses to start
        otherwise, since an unauthenticated player could claim any gamertag.
      '';

      allow-player-joining = unset bool ''
        When `false`, players cannot join unless a behaviour pack script
        explicitly allows them (via `AsyncPlayerJoinBeforeEvent`).
      '';

      enable-packet-rate-limiter = opt bool false ''
        Apply the packet rate limits from `packetlimitconfig.json`, which
        {option}`packetLimitConfig` can generate.
      '';

      compression-threshold = opt (types.ints.between 0 65535) 1 ''
        Smallest raw network payload that gets compressed, as a
        CPU-versus-bandwidth trade-off.
      '';

      compression-algorithm =
        opt
          (enum [
            "zlib"
            "snappy"
          ])
          "zlib"
          ''
            Compression algorithm used on the wire.
          '';

      ## Players and permissions --------------------------------------------

      default-player-permission-level =
        opt
          (enum [
            "visitor"
            "member"
            "operator"
          ])
          "member"
          ''
            Permission level a player gets when joining for the first time, and
            the level applied to anyone not listed in {option}`permissions`.

            `visitor` can look but not build, `member` is a normal player, and
            `operator` may use commands and the in-game admin UI.  On a LAN
            server without Xbox Live authentication this is the *only* way to
            hand out operator rights, because `permissions.json` is keyed by
            XUID.
          '';

      chat-restriction =
        opt
          (enum [
            "None"
            "Dropped"
            "Disabled"
          ])
          "None"
          ''
            `None` is regular free chat, `Dropped` silently drops chat messages
            and tells the player chat is off, and `Disabled` hides the chat UI
            entirely for everyone but operators.
          '';

      disable-player-interaction = opt bool false ''
        Tell clients to ignore other players when interacting with the world.
        Advisory only -- it is not enforced server-side.
      '';

      disable-persona = opt bool false ''
        Internal use only, per upstream documentation.
      '';

      disable-custom-skins = opt bool false ''
        Reject player skins that were not made from Minecraft store or
        in-game assets, which is the usual lever against offensive skins.
      '';

      ## Movement and server authority ---------------------------------------

      server-authoritative-movement-strict = opt bool false ''
        Be stricter about player positions reported by the client, at the
        cost of more position corrections for players on high-latency links
        or on moving blocks.
      '';

      server-authoritative-dismount-strict = opt bool false ''
        Be stricter about the position a player dismounts at.
      '';

      server-authoritative-entity-interactions-strict = opt bool false ''
        Be stricter about entity interactions, which mostly shows up as
        corrections when players interact with each other over high latency.
      '';

      player-position-acceptance-threshold = opt number.nonnegative 0.5 ''
        How far the client's idea of a player's position may drift from the
        server's before a correction is sent.  Higher values mean fewer
        corrections for honest players; values above `1.0` measurably
        increase the chance of missing a cheater.
      '';

      player-movement-action-direction-threshold = opt (number.between (-1) 1) 0.85 ''
        How far a player's attack direction may differ from their view
        direction, as the cosine of the angle between them: `1` requires them
        to match exactly, `0` accepts anything in front of the player, and
        `-1` accepts any direction at all.
      '';

      server-authoritative-block-breaking = opt bool false ''
        Compute block mining server-side and verify that the client was
        allowed to break the block.  Silently disabled when client
        authoritative movement is in use.
      '';

      server-authoritative-block-breaking-pick-range-scalar = opt number.positive 1.5 ''
        Scales the allowed block-breaking range; squared and multiplied with
        the default range.  Only used when
        {option}`server-authoritative-block-breaking` is on.
      '';

      ## Client-side rendering ------------------------------------------------

      client-side-chunk-generation-enabled = opt bool true ''
        Let clients generate visual chunks beyond the distance the server
        simulates, which makes the view distance look larger for free.
      '';

      server-build-radius-ratio =
        opt (either (enum [ "Disabled" ]) (number.between 0.0 1.0)) "Disabled"
          ''
            How much of the player's view the server generates itself, leaving
            the rest to the client.  `"Disabled"` decides dynamically based on
            the client's hardware; a ratio overrides that.  Only used when
            {option}`client-side-chunk-generation-enabled` is on.
          '';

      block-network-ids-are-hashes = opt bool true ''
        Send hashed block network IDs, which stay stable across content
        changes, instead of sequential ones.
      '';

      disable-client-vibrant-visuals = unset bool ''
        Tell clients to fall back to the next best graphics setting instead
        of using Vibrant Visuals.
      '';

      ## Logging and telemetry -------------------------------------------------

      content-log-file-enabled = opt bool false ''
        Log content errors (bad behaviour packs, broken scripts) to a file in
        the state directory.
      '';

      content-log-console-output-enabled = opt bool false ''
        Log content errors to stdout, which on NixOS means the journal:
        `journalctl -u minecraft-bedrock-<name>`.
      '';

      content-log-level =
        opt
          (enum [
            "error"
            "warning"
            "info"
            "verbose"
          ])
          "info"
          ''
            Lowest content log level that is still reported; `error` is the
            highest severity.
          '';

      emit-server-telemetry = opt bool false ''
        Send gameplay telemetry to Mojang.  Off by default, and the server
        prints a banner about it on every start.
      '';

      ## Behaviour pack scripting -----------------------------------------------

      allow-outbound-script-debugging = opt bool false ''
        Allow the script debugger's `connect` command, and
        `script-debugger-auto-attach = "connect"`.
      '';

      allow-inbound-script-debugging = opt bool false ''
        Allow the script debugger's `listen` command, and
        `script-debugger-auto-attach = "listen"`.  This opens a debug port,
        so keep it off on anything reachable from the internet.
      '';

      script-debugger-auto-attach =
        opt
          (enum [
            "disabled"
            "connect"
            "listen"
          ])
          "disabled"
          ''
            Attach the script debugger when the level loads: `connect` dials out
            to a listening debugger, `listen` waits for one to connect.  Requires
            the matching `allow-*-script-debugging` option.
          '';

      force-inbound-debug-port = unset port ''
        Pin the inbound (listen) debugger port instead of using the default
        19144.  Required for `script-debugger-auto-attach = "listen"`.
      '';

      script-debugger-auto-attach-connect-address = unset str ''
        `host:port` to dial for `script-debugger-auto-attach = "connect"`.
      '';

      script-debugger-auto-attach-timeout = unset unsigned ''
        How long to wait at level load for a debugger to attach.
      '';

      script-debugger-passcode = unset str ''
        Passcode the debugger prompts for before connecting.

        Note that this ends up in the world-readable Nix store, like every
        other value in this file; treat it as a speed bump, not a secret.
      '';

      ## Script watchdog ---------------------------------------------------------

      script-watchdog-enable = unset bool ''
        Enable the script watchdog (upstream default `true`).
      '';

      script-watchdog-enable-exception-handling = unset bool ''
        Let scripts handle watchdog trips via the
        `events.beforeWatchdogTerminate` event.
      '';

      script-watchdog-enable-shutdown = unset bool ''
        Shut the server down when a watchdog exception goes unhandled.
      '';

      script-watchdog-hang-exception = unset bool ''
        Throw a critical exception when a hang is detected, interrupting
        script execution.
      '';

      script-watchdog-hang-threshold = unset unsigned ''
        Single-tick hang threshold in milliseconds (upstream default 10000).
      '';

      script-watchdog-spike-threshold = unset unsigned ''
        Single-tick spike threshold; unset disables the warning.
      '';

      script-watchdog-slow-threshold = unset unsigned ''
        Threshold for scripts that stay slow over several ticks; unset
        disables the warning.
      '';

      script-watchdog-memory-warning = unset (types.ints.between 0 2000) ''
        Log a content warning when script memory use exceeds this many
        megabytes; `0` disables it (upstream default 100).
      '';

      script-watchdog-memory-limit = unset (types.ints.between 0 2000) ''
        Save and shut the world down when script memory use exceeds this many
        megabytes; `0` disables the limit (upstream default 250).
      '';

      ## Diagnostics and editor ----------------------------------------------------

      diagnostics-capture-auto-start = unset bool ''
        Start a diagnostics capture session when the level loads.
      '';

      diagnostics-capture-max-files = unset unsigned ''
        Number of diagnostics capture files to keep before cycling.
      '';

      diagnostics-capture-max-file-size = unset unsigned ''
        Size in bytes at which the diagnostics capture file is cycled.
      '';

      sentry-rate-limit-window = unset unsigned ''
        Window in seconds over which scripting errors reported to Sentry are
        rate limited; `0` disables the limit.
      '';

      sentry-max-events-per-window = unset unsigned ''
        Maximum number of events per window; `0` stops sending events to
        Sentry altogether.
      '';

      enable-profiler = unset bool ''
        Enable profiler support for performance analysis.
      '';

      enable-editor-network-metrics = unset bool ''
        Collect network metrics for live diagnostics in the debugger.
      '';

      convert-world-to-editor-project = unset bool ''
        Upgrade an existing vanilla world to an editor project on load.  Only
        has an effect when the server is started with the `Editor=true`
        command line flag.
      '';
    };

  ###########################################################################
  # Per-instance option declarations
  ###########################################################################
  serverOpts =
    { name, config, ... }:
    {
      options = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Whether to run this Bedrock server instance.  Defining a server
            enables it; set this to `false` to keep the declaration (and its
            state directory) around while the service is stopped.
          '';
        };

        package = mkOption {
          type = types.package;
          default = cfg.package;
          defaultText = literalExpression "config.services.minecraft-bedrock.package";
          description = ''
            The `minecraft-bedrock-server` package to run.  Per-instance so
            that a test world can be upgraded to a new Bedrock release before
            the others.
          '';
        };

        dataDir = mkOption {
          type = types.path;
          default = "${cfg.dataDir}/${name}";
          defaultText = literalExpression ''"''${config.services.minecraft-bedrock.dataDir}/<name>"'';
          description = ''
            State directory of this instance: worlds, logs and the mutable
            copies of the configuration files.  It may be empty (or missing)
            on first start; the server creates the world itself.

            Paths below `/var/lib` are managed with systemd's
            `StateDirectory=`, which creates them with the right ownership and
            re-owns them if the instance user changes.  Anywhere else, the
            directory is created by `systemd-tmpfiles` instead, and
            {option}`dynamicUser` must be `false`.
          '';
        };

        user = mkOption {
          type = types.str;
          default = "minecraft-bedrock-${name}";
          defaultText = literalExpression ''"minecraft-bedrock-<name>"'';
          description = ''
            UNIX user the server runs as.  With {option}`dynamicUser` enabled
            this is the name of the transient user systemd allocates; the
            world data stays owned by it across restarts.
          '';
        };

        dynamicUser = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Run the instance under systemd's `DynamicUser=`, so no permanent
            account exists on the system.  Set to `false` if you need a real
            account, e.g. to log in as the server user, run backups over SSH,
            or share a home directory between instances; the account is then
            created as a system user in the
            {option}`services.minecraft-bedrock.group` group.
          '';
        };

        fixStateOwnership = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Before each start, take ownership of everything in
            {option}`dataDir` for the instance user, as root.

            This is what makes copying a world in by hand work: files
            dropped there stay owned by whoever copied them, and with
            {option}`dynamicUser` the owning uid is transient and cannot be
            named in a `chown` command.  Symlinks are skipped, so the links
            into the Nix store are never followed.

            Turn it off if the state directory is large enough that walking it
            on every start hurts, or if you manage ownership yourself.
          '';
        };

        openFirewall = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Open the ports this instance listens on in the host firewall.
            Which ports those are is derived from `transport`, `server-port`,
            `server-portv6`, `enable-lan-visibility` and
            {option}`udpPortRange`.

            Leave this off for a server that should only be reachable from a
            VPN or from a machine on the same host.
          '';
        };

        udpPortRange = mkOption {
          type = types.nullOr (
            types.submodule {
              options = {
                from = mkOption {
                  type = types.port;
                  description = "First UDP port (inclusive).";
                };
                to = mkOption {
                  type = types.port;
                  description = "Last UDP port (inclusive).";
                };
              };
            }
          );
          default = null;
          example = {
            from = 32000;
            to = 32063;
          };
          description = ''
            Pin the UDP ports used by the NetherNet transport, by setting
            `server-udp-ports` and opening exactly this range when
            {option}`openFirewall` is set.

            Only meaningful with `transport = "nethernet"`, where the port on
            `server-port` carries just the HTTP/TCP signalling handshake and
            the actual gameplay traffic moves to negotiated UDP ports.  With
            this unset the server picks ephemeral ports, which cannot be
            opened in the firewall ahead of time.

            Allow for at least one port per concurrent player.
          '';
        };

        serverProperties = mkOption {
          type = types.submodule {
            # Declared sub-options give type checking and documentation for
            # every property of the packaged release; the free-form type keeps
            # anything they do not cover working.
            freeformType = propertiesFormat.type;
            options = serverPropertyOptions {
              inherit name;
              server = config;
            };
          };
          default = { };
          example = literalExpression ''
            {
              gamemode = "creative";
              difficulty = "peaceful";
              allow-cheats = true;
              max-players = 8;
              level-seed = "1234567890";
              view-distance = 24;
              # Not declared as a sub-option? Set it anyway.
              some-future-property = true;
            }
          '';
          description = ''
            Contents of `server.properties`, written out on every start.

            Every property of the packaged server version has a sub-option
            below carrying upstream's own default and documentation.  The ones
            upstream ships commented out default to `null`, which leaves them
            out of the generated file so the server applies its built-in
            default.  Properties that are not declared are accepted too, so a
            property added by a newer Bedrock release can be set right away.

            Four defaults deviate from upstream on purpose: `server-name` and
            `level-name` follow the instance name, `enable-lan-visibility` is
            only enabled when a single server is defined, and `allow-list`
            follows {option}`allowList`.

            Note that `level-name`, `level-seed` and -- unless
            `force-gamemode` is set -- `gamemode` are only read when the world
            is created; changing them later does not rewrite an existing
            world.
          '';
        };

        allowList = mkOption {
          type = types.nullOr (
            types.attrsOf (
              types.submodule (
                { name, ... }:
                {
                  options = {
                    name = mkOption {
                      type = types.str;
                      default = name;
                      defaultText = literalExpression "<name>";
                      description = "Xbox gamertag of the player.";
                    };
                    xuid = mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      description = ''
                        Optional XUID of the player.  When omitted the server
                        fills it in the first time a player with a matching
                        gamertag connects -- but since the file is rewritten
                        from this option on every start, prefer pinning it.
                      '';
                    };
                    ignoresPlayerLimit = mkOption {
                      type = types.bool;
                      default = false;
                      description = ''
                        Let this player join even when the server is full.
                      '';
                    };
                  };
                }
              )
            )
          );
          default = null;
          example = literalExpression ''
            {
              Notch = { };
              Jeb_ = {
                xuid = "2535000000000000";
                ignoresPlayerLimit = true;
              };
            }
          '';
          description = ''
            Declarative `allowlist.json`.  Setting this to anything but
            `null` also turns on the `allow-list` server property and makes
            the file fully managed: in-game `allowlist add` still works until
            the next restart, but is not persisted.

            Leave it at `null` to let the server own the file; `allowlist add`
            / `allowlist remove` on the console then persist into it, and
            `allowlist reload` re-reads it.
          '';
        };

        permissions = mkOption {
          type = types.nullOr (
            types.attrsOf (
              types.coercedTo types.str (permission: { inherit permission; }) (
                types.submodule (
                  { name, ... }:
                  {
                    options = {
                      xuid = mkOption {
                        type = types.str;
                        default = name;
                        defaultText = literalExpression "<name>";
                        description = "XUID of the player.";
                      };
                      permission = mkOption {
                        type = types.enum [
                          "visitor"
                          "member"
                          "operator"
                        ];
                        description = "Permission level granted to the player.";
                      };
                    };
                  }
                )
              )
            )
          );
          default = null;
          example = literalExpression ''
            {
              # Keyed by XUID ...
              "2535000000000000" = "operator";
              # ... or by any label, with the XUID given explicitly.
              kid = {
                xuid = "2535000000000001";
                permission = "member";
              };
            }
          '';
          description = ''
            Declarative `permissions.json`, mapping XUIDs to permission
            levels.  This requires `online-mode = true`, because a XUID is
            only known for players authenticated against Xbox Live; on a LAN
            server use `default-player-permission-level` instead.

            `visitor` may look but not build, `member` is a normal player and
            `operator` may use commands and the admin UI.  Anyone not listed
            gets {option}`serverProperties.default-player-permission-level`.

            A player's XUID is printed to the journal when they connect
            (`journalctl -u minecraft-bedrock-<name> | grep -i xuid`), and the
            running server can list what it currently uses with the
            `permission list` console command.

            When set, the file is managed: in-game `op`/`deop` apply to the
            running session but are reverted on the next restart.  Leave it at
            `null` to let the server own the file.
          '';
        };

        behaviorPacks = mkOption {
          type = types.attrsOf types.path;
          default = { };
          example = literalExpression "{ my-addon = ./packs/my-addon; }";
          description = ''
            Extra behaviour packs, merged with the ones shipped by the
            server.  Each attribute becomes a symlink below `behavior_packs`;
            an attribute that shadows a vanilla pack name wins.  Activating a
            pack for a world is still done in-game or through the world's
            `world_behavior_packs.json`.

            These stay read-only in the Nix store and are re-linked on every
            start, so they are the right place for packs you maintain
            declaratively.
          '';
        };

        resourcePacks = mkOption {
          type = types.attrsOf types.path;
          default = { };
          example = literalExpression "{ my-textures = ./packs/my-textures; }";
          description = ''
            Extra resource packs, merged with the ones shipped by the server.
            See {option}`behaviorPacks`.
          '';
        };

        packetLimitConfig = mkOption {
          type = types.nullOr jsonFormat.type;
          default = null;
          example = literalExpression ''
            {
              defaultAlgorithm = {
                name = "BucketPacketLimitAlgorithm";
                params = {
                  drainRatePerSec = 0.0013;
                  maxBucketSize = 1;
                };
              };
            }
          '';
          description = ''
            Declarative `packetlimitconfig.json`, used when the
            `enable-packet-rate-limiter` server property is on.  `null` seeds
            the file from the package once and then leaves it alone.
          '';
        };
      };

      # Defaults the module itself reads back (ports, transport, ...).  They
      # live here rather than in `serverProperties.default` so that each key
      # keeps its own priority: `mkOptionDefault` is the weakest priority, so
      # any plain definition by the user replaces it, while the module can
      # still see the effective value when building firewall rules.
      # `udpPortRange` is a convenience wrapper around the property: setting
      # it both configures the server and opens the matching firewall range.
      # `mkDefault` keeps an explicit `serverProperties.server-udp-ports`
      # winning over it.
      config.serverProperties =
        { }
        // lib.optionalAttrs (config.udpPortRange != null) {
          server-udp-ports = mkDefault "${toString config.udpPortRange.from}-${toString config.udpPortRange.to}";
        };
    };

  ###########################################################################
  # Per-instance systemd units
  ###########################################################################

  # Sending a command to a running server.  systemd owns the read end of the
  # FIFO for the whole lifetime of the socket unit, so writers never block and
  # the server never sees EOF on stdin.
  consoleScript = pkgs.writeShellApplication {
    name = "minecraft-bedrock-console";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      usage() {
        echo "usage: minecraft-bedrock-console <server> [command ...]" >&2
        echo "       reads commands from stdin when none are given" >&2
        exit 64
      }

      [ "$#" -ge 1 ] || usage
      fifo="/run/minecraft-bedrock/$1.stdin"
      shift

      if [ ! -p "$fifo" ]; then
        echo "minecraft-bedrock-console: $fifo does not exist; is the server running?" >&2
        exit 69
      fi

      if [ "$#" -gt 0 ]; then
        printf '%s\n' "$*" >"$fifo"
      else
        cat >"$fifo"
      fi
    '';
  };

  # Turns an empty -- or hand-populated -- state directory into a working
  # server root.  Idempotent, runs as the service user inside the same sandbox
  # as the server itself, and is the only thing that has to happen before the
  # server can be started on a fresh directory.
  setupScript =
    name: server:
    let
      root = serverRoot server;
    in
    pkgs.writeShellApplication {
      name = "${unitName name}-setup";
      runtimeInputs = [ pkgs.coreutils ];
      text = ''
        shopt -s nullglob
        umask 0027

        # Vanilla game data is managed from the store.  Refuse to clobber
        # anything real, so a directory left behind by an older, copy-based
        # setup is reported instead of silently nested inside a symlink.
        link() {
          if [ -e "$2" ] && [ ! -L "$2" ]; then
            echo "${unitName name}: $PWD/$2 exists and is not a symlink." >&2
            echo "It is managed from the Nix store; delete or rename it, then start again." >&2
            exit 1
          fi
          ln -sfn "$1" "$2"
        }

        # Pack directories are the exception: they stay *real* directories
        # holding one symlink per pack.  Packs copied in by hand -- a world
        # exported from a phone or tablet brings its own -- then sit next to
        # the packs from the store instead of colliding with a single big
        # symlink.  Links are rebuilt on every start, so a pack dropped from
        # the configuration disappears with it, while anything that is not a
        # symlink is left strictly alone.
        sync_packs() {
          # Convert a whole-directory symlink from an earlier module version.
          if [ -L "$2" ]; then rm -f "$2"; fi
          mkdir -p "$2"
          for pack in "$2"/*; do
            if [ -L "$pack" ]; then rm -f "$pack"; fi
          done
          for pack in "$1"/*; do
            if [ ! -e "$2/$(basename "$pack")" ]; then
              ln -s "$pack" "$2/$(basename "$pack")"
            fi
          done
        }

        # Immutable game data stays in the store: no copying of ~250 MB per
        # instance, and an upgrade is just a new symlink target.
        link ${root}/definitions definitions
        link ${root}/config config
        link ${root}/data data
        link ${root}/profanity_filter.wlist profanity_filter.wlist

        sync_packs ${mkPackDir server "behavior_packs" server.behaviorPacks} behavior_packs
        sync_packs ${mkPackDir server "resource_packs" server.resourcePacks} resource_packs

        # Managed configuration is installed as a real (writable) file: the
        # server rewrites some of these itself, and a read-only store symlink
        # would turn that into a runtime error.
        install -m 0640 ${propertiesFile server} server.properties
        ${
          if server.allowList != null then
            "install -m 0640 ${allowListFile server} allowlist.json"
          else
            "[ -e allowlist.json ] || install -m 0640 ${root}/allowlist.json allowlist.json"
        }
        ${
          if server.permissions != null then
            "install -m 0640 ${permissionsFile server} permissions.json"
          else
            "[ -e permissions.json ] || install -m 0640 ${root}/permissions.json permissions.json"
        }
        ${
          if server.packetLimitConfig != null then
            "install -m 0640 ${jsonFormat.generate "packetlimitconfig.json" server.packetLimitConfig} packetlimitconfig.json"
          else
            "[ -e packetlimitconfig.json ] || install -m 0640 ${root}/packetlimitconfig.json packetlimitconfig.json"
        }
      '';
    };

  # Run as root (via the "+" prefix) before the setup script, with the state
  # directory as the working directory.  `--reference=.` takes the owner
  # systemd itself gave that directory, which is the only way to name the user
  # of a `DynamicUser=` instance -- so a world copied in by hand as root ends
  # up owned by the server without anyone having to look up a transient uid.
  # Symlinks are skipped: dereferencing one would chown a store path.
  ownershipScript = pkgs.writeShellApplication {
    name = "minecraft-bedrock-fix-ownership";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
    ];
    text = ''
      find . -xdev ! -type l -exec chown --reference=. -- {} +
    '';
  };

  # Graceful shutdown.  The server only saves the world when it receives the
  # `stop` console command -- a plain SIGTERM loses everything since the last
  # autosave.  systemd starts killing the remaining processes as soon as
  # ExecStop returns, so this waits for the main process to disappear first.
  stopScript =
    name:
    pkgs.writeShellApplication {
      name = "${unitName name}-stop";
      runtimeInputs = [ pkgs.coreutils ];
      text = ''
        fifo=${escapeShellArg (fifoFor name)}
        [ -p "$fifo" ] || exit 0   # nothing to talk to; let systemd SIGTERM

        # Bounded, in case the socket unit died and nobody holds the read end
        # open any more -- opening a FIFO for writing blocks until it does.
        printf 'stop\n' | timeout 5 tee "$fifo" >/dev/null || exit 0

        # systemd starts killing leftover processes the moment this script
        # returns, so wait for the server to finish saving.  Bounded below
        # TimeoutStopSec=, which stays the backstop.
        [ -n "''${MAINPID:-}" ] || exit 0
        for _ in $(seq 60); do
          kill -0 "$MAINPID" 2>/dev/null || exit 0
          sleep 1
        done
      '';
    };

  mkService =
    name: server:
    let
      useStateDirectory = hasPrefix "/var/lib/" server.dataDir;
    in
    nameValuePair (unitName name) {
      description = "Minecraft Bedrock dedicated server (${name})";
      documentation = [
        "file://${server.package}/share/doc/minecraft-bedrock-server/bedrock_server_how_to.html"
      ];
      wantedBy = [ "multi-user.target" ];
      # `network-online.target` rather than `network.target`: the server binds
      # its sockets once at startup and (with online-mode) talks to Xbox Live.
      wants = [ "network-online.target" ];
      after = [
        "network-online.target"
        "${unitName name}.socket"
      ];
      requires = [ "${unitName name}.socket" ];

      # A server that cannot open its world (a corrupt save, or one that needs
      # content the server cannot load) exits non-zero every time, and
      # Restart=on-failure would otherwise retry every 5 s forever.  Give up
      # after five failures in five minutes and leave the unit failed, where
      # `systemctl --failed` shows it; `systemctl reset-failed` clears it.
      unitConfig = {
        StartLimitIntervalSec = 300;
        StartLimitBurst = 5;
      };

      serviceConfig = {
        # Declared explicitly rather than through NixOS' `preStart`, because
        # the order of the two matters: ownership is repaired as root before
        # the unprivileged setup script touches anything.
        ExecStartPre = optional server.fixStateOwnership "+${lib.getExe ownershipScript}" ++ [
          (lib.getExe (setupScript name server))
        ];
        ExecStart = "${server.package}/bin/bedrock_server";
        ExecStop = "${stopScript name}/bin/${unitName name}-stop";
        WorkingDirectory = server.dataDir;

        User = server.user;
        DynamicUser = server.dynamicUser;
        # A transient user always gets a transient group of the same name, so
        # the shared group can only be the *primary* group of a real account.
        # It is added as a supplementary group either way, because the console
        # FIFO is group-owned and ExecStop writes to it.
        SupplementaryGroups = [ cfg.group ];

        # stdin comes from the FIFO of the matching .socket unit; without
        # these two, stdout/stderr would default to the same socket instead of
        # the journal.
        StandardInput = "socket";
        StandardOutput = "journal";
        StandardError = "journal";

        Restart = "on-failure";
        RestartSec = 5;
        # Saving a large world on shutdown can take a while; must stay above
        # the wait loop in ExecStop.
        TimeoutStopSec = 90;

        UMask = "0027";
      }
      // lib.optionalAttrs (!server.dynamicUser) { Group = cfg.group; }
      // (
        if useStateDirectory then
          {
            StateDirectory = removePrefix "/var/lib/" server.dataDir;
            StateDirectoryMode = "0750";
          }
        else
          {
            ReadWritePaths = [ server.dataDir ];
          }
      )
      // {
        # Sandboxing.  The server is a closed-source binary that only needs its
        # own state directory and a network socket, so it can be locked down
        # hard.  Everything here is a plain systemd setting: override any of it
        # per instance with
        #   systemd.services."minecraft-bedrock-<name>".serviceConfig.X = ...;
        CapabilityBoundingSet = [ "" ];
        AmbientCapabilities = [ "" ];
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        # ProcSubset = "pid" is deliberately left out: it also hides
        # /proc/meminfo and /proc/cpuinfo, which closed-source runtimes tend
        # to read while sizing their thread and memory pools.
        ProtectSystem = "strict";
        RemoveIPC = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
          # getifaddrs(3) -- enumerating interfaces for LAN discovery.
          "AF_NETLINK"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
        ];
        # Not set: MemoryDenyWriteExecute -- the bundled scripting engine
        # JIT-compiles behaviour pack scripts.
      };
    };

  # One FIFO per instance, exposed as the server's stdin.  Declared as a
  # separate socket unit (rather than a tmpfiles FIFO) so that systemd holds
  # the read end open, removes it on stop, and applies the ownership below.
  mkSocket =
    name: _server:
    nameValuePair (unitName name) {
      description = "Console input for Minecraft Bedrock dedicated server (${name})";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenFIFO = fifoFor name;
        SocketMode = "0660";
        # Owned by root and readable/writable by the console group: the
        # service never opens the FIFO itself, systemd passes it the fd.
        SocketUser = "root";
        SocketGroup = cfg.group;
        RemoveOnStop = true;
        FlushPending = true;
      };
    };

  ###########################################################################
  # Firewall
  ###########################################################################
  # RakNet (the classic transport) is pure UDP on server-port/server-portv6,
  # plus the fixed 19132/19133 pair whenever LAN discovery is on.  NetherNet
  # instead speaks HTTP over TCP on server-port for signalling and moves
  # gameplay to negotiated UDP ports.
  firewallFor =
    server:
    let
      nethernet = prop server "transport" == "nethernet";
      port = prop server "server-port";
      portv6 = prop server "server-portv6";
      lan = prop server "enable-lan-visibility";
    in
    {
      allowedTCPPorts = optional nethernet port;
      allowedUDPPorts =
        if nethernet then
          [ ]
        else
          [
            port
            portv6
          ]
          ++ optionals lan [
            19132
            19133
          ];
      allowedUDPPortRanges = optional (nethernet && server.udpPortRange != null) {
        inherit (server.udpPortRange) from to;
      };
    };

  openedServers = filterAttrs (_: server: server.openFirewall) enabledServers;

  # Servers that need a real account created for them.
  staticUserServers = filterAttrs (_: server: !server.dynamicUser) enabledServers;

in
{
  options.services.minecraft-bedrock = {
    package = mkOption {
      type = types.package;
      default = pkgs.callPackage ./pkgs-minecraft-bedrock-server.nix { };
      defaultText = literalExpression "pkgs.callPackage ./pkgs-minecraft-bedrock-server.nix { }";
      description = ''
        Default `minecraft-bedrock-server` package for all instances.  Bedrock
        clients refuse to connect to a server of a different version, so this
        is what pins the version every world runs.

        Because the server binary cannot be redistributed automatically,
        the `bedrock-server.zip` archive must be manually downloaded and added
        to the Nix store before building:
          nix-store --add-fixed sha256 bedrock-server.zip
        or:
          nix-prefetch-url --type sha256 --name bedrock-server.zip file:///path/to/bedrock-server-<version>.zip
      '';
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/minecraft-bedrock";
      description = ''
        Parent directory for the per-instance state directories.
      '';
    };

    group = mkOption {
      type = types.str;
      default = "minecraft-bedrock";
      description = ''
        Group shared by all instances.  Members may write to the console FIFOs
        (`minecraft-bedrock-console <server> <command>`) and read the world
        data of every instance, which is what makes backups possible without
        root.
      '';
    };

    servers = mkOption {
      type = types.attrsOf (types.submodule serverOpts);
      default = { };
      example = literalExpression ''
        {
          # LAN world: no Microsoft account needed, shows up under "Friends".
          koti = {
            openFirewall = true;
            serverProperties = {
              online-mode = false;
              # Only defaulted to true when a single server is defined, since
              # LAN discovery binds the fixed ports 19132/19133.
              enable-lan-visibility = true;
              difficulty = "normal";
              allow-cheats = true;
              default-player-permission-level = "operator";
            };
          };

          # Internet-facing world for Microsoft/Xbox accounts only.
          julkinen = {
            openFirewall = true;
            serverProperties = {
              server-port = 19134;
              server-portv6 = 19135;
              online-mode = true;
              max-players = 8;
            };
            allowList = {
              Notch = { };
              Jeb_.ignoresPlayerLimit = true;
            };
            permissions."2535000000000000" = "operator";
          };
        }
      '';
      description = ''
        Bedrock server instances, one systemd service and one state directory
        each.  The attribute name is used for the unit name, the state
        directory, and by default for `server-name` and `level-name`.
      '';
    };
  };

  config = mkIf (enabledServers != { }) {

    # Fail the build on configurations that would only break at runtime --
    # usually as two servers silently fighting over the same UDP port.
    assertions =
      let
        # "<proto>/<port>", so that a NetherNet signalling port and a RakNet
        # gameplay port of the same number are not reported as a conflict.
        portsOf =
          server:
          lib.unique (
            if prop server "transport" == "nethernet" then
              [ "tcp/${toString (prop server "server-port")}" ]
            else
              [
                "udp/${toString (prop server "server-port")}"
                "udp/${toString (prop server "server-portv6")}"
              ]
              ++ optionals (prop server "enable-lan-visibility") [
                "udp/19132"
                "udp/19133"
              ]
          );
        allPorts = lib.concatMap portsOf (attrValues enabledServers);
        duplicatePorts = lib.unique (lib.filter (port: lib.count (p: p == port) allPorts > 1) allPorts);
      in
      [
        {
          assertion = duplicatePorts == [ ];
          message = ''
            services.minecraft-bedrock: more than one enabled server listens on ${concatStringsSep ", " duplicatePorts}.

            Give each instance its own `serverProperties.server-port` and
            `server-portv6`.  Note that `enable-lan-visibility` additionally
            binds the fixed ports 19132 and 19133 with the RakNet transport,
            so LAN discovery can only be enabled for one instance per host.
          '';
        }
      ]
      ++ mapAttrsToList (name: server: {
        assertion = server.dynamicUser -> hasPrefix "/var/lib/" server.dataDir;
        message = ''
          services.minecraft-bedrock.servers.${name}.dataDir is outside
          /var/lib, which needs a real account to own it.  Set
          `dynamicUser = false` (and pick a `user`), or move the directory
          under /var/lib.
        '';
      }) enabledServers
      ++ mapAttrsToList (name: server: {
        # Verified against the server binary: it logs "Using an allowlist
        # without online authentication can be dangerous and is not allowed"
        # and exits immediately, so catch the combination at build time.
        assertion = prop server "allow-list" -> prop server "online-mode";
        message = ''
          services.minecraft-bedrock.servers.${name} enables the allowlist
          (`allow-list`, implied by setting `allowList`) while
          `online-mode = false`.  The server refuses to start in that
          combination, because without Xbox Live authentication anyone can
          claim an allowlisted gamertag.

          Either turn `online-mode` back on, or drop `allowList` and set
          `serverProperties.allow-list = false` for this LAN server.
        '';
      }) enabledServers
      ++ mapAttrsToList (name: server: {
        # `server.properties` has no quoting: a `;` ends the value, and the
        # level name also becomes a directory name.
        assertion = !(lib.hasInfix ";" (toString (prop server "server-name")));
        message = ''
          services.minecraft-bedrock.servers.${name}: `server-name` must not
          contain a semicolon.
        '';
      }) enabledServers;

    warnings = lib.filter (warning: warning != "") (
      mapAttrsToList (
        name: server:
        optionalString (server.openFirewall && prop server "online-mode" == false) ''
          services.minecraft-bedrock.servers.${name} has `online-mode = false`
          while opening its ports in the firewall: anyone able to reach the
          host can join under any name, without a Microsoft account.  Either
          keep the server on a trusted network (`openFirewall = false`), or
          turn `online-mode` back on.
        ''
      ) enabledServers
      ++ mapAttrsToList (
        name: server:
        optionalString (server.permissions != null && prop server "online-mode" == false) ''
          services.minecraft-bedrock.servers.${name} declares `permissions`
          but has `online-mode = false`.  XUIDs are only resolved for
          Xbox-authenticated players, so the file has no effect; use
          `serverProperties.default-player-permission-level` instead.
        ''
      ) enabledServers
      ++ mapAttrsToList (
        name: server:
        optionalString
          (server.openFirewall && prop server "transport" == "nethernet" && server.udpPortRange == null)
          ''
            services.minecraft-bedrock.servers.${name} uses the NetherNet
            transport with `openFirewall`, but no `udpPortRange`: gameplay
            traffic will use ephemeral UDP ports that the firewall does not
            open.  Set `udpPortRange` to pin them.
          ''
      ) enabledServers
    );

    systemd.services = mapAttrs' mkService enabledServers;
    systemd.sockets = mapAttrs' mkSocket enabledServers;

    # Only for instances that opted out of DynamicUser; `listToAttrs` also
    # takes care of several instances sharing one account.
    users.users = lib.listToAttrs (
      mapAttrsToList (
        _: server:
        nameValuePair server.user {
          description = "Minecraft Bedrock dedicated server";
          group = cfg.group;
          home = server.dataDir;
          isSystemUser = true;
        }
      ) staticUserServers
    );

    # The group always exists: it owns the console FIFOs, which are created
    # even for DynamicUser instances.
    users.groups.${cfg.group} = { };

    # StateDirectory= covers the default location; anything else has to be
    # created before the unit's WorkingDirectory= is applied.
    systemd.tmpfiles.settings."10-minecraft-bedrock" = lib.listToAttrs (
      mapAttrsToList (
        _: server:
        nameValuePair server.dataDir {
          d = {
            user = server.user;
            group = cfg.group;
            mode = "0750";
          };
        }
      ) (filterAttrs (_: server: !hasPrefix "/var/lib/" server.dataDir) enabledServers)
    );

    networking.firewall = {
      allowedTCPPorts = lib.concatMap (server: (firewallFor server).allowedTCPPorts) (
        attrValues openedServers
      );
      allowedUDPPorts = lib.unique (
        lib.concatMap (server: (firewallFor server).allowedUDPPorts) (attrValues openedServers)
      );
      allowedUDPPortRanges = lib.concatMap (server: (firewallFor server).allowedUDPPortRanges) (
        attrValues openedServers
      );
    };

    environment.systemPackages = [ consoleScript ];
  };
}
