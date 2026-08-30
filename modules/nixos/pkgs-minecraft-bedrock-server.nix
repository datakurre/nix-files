# Package expression for Mojang's *Minecraft: Bedrock Edition* dedicated
# server.  The upstream release is a prebuilt, closed-source x86-64 binary
# shipped as a zip, so this is a "binary repackaging" derivation:
#
#   * `autoPatchelfHook` rewrites the ELF interpreter and RUNPATH so the
#     Ubuntu-targeted binary finds glibc/libgcc from the Nix store.  This is
#     preferred over `buildFHSEnv`, because an FHS sandbox needs user
#     namespaces and `mount`, which forces us to disable most of the systemd
#     hardening in the service module.
#   * `runtimeDependencies` is used for libraries the server `dlopen()`s
#     rather than links against (libcurl, used for Xbox Live / Microsoft
#     account authentication and telemetry).  Those never show up in
#     DT_NEEDED, so autoPatchelf cannot discover them on its own.
#
# `nix-shell -p nix-index` / `patchelf --print-needed` were used to derive the
# dependency list; re-check it when bumping to a new upstream release.
{
  lib,
  stdenv,
  autoPatchelfHook,
  unzip,
  curl,
  requireFile,
  version ? "1.26.45.1",
  hash ? "sha256-sNuGCY7kGKm7Im9vP1H/K+NlQiNoOTdWJ7Ka7z36XNo=",
  # The Minecraft EULA does not allow redistribution, and Mojang's download
  # endpoint rejects non-browser user agents, so the release zip must be
  # downloaded manually and added to the Nix store with:
  #
  #   nix-store --add-fixed sha256 bedrock-server.zip
  # or
  #   nix-prefetch-url --type sha256 --name bedrock-server.zip file:///path/to/bedrock-server-${version}.zip
  #
  # Deliberately *not* called `src`: `callPackage` fills every argument whose
  # name exists in `pkgs` -- and `pkgs.src` does -- which would shadow the
  # default below with an unrelated (throwing) package.
  srcZip ? requireFile {
    name = "bedrock-server.zip";
    inherit hash;
    url = "https://www.minecraft.net/en-us/download/server/bedrock";
    message = ''
      Unfortunately, Mojang does not allow redistribution of the Minecraft Bedrock Dedicated Server.
      Please download the Linux server zip from https://www.minecraft.net/en-us/download/server/bedrock
      (or https://www.minecraft.net/bedrockdedicatedserver/bin-linux/bedrock-server-${version}.zip)
      and add it to the Nix store using:

        nix-store --add-fixed sha256 bedrock-server.zip

      or if downloaded with its versioned filename:

        nix-prefetch-url --type sha256 --name bedrock-server.zip file:///path/to/bedrock-server-${version}.zip
    '';
  },
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "minecraft-bedrock-server";
  inherit version;
  src = srcZip;

  # The zip has no top-level directory; unpack straight into the build dir.
  sourceRoot = ".";

  nativeBuildInputs = [
    unzip
    autoPatchelfHook
  ];

  # DT_NEEDED: libgcc_s (from stdenv.cc.cc.lib) plus plain glibc.  libstdc++
  # is statically linked into the binary by Mojang.
  buildInputs = [ (lib.getLib stdenv.cc.cc) ];

  # dlopen()ed at runtime, so it must be appended to the RUNPATH explicitly.
  runtimeDependencies = [ (lib.getLib curl) ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 bedrock_server $out/bin/bedrock_server

    # The profiler bootstrap is resolved relative to the *executable*, not to
    # the working directory, so it has to sit next to the binary as well.
    cp -r data $out/bin/data

    # Everything else is the read-only server root: the game data the server
    # resolves relative to its working directory, plus the pristine
    # configuration files the NixOS module seeds a fresh state dir with.
    # The service module symlinks/copies these into the instance's data
    # directory; nothing here is ever written to at runtime.
    install -d $out/lib/minecraft-bedrock-server
    cp -r \
      behavior_packs resource_packs definitions config data \
      server.properties allowlist.json permissions.json \
      packetlimitconfig.json profanity_filter.wlist \
      $out/lib/minecraft-bedrock-server/

    install -Dm644 bedrock_server_how_to.html release-notes.txt \
      -t $out/share/doc/minecraft-bedrock-server

    runHook postInstall
  '';

  # The binary is huge (~250 MB) and already stripped upstream.
  dontStrip = true;

  meta = {
    description = "Minecraft: Bedrock Edition dedicated server";
    homepage = "https://www.minecraft.net/en-us/download/server/bedrock";
    license = lib.licenses.unfreeRedistributable; # Minecraft EULA
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    platforms = [ "x86_64-linux" ];
    mainProgram = "bedrock_server";
  };
})
