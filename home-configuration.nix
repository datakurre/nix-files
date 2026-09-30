{
  config,
  pkgs,
  lib,
  nixgl,
  operatonBpmnModeler,
  ...
}:
let
  nixglConfig =
    if builtins.pathExists ./nixgl-local.json then
      builtins.fromJSON (builtins.readFile ./nixgl-local.json)
    else
      null;
  nixglPackage = lib.optional (nixglConfig != null && nixglConfig.nvidiaVersion != null) (
    let
      nixglPkgs = import nixgl.inputs.nixpkgs {
        system = nixglConfig.system;
        config.allowUnfree = true;
      };
      nixglWrappers = import (nixgl + "/default.nix") {
        pkgs = nixglPkgs;
        nvidiaVersion = nixglConfig.nvidiaVersion;
      };
    in
    pkgs.runCommand "nixGLNvidia" { } ''
      mkdir -p $out/bin
      ln -s ${nixglWrappers.nixGLNvidia}/bin/nixGLNvidia-${nixglConfig.nvidiaVersion} $out/bin/nixGLNvidia
    ''
  );
  bpmnModeler = lib.optional (nixglConfig != null && nixglConfig.nvidiaVersion != null) (
    pkgs.writeShellScriptBin "bpmn-modeler" ''
      exec env GDK_BACKEND=x11 nixGLNvidia \
        ${operatonBpmnModeler.packages.${pkgs.system}.default}/bin/operaton-modeler "$@"
    ''
  );
  tmpDir = "${config.home.homeDirectory}/MyTemp";
in
{
  imports = (import ./modules/home/default.nix) ++ [
    ./modules/home/services-river.nix
  ];
  programs.home-manager.enable = true;
  programs.chromium.enable = true;
  # dconf D-Bus activation requires a running GNOME/dconf session; on RHEL 9
  # the system-level dconf service is not set up by Home Manager alone (only
  # NixOS does this via programs.dconf.enable).  Disable here to prevent
  # `home-manager switch` from failing outside a desktop session.
  dconf.enable = lib.mkForce false;
  home.sessionVariables.TMPDIR = tmpDir;
  home.packages = nixglPackage ++ bpmnModeler;
  programs.nushell.environmentVariables.TMPDIR = tmpDir;
  # Unlike bash (see .bashrc.d/99-nix.sh below), nushell never sources any
  # POSIX shell profile scripts, so it never picks up nix.sh. Prepend the
  # common Nix profile bin dirs here so `nix`, `home-manager`, etc. are on
  # PATH when nu is used directly (e.g. as the login shell).
  programs.nushell.extraEnv = ''
    let nix_profile_bins = [
      ($env.HOME | path join ".nix-profile" "bin")
      ($env.HOME | path join ".local" "state" "nix" "profile" "bin")
      "/nix/var/nix/profiles/default/bin"
    ]
    for nix_profile_bin in $nix_profile_bins {
      if ($nix_profile_bin | path exists) {
        if ($env.PATH | describe | str starts-with "list") {
          if not ($nix_profile_bin in $env.PATH) {
            $env.PATH = ($env.PATH | prepend $nix_profile_bin)
          }
        } else {
          $env.PATH = ($nix_profile_bin + (char esep) + $env.PATH)
        }
      }
    }
  '';
  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
  home.file.".bashrc.d/99-nix.sh".text = ''
    for nix_profile_sh in \
      "$HOME/.nix-profile/etc/profile.d/nix.sh" \
      "$HOME/.nix-profile/etc/profile.d/nix-daemon.sh" \
      "$HOME/.local/state/nix/profile/etc/profile.d/nix.sh" \
      "$HOME/.local/state/nix/profile/etc/profile.d/nix-daemon.sh" \
      "/nix/var/nix/profiles/default/etc/profile.d/nix.sh" \
      "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
    do
      if [ -r "$nix_profile_sh" ]; then
        . "$nix_profile_sh"
        break
      fi
    done
    export TMPDIR=${tmpDir}
    if command -v nu >/dev/null 2>&1; then
      export SHELL="$(command -v nu)"
      export XTERM_SHELL="$SHELL"
    fi
  '';
  services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "default";
        profile.outputs = [
          {
            criteria = "*";
            scale = 2.0;
          }
        ];
      }
    ];
  };
}
