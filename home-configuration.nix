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
  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
  home.file.".bashrc.d/99-nix.sh".text = ''
    . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
    export TMPDIR=${tmpDir}
    export SHELL=${config.home.homeDirectory}/.nix-profile/bin/nu
    export XTERM_SHELL=${config.home.homeDirectory}/.nix-profile/bin/nu
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
