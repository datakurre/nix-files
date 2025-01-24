{ pkgs, config, ... }:
{
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  #networking.useDHCP = true;
  networking.interfaces.enp0s20f0u4.useDHCP = true;
  networking.interfaces.enp0s31f6.useDHCP = true;

  # Graphics
  hardware.opengl.enable = true;
  hardware.nvidia.powerManagement.enable = true;
  # hardware.opengl.extraPackages = [ pkgs.mesa.drivers ];
  services.xserver.videoDrivers = [ "nvidia" ];
  # services.xserver.videoDrivers = [ "intel" ];

  # services.resilio.enable = true;
  # services.resilio.enableWebUI = true;

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "zoom"
      "cudatoolkit"
      "cudatoolkit-11-cudnn"
      "libcublas"
      "nvidia-x11"
      "nvidia-settings"
      "corefonts"
      "Oracle_VM_VirtualBox_Extension_Pack"
    ];
  virtualisation.virtualbox.host.enableExtensionPack = true;

  services.openssh.enable = true;
  services.openssh.settings.X11Forwarding = true;
  services.openssh.allowSFTP = true;
  services.openssh.settings.PasswordAuthentication = false;
  services.openssh.settings.PermitRootLogin = "no";
  services.openssh.extraConfig = ''
    HostkeyAlgorithms +ssh-rsa
    PubkeyAcceptedAlgorithms +ssh-rsa
  '';

  # Initial system version
  system.stateVersion = "22.11";

  services.xserver.displayManager.xserverArgs = [ "-dpi 224" ];
  services.libinput.enable = true;
  services.libinput.touchpad.accelProfile = "adaptive";
  services.libinput.touchpad.accelSpeed = "0.7";
  services.libinput.touchpad.scrollMethod = "twofinger";
  services.libinput.touchpad.tapping = false;
  services.libinput.touchpad.tappingDragLock = false;
  services.libinput.touchpad.naturalScrolling = true;
  services.libinput.touchpad.disableWhileTyping = true;
  services.libinput.touchpad.additionalOptions = ''
    Option "SendEventsMode" "disabled-on-external-mouse"
  '';

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
    HandleHibernateKey=ignore
    HandleLidSwitch=ignore
    RuntimeDirectorySize=12G
  '';
  services.acpid.enable = true;
  services.acpid.powerEventCommands = ''
    systemctl suspend
  '';
  services.acpid.lidEventCommands = ''
    systemctl hibernate
  '';
  services.acpid.acEventCommands = ''
    if [ `cat /sys/class/power_supply/ACAD/online` -eq 0 ]; then
      tee /sys/class/backlight/nvidia_0/brightness <<< 1000
      /run/current-system/sw/bin/cpupower frequency-set -u 1.50GHz
      tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor <<< powersave
    else
      tee /sys/class/backlight//brightness <<< 3000
      tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor <<< performance
      /run/current-system/sw/bin/cpupower frequency-set -u 3.30GHz
    fi
  '';
  services.acpid.handlers.brightnessDown.event = "video/brightnessdown.*";
  services.acpid.handlers.brightnessDown.action = ''
    val=`cat /sys/class/backlight/nvidia_0/brightness`
    tee /sys/class/backlight/nvidia_0/brightness <<< `expr $val - 1`
  '';
  services.acpid.handlers.brightnessUp.event = "video/brightnessUp.*";
  services.acpid.handlers.brightnessUp.action = ''
    val=`cat /sys/class/backlight/nvidia_0/brightness`
    tee /sys/class/backlight/nvidia_0/brightness <<< `expr $val + 1`
  '';
}
