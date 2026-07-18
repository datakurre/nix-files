{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    configPath = ".mozilla/firefox";
    package = pkgs.wrapFirefox pkgs.unstable.firefox-unwrapped {
      extraPolicies = {
        SecurityDevices.opensc = "${pkgs.opensc}/lib/onepin-opensc-pkcs11.so";
      };
    };
  };
}
