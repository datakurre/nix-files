{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
      extraPolicies = {
        SecurityDevices.opensc = "${pkgs.opensc}/lib/onepin-opensc-pkcs11.so";
      };
    };
  };
}
