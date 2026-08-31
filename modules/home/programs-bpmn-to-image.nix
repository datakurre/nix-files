{ pkgs, ... }:

{
  home.packages = with pkgs; [
    bpmn-to-image
  ];
}
