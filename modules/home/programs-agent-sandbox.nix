{ pkgs, ... }:

{
  home.packages = with pkgs; [
    agent-sandbox
  ];
}
