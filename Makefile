NIX_FILES = $(find . -name "*.nix")
switch:
	nixos-rebuild switch --flake .

format:
	nix fmt $(NIX_FILES)
