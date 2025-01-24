help:
	@grep -Eh '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' | uniq

NIX_FILES = $(shell find . -name "*.nix")

switch\ albemuth:
	nixos-rebuild switch --flake .#albemuth

switch\ makondo:
	nixos-rebuild switch --flake .#makondo

format:
	@nix fmt $(NIX_FILES)
