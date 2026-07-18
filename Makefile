help:
	@grep -Eh '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' | uniq

switch\ albemuth:
	nixos-rebuild switch --flake .#albemuth

switch\ makondo:
	nixos-rebuild switch --flake .#makondo

switch\ atsoukka:
	home-manager switch --flake .#homeConfigurations.atsoukka

check:
	nix flake check

format:
	@nix fmt
