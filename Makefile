help:
	@grep -Eh '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' | uniq

switch\ albemuth:  ## Switch NixOS on albemuth
	nixos-rebuild switch --flake .#albemuth

switch\ makondo:  ## Switch NixOS on makondo
	nixos-rebuild switch --flake .#makondo

switch\ atsoukka:  ## Switch home-manager on atsoukka
	home-manager --extra-experimental-features "nix-command flakes" switch --flake .#atsoukka

atsoukka:  ## Switch home-manager on atsoukka
	home-manager --extra-experimental-features "nix-command flakes" switch --flake .#atsoukka

check:  ## Run nix flake check
	nix flake check

format:  ## Format nix files
	@nix fmt
