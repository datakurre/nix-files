requirements.nix: requirements.txt
	nix-shell -A pip2nix \
	  --run "pip2nix generate -r requirements.txt --output=requirements.nix" \
	  --arg "pkgs" "import <nixpkgs> {}" \
	  --arg "pythonPackages" "(import <nixpkgs> {}).pythonPackages"
