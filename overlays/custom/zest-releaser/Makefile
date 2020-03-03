PYTHON ?= python37
NIX_OPTIONS ?= --pure --argstr python $(PYTHON)

.PHONY: all
all: requirements-${PYTHON}.nix

requirements-$(PYTHON).nix: requirements-$(PYTHON).txt
	nix-shell release.nix $(NIX_OPTIONS) -A pip2nix --run "pip2nix generate -r requirements-$(PYTHON).txt --output=requirements-$(PYTHON).nix"

requirements-$(PYTHON).txt: requirements.txt
	nix-shell release.nix $(NIX_OPTIONS) -A pip2nix --run "pip2nix generate -r requirements.txt --output=requirements-$(PYTHON).nix"
	@grep "pname =\|version =" requirements-$(PYTHON).nix|awk "ORS=NR%2?FS:RS"|sed 's|.*"\(.*\)";.*version = "\(.*\)".*|\1==\2|' > requirements-$(PYTHON).txt
