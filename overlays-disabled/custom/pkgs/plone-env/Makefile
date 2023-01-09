PYTHON = python27

requirements-$(PYTHON).nix: requirements.txt
	nix-shell -p libffi \
	--run 'nix-shell setup.nix -A pip2nix \
	--run "pip2nix generate -r requirements.txt --output=requirements-$(PYTHON).nix"'

.PHONY: freeze
freeze:
	grep "pname =\|version =" requirements-$(PYTHON).nix|awk "ORS=NR%2?FS:RS"|sed 's|.*"\(.*\)";.*version = "\(.*\)".*|\1==\2|' > requirements-$(PYTHON).txt
