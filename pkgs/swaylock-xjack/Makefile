SHELL := /usr/bin/env bash

SWAYLOCK_REPO ?= https://github.com/swaywm/swaylock.git
SWAYLOCK_REF ?= v1.8.6
SRC_DIR ?= $(CURDIR)/build/swaylock-src
BUILD_DIR ?= $(CURDIR)/build/swaylock-build
PREFIX ?= /usr/local
PATCH_DIR := $(CURDIR)/patches
PATCHES := $(sort $(wildcard $(PATCH_DIR)/*.patch))

SUDO ?= sudo
DNF ?= dnf
MESON ?= meson

RHEL_DEPS := \
	gcc \
	git \
	meson \
	ninja-build \
	pkgconf-pkg-config \
	wayland-devel \
	wayland-protocols-devel \
	cairo-devel \
	gdk-pixbuf2-devel \
	libxkbcommon-devel \
	libdrm-devel \
	mesa-libEGL-devel \
	mesa-libGLES-devel \
	pam-devel

.PHONY: help print-rhel-deps deps-rhel source apply-patches configure build install install-pam-service auth-check clean distclean

help: ## Show available targets
	@grep -Eh '^[a-zA-Z0-9_.-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-22s\033[0m %s\n", $$1, $$2}'

print-rhel-deps: ## Print required RHEL dependency packages
	@printf '%s\n' $(RHEL_DEPS)

deps-rhel: ## Install build dependencies on RHEL host
	$(SUDO) $(DNF) install -y $(RHEL_DEPS)

source: ## Clone swaylock source and checkout SWAYLOCK_REF
	@mkdir -p "$(dir $(SRC_DIR))"
	@if [ ! -d "$(SRC_DIR)/.git" ]; then git clone "$(SWAYLOCK_REPO)" "$(SRC_DIR)"; fi
	git -C "$(SRC_DIR)" fetch --tags --force
	git -C "$(SRC_DIR)" checkout --force "$(SWAYLOCK_REF)"
	git -C "$(SRC_DIR)" reset --hard "$(SWAYLOCK_REF)"
	git -C "$(SRC_DIR)" clean -fdx

apply-patches: source ## Apply xjack patch stack to local source tree
	git -C "$(SRC_DIR)" am --abort || true
	git -C "$(SRC_DIR)" -c commit.gpgsign=false am $(PATCHES)

configure: apply-patches ## Configure build directory with meson
	@rm -rf "$(BUILD_DIR)"
	$(MESON) setup "$(BUILD_DIR)" "$(SRC_DIR)" --prefix="$(PREFIX)" -Dman-pages=disabled

build: configure ## Build swaylock-xjack
	$(MESON) compile -C "$(BUILD_DIR)"

install: build ## Install swaylock-xjack under PREFIX
	$(SUDO) $(MESON) install -C "$(BUILD_DIR)"

install-pam-service: ## Install PAM service file at /etc/pam.d/swaylock
	$(SUDO) install -Dm0644 "$(CURDIR)/pam/swaylock" /etc/pam.d/swaylock

auth-check: ## Verify linked PAM library and installed binary path
	@test -x "$(PREFIX)/bin/swaylock"
	@ldd "$(PREFIX)/bin/swaylock" | grep -E 'libpam\.so|pam\.so' || { echo "libpam linkage not detected"; exit 1; }
	@echo "OK: $(PREFIX)/bin/swaylock links to PAM on this host."

clean: ## Remove meson build directory
	rm -rf "$(BUILD_DIR)"

distclean: clean ## Remove all generated build artifacts
	rm -rf "$(CURDIR)/build"
