#!/usr/bin/env bash
set -Eeuo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
config_file="$script_dir/nixgl-local.json"

if ! command -v nvidia-smi >/dev/null 2>&1; then
    printf 'nvidia-smi is required to determine the installed NVIDIA driver version.\n' >&2
    exit 1
fi

driver_version=$(
    nvidia-smi --query-gpu=driver_version --format=csv,noheader,nounits |
        sed -n '1{s/[[:space:]]//g;p;}'
)

if [[ ! "$driver_version" =~ ^[0-9]+([.][0-9]+)+$ ]]; then
    printf 'Unable to determine a valid NVIDIA driver version: %s\n' "$driver_version" >&2
    exit 1
fi

tmp_config="${config_file}.tmp.$$"
trap 'rm -f "$tmp_config"' EXIT
printf '%s\n' \
    '{' \
    '  "system": "x86_64-linux",' \
    '  "nvidiaVersion": "'"$driver_version"'",' \
    '  "wrapper": "nixGLNvidia"' \
    '}' >"$tmp_config"
mv "$tmp_config" "$config_file"
trap - EXIT

printf '\n'
printf 'nixGL configuration written for atsoukka.\n'
printf '  NVIDIA driver: %s\n' "$driver_version"
printf '  JSON config:     %s\n' "$config_file"
printf '\n'
printf 'Apply it with:\n'
printf '  make "switch atsoukka"\n'
