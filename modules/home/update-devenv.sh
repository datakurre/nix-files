#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

API_URL="https://open-vsx.org/api/datakurre/devenv"
LATEST_INFO=$(curl -s "$API_URL")

VERSION=$(echo "$LATEST_INFO" | jq -r '.version')
DOWNLOAD_URL=$(echo "$LATEST_INFO" | jq -r '.files.download')

echo "Latest version of datakurre.devenv: $VERSION"
echo "Fetching sha256 from $DOWNLOAD_URL..."

SHA256=$(nix-prefetch-url "$DOWNLOAD_URL" 2>/dev/null)

cat > datakurre.devenv.json <<EOF
{
  "version": "$VERSION",
  "url": "$DOWNLOAD_URL",
  "sha256": "$SHA256"
}
EOF

echo "Successfully updated datakurre.devenv.json to version $VERSION"
