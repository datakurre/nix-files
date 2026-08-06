#!/usr/bin/env python3
import urllib.request
import json
import subprocess
import sys
import os

EXTENSIONS = {
    "datakurre.devenv": "openvsx",
    "datakurre.vscode-operaton-form-js-modeler": "marketplace",
    "datakurre.vscode-operaton-bpmn-js-modeler": "marketplace",
    "datakurre.vscode-operaton-dmn-js-modeler": "marketplace"
}

def get_marketplace_ext(publisher, name):
    url = "https://marketplace.visualstudio.com/_apis/public/gallery/extensionquery"
    req_body = json.dumps({
        "filters": [{"criteria": [{"filterType": 7, "value": f"{publisher}.{name}"}]}],
        "flags": 914
    }).encode('utf-8')
    req = urllib.request.Request(url, data=req_body, headers={
        'Accept': 'application/json;api-version=3.0-preview.1',
        'Content-Type': 'application/json'
    }, method='POST')
    
    with urllib.request.urlopen(req) as response:
        data = json.loads(response.read().decode())
        
    latest_version = data['results'][0]['extensions'][0]['versions'][0]
    
    vsix_url = None
    for file in latest_version['files']:
        if file['assetType'] == 'Microsoft.VisualStudio.Services.VSIXPackage':
            vsix_url = file['source']
            break
    if not vsix_url:
        vsix_url = latest_version['fallbackAssetUri'] + '/Microsoft.VisualStudio.Services.VSIXPackage'
        
    return latest_version['version'], vsix_url

def get_openvsx_ext(publisher, name):
    url = f"https://open-vsx.org/api/{publisher}/{name}/latest"
    req = urllib.request.Request(url)
    with urllib.request.urlopen(req) as response:
        data = json.loads(response.read().decode())
    return data['version'], data['files']['download']

def main():
    results = {}
    for ext_id, registry in EXTENSIONS.items():
        publisher, name = ext_id.split('.')
        print(f"Fetching latest info for {ext_id} from {registry}...")
        
        if registry == "marketplace":
            version, url = get_marketplace_ext(publisher, name)
        else:
            version, url = get_openvsx_ext(publisher, name)
            
        print(f" -> Found v{version}. Prefetching sha256 hash...")
        res = subprocess.run(['nix-prefetch-url', url], capture_output=True, text=True)
        if res.returncode != 0:
            print(f"Failed to prefetch url: {res.stderr}", file=sys.stderr)
            sys.exit(1)
            
        results[ext_id] = {
            "version": version,
            "url": url,
            "sha256": res.stdout.strip(),
            "publisher": publisher,
            "name": name
        }
        
    out_path = os.path.join(os.path.dirname(__file__), 'manual-extensions.json')
    with open(out_path, 'w') as f:
        json.dump(results, f, indent=2)
        
    print(f"Successfully updated {out_path}")

if __name__ == "__main__":
    main()
