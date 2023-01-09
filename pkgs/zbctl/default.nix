{ src, version, buildGoModule }:

buildGoModule rec {
  name = "zbctl-${version}";
  modRoot = "./clients/go/cmd/zbctl";
  vendorSha256 = "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5";
  preBuild = ''
    source $stdenv/setup
    patchShebangs build.sh
  '';
  doCheck = false;  # requires docker
  inherit src version;
}
