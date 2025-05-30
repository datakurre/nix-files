{ config, pkgs, ... }:
{
  security.pki.certificateFiles = [
    ./env-ca/DigiCertAssuredIDRootCA.pem
    ./env-ca/VeriSignClass3PublicPrimaryCertificationAuthority-G5.pem
  ];
}
