# Generated by pip2nix 0.8.0.dev1
# See https://github.com/johbo/pip2nix

{ pkgs, fetchurl, fetchgit, fetchhg }:

self: super: {
  "PyYAML" = super.buildPythonPackage {
    name = "PyYAML-5.1";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/9f/2c/9417b5c774792634834e730932745bc09a7d36754ca00acf1ccd1ac2594d/PyYAML-5.1.tar.gz";
      sha256 = "15czj11s2bcgchn2jx81k0jmswf2hjxry5cq820h7hgpxiscfss3";
    };
  };
  "argh" = super.buildPythonPackage {
    name = "argh-0.26.2";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/e3/75/1183b5d1663a66aebb2c184e0398724b624cecd4f4b679cb6e25de97ed15/argh-0.26.2.tar.gz";
      sha256 = "0rdv0n2aa181mkrybwvl3czkrrikgzd4y2cri6j735fwhj65nlz9";
    };
  };
  "asn1crypto" = super.buildPythonPackage {
    name = "asn1crypto-0.24.0";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/fc/f1/8db7daa71f414ddabfa056c4ef792e1461ff655c2ae2928a2b675bfed6b4/asn1crypto-0.24.0.tar.gz";
      sha256 = "0jaf8rf9dx1lf23xfv2cdd5h52f1qr3w8k63985bc35g3d220p4x";
    };
  };
  "cffi" = super.buildPythonPackage {
    name = "cffi-1.12.3";
    doCheck = false;
    propagatedBuildInputs = [
      self."pycparser"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/93/1a/ab8c62b5838722f29f3daffcc8d4bd61844aa9b5f437341cc890ceee483b/cffi-1.12.3.tar.gz";
      sha256 = "0x075521fxwv0mfp4cqzk7lvmw4n94bjw601qkcv314z5s182704";
    };
  };
  "cryptography" = super.buildPythonPackage {
    name = "cryptography-2.6.1";
    doCheck = false;
    propagatedBuildInputs = [
      self."asn1crypto"
      self."cffi"
      self."enum34"
      self."ipaddress"
      self."six"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/07/ca/bc827c5e55918ad223d59d299fff92f3563476c3b00d0a9157d9c0217449/cryptography-2.6.1.tar.gz";
      sha256 = "19iwz5avym5zl6jrrrkym1rdaa9h61j20ph4cswsqgv8xg5j3j16";
    };
  };
  "dataflake-fakeldap" = super.buildPythonPackage {
    name = "dataflake-fakeldap-1.0";
    doCheck = false;
    propagatedBuildInputs = [
      self."python-ldap"
      self."setuptools"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/20/f4/adb8dcb2646fc559c6a78bdd01dd5f7809efee2695871d3c78168f8c328d/dataflake.fakeldap-1.0.tar.gz";
      sha256 = "1ll53h4hdnvac2az0vyh483c8333r6g44sz077ddisrc5wq1y7jj";
    };
  };
  "enum34" = super.buildPythonPackage {
    name = "enum34-1.1.6";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/bf/3e/31d502c25302814a7c2f1d3959d2a3b3f78e509002ba91aea64993936876/enum34-1.1.6.tar.gz";
      sha256 = "1cgm5ng2gcfrkrm3hc22brl6chdmv67b9zvva9sfs7gn7dwc9n4a";
    };
  };
  "fancycompleter" = super.buildPythonPackage {
    name = "fancycompleter-0.8";
    doCheck = false;
    propagatedBuildInputs = [
      self."pyrepl"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/fd/e3/da39a6cfaffe578a01221261ac1d5d99c48d44f6377ff0de3a12dd332cec/fancycompleter-0.8.tar.gz";
      sha256 = "04lvacix99d242b1jv2qxdi6gq1djv8w1i3racliydqj6lgjylnj";
    };
  };
  "gnureadline" = super.buildPythonPackage {
    name = "gnureadline-6.3.8";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/50/64/86085c823cd78f9df9d8e33dce0baa71618016f8860460b82cf6610e1eb3/gnureadline-6.3.8.tar.gz";
      sha256 = "0ddhj98x2nv45iz4aadk4b9m0b1kpsn1xhcbypn5cd556knhiqjq";
    };
  };
  "ipaddress" = super.buildPythonPackage {
    name = "ipaddress-1.0.22";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/97/8d/77b8cedcfbf93676148518036c6b1ce7f8e14bf07e95d7fd4ddcb8cc052f/ipaddress-1.0.22.tar.gz";
      sha256 = "0b570bm6xqpjwqis15pvdy6lyvvzfndjvkynilcddjj5x98wfimi";
    };
  };
  "kerberos" = super.buildPythonPackage {
    name = "kerberos-1.3.0";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/34/18/9c86fdfdb27e0f7437b7d5a9e22975dcc382637b2a68baac07843be512fc/kerberos-1.3.0.tar.gz";
      sha256 = "19663qxmma0i8bfbjc2iwy5hgq0g4pfb75r023v5dps68zfvffgh";
    };
  };
  "lxml" = super.buildPythonPackage {
    name = "lxml-4.3.3";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/7d/29/174d70f303016c58bd790c6c86e6e86a9d18239fac314d55a9b7be501943/lxml-4.3.3.tar.gz";
      sha256 = "141xvx096bh5xm8mhb4nrycgy1fp12ahnklh6h1a2dcf5xlds0sa";
    };
  };
  "pathlib" = super.buildPythonPackage {
    name = "pathlib-1.0.1";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/ac/aa/9b065a76b9af472437a0059f77e8f962fe350438b927cb80184c32f075eb/pathlib-1.0.1.tar.gz";
      sha256 = "17zajiw4mjbkkv6ahp3xf025qglkj0805m9s41c45zryzj6p2h39";
    };
  };
  "pathtools" = super.buildPythonPackage {
    name = "pathtools-0.1.2";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/e7/7f/470d6fcdf23f9f3518f6b0b76be9df16dcc8630ad409947f8be2eb0ed13a/pathtools-0.1.2.tar.gz";
      sha256 = "1h7iam33vwxk8bvslfj4qlsdprdnwf8bvzhqh3jq5frr391cadbw";
    };
  };
  "pdbpp" = super.buildPythonPackage {
    name = "pdbpp-0.10.0";
    doCheck = false;
    propagatedBuildInputs = [
      self."fancycompleter"
      self."pygments"
      self."wmctrl"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/ec/c3/cf957fa98406ef25422b3324dee70b3624dacdd05602201af356234059c4/pdbpp-0.10.0.tar.gz";
      sha256 = "1pf4pcnaa4q40nvww3dyyw9zllm1pnwyspmlcsyr4bgkxh1anzpf";
    };
  };
  "pillow" = super.buildPythonPackage {
    name = "pillow-6.0.0";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/81/1a/6b2971adc1bca55b9a53ed1efa372acff7e8b9913982a396f3fa046efaf8/Pillow-6.0.0.tar.gz";
      sha256 = "1dgbhamlr5gxk9avfvmq3ivqqp6w9fpp2grinpbvqb03x4n0m740";
    };
  };
  "pyasn1" = super.buildPythonPackage {
    name = "pyasn1-0.4.5";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/46/60/b7e32f6ff481b8a1f6c8f02b0fd9b693d1c92ddd2efb038ec050d99a7245/pyasn1-0.4.5.tar.gz";
      sha256 = "1xqh3jh2nfi2bflk5a0vn59y3pp1vn54f3ksx652sid92gz2096s";
    };
  };
  "pyasn1-modules" = super.buildPythonPackage {
    name = "pyasn1-modules-0.2.5";
    doCheck = false;
    propagatedBuildInputs = [
      self."pyasn1"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/ec/0b/69620cb04a016e4a1e8e352e8a42717862129b574b3479adb2358a1f12f7/pyasn1-modules-0.2.5.tar.gz";
      sha256 = "15nvfx0vnl8akdlv3k6s0n80vqvryj82bm040jdsn7wmyxl1ywpg";
    };
  };
  "pycparser" = super.buildPythonPackage {
    name = "pycparser-2.19";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/68/9e/49196946aee219aead1290e00d1e7fdeab8567783e83e1b9ab5585e6206a/pycparser-2.19.tar.gz";
      sha256 = "1cr5dcj9628lkz1qlwq3fv97c25363qppkmcayqvd05dpy573259";
    };
  };
  "pycrypto" = super.buildPythonPackage {
    name = "pycrypto-2.6.1";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/60/db/645aa9af249f059cc3a368b118de33889219e0362141e75d4eaf6f80f163/pycrypto-2.6.1.tar.gz";
      sha256 = "0g0ayql5b9mkjam8hym6zyg6bv77lbh66rv1fyvgqb17kfc1xkpj";
    };
  };
  "pycryptodome" = super.buildPythonPackage {
    name = "pycryptodome-3.8.1";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/c0/1e/5398467ff0fcb39dbb23412c5ea9c9b2db2b659282e49d4073daae952247/pycryptodome-3.8.1.tar.gz";
      sha256 = "1cv1fw5qff2pdvg80llbh51a7hp3mdsqaibzpck7lmvllgj0rbb8";
    };
  };
  "pygments" = super.buildPythonPackage {
    name = "pygments-2.3.1";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/64/69/413708eaf3a64a6abb8972644e0f20891a55e621c6759e2c3f3891e05d63/Pygments-2.3.1.tar.gz";
      sha256 = "0ji87g09jph8jqcvclgb02qvxasdnr9pzvk90rl66d90yqcxmyjz";
    };
  };
  "pyrepl" = super.buildPythonPackage {
    name = "pyrepl-0.9.0";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/05/1b/ea40363be0056080454cdbabe880773c3c5bd66d7b13f0c8b8b8c8da1e0c/pyrepl-0.9.0.tar.gz";
      sha256 = "0xd7h7k5cg9gd8nkqdykzkxmfasg8wwxcrmrpdqyh0jm9grp0999";
    };
  };
  "pyscss" = super.buildPythonPackage {
    name = "pyscss-1.3.5";
    doCheck = false;
    propagatedBuildInputs = [
      self."enum34"
      self."pathlib"
      self."six"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/01/7b/c6bfb2515ed08cbfb76b0e72254f24caf76f25676d72024837a85a1e68f5/pyScss-1.3.5.tar.gz";
      sha256 = "1c6dh299lw4mkkp3qczry8yqslknydks19h0y2qnp9i1q8rmr8hl";
    };
  };
  "python-ldap" = super.buildPythonPackage {
    name = "python-ldap-3.2.0";
    doCheck = false;
    propagatedBuildInputs = [
      self."pyasn1"
      self."pyasn1-modules"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/ea/93/596f875e003c770447f4b99267820a0c769dd2dc3ae3ed19afe460fcbad0/python-ldap-3.2.0.tar.gz";
      sha256 = "13nvrhp85yr0jyxixcjj012iw8l9wynxxlykm9j3alss6waln73x";
    };
  };
  "python-magic" = super.buildPythonPackage {
    name = "python-magic-0.4.15";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/84/30/80932401906eaf787f2e9bd86dc458f1d2e75b064b4c187341f29516945c/python-magic-0.4.15.tar.gz";
      sha256 = "1mgwig9pnzgkf86q9ji9pnc99bngms15lfszq5rgqb9db07mqxpk";
    };
  };
  "robotframework" = super.buildPythonPackage {
    name = "robotframework-3.1.1";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/59/0f/233cc42cc21b95e6204a2e58964552c7242579c5890b531bc98a8834fed0/robotframework-3.1.1.zip";
      sha256 = "1gg7rsadvllgz7f32yw95h0xzly6b73w09cd6kgdnssy86nb4aby";
    };
  };
  "robotframework-selenium2library" = super.buildPythonPackage {
    name = "robotframework-selenium2library-3.0.0";
    doCheck = false;
    propagatedBuildInputs = [
      self."robotframework-seleniumlibrary"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/c4/7d/3c07081e7f0f1844aa21fd239a0139db4da5a8dc219d1e81cb004ba1f4e2/robotframework-selenium2library-3.0.0.tar.gz";
      sha256 = "19zxf2f5f6ply2ab4q9l52cn3d6j6j5h0f9h4pnnvcc80wmr93ia";
    };
  };
  "robotframework-seleniumlibrary" = super.buildPythonPackage {
    name = "robotframework-seleniumlibrary-3.3.1";
    doCheck = false;
    propagatedBuildInputs = [
      self."robotframework"
      self."selenium"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/c8/e7/c577d9528b80d748fe7beca1b106d751caf39d0d77f8fd85e39b1ef50ded/robotframework-seleniumlibrary-3.3.1.tar.gz";
      sha256 = "03x1fbc37mb9lkm3spn21hiyij8c8gs20517sc89nkqa7yx0wfkg";
    };
  };
  "selenium" = super.buildPythonPackage {
    name = "selenium-3.141.0";
    doCheck = false;
    propagatedBuildInputs = [
      self."urllib3"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/ed/9c/9030520bf6ff0b4c98988448a93c04fcbd5b13cd9520074d8ed53569ccfe/selenium-3.141.0.tar.gz";
      sha256 = "039hf9knvl4s3hp21bzwsp1g5ri9gxsh504dp48lc6nr1av35byy";
    };
  };
  "setuptools" = super.buildPythonPackage {
    name = "setuptools-41.0.1";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/1d/64/a18a487b4391a05b9c7f938b94a16d80305bf0369c6b0b9509e86165e1d3/setuptools-41.0.1.zip";
      sha256 = "04sns22y2hhsrwfy1mha2lgslvpjsjsz8xws7h2rh5a7ylkd28m2";
    };
  };
  "setuptools-git" = super.buildPythonPackage {
    name = "setuptools-git-1.2";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/d9/c5/396c2c06cc89d4ce2d8ccf1d7e6cf31b33d4466a7c65a67a992adb3c6f29/setuptools-git-1.2.tar.gz";
      sha256 = "0i84qjwp5m0l9qagdjww2frdh63r37km1c48mrvbmaqsl1ni6r7z";
    };
  };
  "setuptools-scm" = super.buildPythonPackage {
    name = "setuptools-scm-3.3.3";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/83/44/53cad68ce686585d12222e6769682c4bdb9686808d2739671f9175e2938b/setuptools_scm-3.3.3.tar.gz";
      sha256 = "19cyndx23xmpbhz4qrwmfwsmnnaczd0dw7qg977ksq2dbvxy29dx";
    };
  };
  "six" = super.buildPythonPackage {
    name = "six-1.12.0";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/dd/bf/4138e7bfb757de47d1f4b6994648ec67a51efe58fa907c1e11e350cddfca/six-1.12.0.tar.gz";
      sha256 = "0wxs1q74v07ssjywbbm7x6h5v9qx209ld2yfsif4060sxi0h2sni";
    };
  };
  "urllib3" = super.buildPythonPackage {
    name = "urllib3-1.22";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/ee/11/7c59620aceedcc1ef65e156cc5ce5a24ef87be4107c2b74458464e437a5d/urllib3-1.22.tar.gz";
      sha256 = "0kyvc9zdlxr5r96bng5rhm9a6sfqidrbvvkz64s76qs5267dli6c";
    };
  };
  "watchdog" = super.buildPythonPackage {
    name = "watchdog-0.9.0";
    doCheck = false;
    propagatedBuildInputs = [
      self."PyYAML"
      self."argh"
      self."pathtools"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/bb/e3/5a55d48a29300160779f0a0d2776d17c1b762a2039b36de528b093b87d5b/watchdog-0.9.0.tar.gz";
      sha256 = "07cnvvlpif7a6cg4rav39zq8fxa5pfqawchr46433pij0y6napwn";
    };
  };
  "wmctrl" = super.buildPythonPackage {
    name = "wmctrl-0.3";
    doCheck = false;
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/01/c6/001aefbde5782d6f359af0a8782990c3f4e751e29518fbd59dc8dfc58b18/wmctrl-0.3.tar.gz";
      sha256 = "04wacb7lj7rbpx5q5cnb19n9k4w85szdfa8xwfv6chsmq5dgc1nq";
    };
  };
  "zc.buildout" = super.buildPythonPackage {
    name = "zc.buildout-2.13.1";
    doCheck = false;
    propagatedBuildInputs = [
      self."setuptools"
    ];
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/3f/92/bf6ddecce944e3dcebfd8af4efef414a7131276b1433afb4f842012ed5ca/zc.buildout-2.13.1.tar.gz";
      sha256 = "110ln5d0zf1wnmqirimjclxjx7krs9wmibfzjmr52fln4rrd051x";
    };
  };
}
