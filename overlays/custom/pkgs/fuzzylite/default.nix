{ stdenv, cmake }:

stdenv.mkDerivation {
  name = "fuzzylite-6.0";
  src = builtins.fetchTarball {
    url = "https://github.com/fuzzylite/fuzzylite/archive/v6.0.tar.gz";
    sha256 = "0yay0qc81x0irlvxqpy7jywjxpkmpjabdhq2hdh28r9z85wp2nwb";
  };
  sourceRoot = "source/fuzzylite";
  buildInputs = [ cmake ];
}
