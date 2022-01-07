{stdenv, fetchurl, aspell, which}:

let

  /* Function to compile an Aspell dictionary.  Fortunately, they all
     build in the exact same way. */
  buildDict =
    {shortName, fullName, src, postInstall ? ""}:

    stdenv.mkDerivation {
      name = "aspell-dict-${shortName}";

      inherit src;

      buildInputs = [aspell which];

      dontAddPrefix = true;

      preBuild = "makeFlagsArray=(dictdir=$out/lib/aspell datadir=$out/lib/aspell)";

      inherit postInstall;

      meta = {
        description = "Aspell dictionary for ${fullName}";
        platforms = lib.platforms.all;
      };
    };

in {

  fi = buildDict {
    shortName = "fi-0.7.0";
    fullName = "Catalan";
    src = fetchurl {
      url = "https://ftp.gnu.org/gnu/aspell/dict/fi/aspell6-fi-0.7-0.tar.bz2";
      sha256 = "f8d7f07b4511e606eb56392ddaa76fd29918006331795e5942ad11b510d0a51d";
    };
  };

}
