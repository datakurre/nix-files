{ stdenv, fetchurl, makeWrapper
, jre, ant, jdk, opencv3, jython
, xdotool, wmctrl, python2, python3, unzip }:

let

  python = python3.withPackages(ps: with ps; [ numpy ]);
  buildPython = python2.withPackages(ps: with ps; [ numpy ]);
  opencv = opencv3.overrideAttrs(old: {
    buildInputs = old.buildInputs ++ [
      ant
      jdk
      python
      buildPython
    ];
    PYTHON_EXECUTABLE = "${buildPython}/bin/python";
    JAVA_HOME = "${jdk}/lib/openjdk";
    cmakeFlags = old.cmakeFlags ++ [
      "-DBUILD_SHARED_LIBS=OFF"
      "-DBUILD_FAT_JAVA_LIB=ON"
      "-DBUILD_opencv_java=ON"
    ];
  });

in

stdenv.mkDerivation rec {
  name = "sikulix-${version}";
  version = "2.0.4";

  src = fetchurl {
    url = "https://launchpad.net/sikuli/sikulix/${version}/+download/sikulixide-${version}.jar";
    sha256 = "0pq0d58h4svkgnw3h5qv27bzmap2cgx0pkhmq824nq4rdj9ivphi";
  };

  buildInputs = [ makeWrapper jdk opencv unzip ];
  propagatedBuildInputs = [ jython ];

  unpackPhase = "true";
  buildPhase = "";

  installPhase = ''
    mkdir -p $out/bin $out/share
    cp $src $out/share/sikulixide-${version}.jar
    cat > $out/bin/.sikulix-wrapped << EOF
#!/usr/bin/env sh
java -Djava.library.path=${opencv}/share/OpenCV/Java/ -jar $out/share/sikulixide-${version}.jar $@
EOF
    chmod u+x $out/bin/.sikulix-wrapped
    makeWrapper $out/bin/.sikulix-wrapped $out/bin/sikulix \
      --prefix PATH : "${jre}/bin" \
      --prefix PATH : "${xdotool}/bin" \
      --prefix PATH : "${wmctrl}/bin" \
      --prefix PATH : "${jre}/bin:${xdotool}/bin:${wmctrl}/bin" \
      --prefix CLASSPATH : "${jython}/jython.jar" \
      --prefix CLASSPATH : "${opencv}/share/OpenCV/java/opencv-348.jar"
  '';

  meta = with stdenv.lib; {
    description = "Sikuli automates anything you see on the screen.";
    homepage = http://www.sikulix.com/;
    license = with licenses; [ mit ];
    maintainers = with maintainers; [];
    platforms = with platforms; linux;
  };
}
