{ lib
, stdenv
, fetchFromGitHub
, fetchpatch
, cmake
, qtbase
, qtsvg
, obs-studio
, git
, onnxruntime
, opencv4
, cudatoolkit
}:

stdenv.mkDerivation rec {
  pname = "obs-backgroundremoval";

  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "royshil";
    repo = "obs-backgroundremoval";
    rev = "0ba4a825328f3979fb10b7978367b87d75f8d9bf";
    sha256 = "1v7l3n1gldqym5ws6q6x5503inwl30aivqq5p878rin91izz5jm1";
    fetchSubmodules = true;
  };

  patches = [
  ];

  postPatch = ''
    substituteInPlace src/background-filter.cpp \
      --replace "#include <cuda_provider_factory.h>" ""
    git init
    git add .
    git config user.email "john.doe@example.com"
    git config user.name "John Doe"
    git commit -m "First commit"
    git tag "v0.4.0"
  '';

  nativeBuildInputs = [ cmake git ];
  buildInputs = [
    qtbase
    qtsvg
    obs-studio
    onnxruntime
    opencv4
    cudatoolkit
  ];
  propagatedBuildInputs = [
    onnxruntime
  ];

  dontWrapQtApps = true;

  CXXFLAGS = [
    "-I${onnxruntime}/include/onnxruntime/core/session"
    "-I${onnxruntime}/include/onnxruntime/"
    "-I${onnxruntime}/include/onnxruntime/core/providers/cpu"
    "-I${onnxruntime}/include/onnxruntime/core/providers/cuda"
  ];

  cmakeFlags = [
    "-DLIBOBS_INCLUDE_DIR=${obs-studio.src}/libobs"
    "-DWITH_CUDA=ON"
  ];

  meta = with lib; {
    description = "An OBS plugin for removing background in portrait images (video), making it easy to replace the background when screen recording.";
    homepage = "https://github.com/royshil/obs-backgroundremoval";
    maintainers = with maintainers; [];
    license = licenses.gpl2Plus;
    platforms = [ "x86_64-linux" "i686-linux" ];
  };
}
