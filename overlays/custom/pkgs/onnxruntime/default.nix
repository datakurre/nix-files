{ stdenv, fetchFromGitHub, glibcLocales
, cmake, python3, lib, libpng, zlib
, cudatoolkit, cudaPackages
}:

stdenv.mkDerivation rec {
  pname = "onnxruntime";
  version = "1.10.0";

  src = fetchFromGitHub {
    owner = "microsoft";
    repo = "onnxruntime";
    rev = "v${version}";
    sha256 = "181qvk50vwlx5n0nlg53x7f9vkimyshp8d2lvm0qrqpvqhw8k7jp";
    # TODO: use nix-versions of grpc, onnx, eigen, googletest, etc.
    # submodules increase src size and compile times significantly
    # not currently feasible due to how integrated cmake build is with git
    fetchSubmodules = true;
    # Remove unicode file names which leads to different checksums on HFS+
    # vs. other filesystems because of unicode normalisation.
    postFetch = ''
      rm -rf $out/winml/test/collateral/models/UnicodePath/
    '';
  };

  nativeBuildInputs = [
    cmake
    python3 # for shared-lib or server
  ];

  buildInputs = [
    # technically optional, but highly recommended
    libpng
    zlib
    cudatoolkit
    cudaPackages.cudnn
  ];

  cmakeDir = "../cmake";

  cmakeFlags = [
    "-Donnxruntime_USE_OPENMP=ON"
    "-Donnxruntime_BUILD_SHARED_LIB=ON"
    "-Donnxruntime_BUILD_UNIT_TESTS=OFF"
    "-Donnxruntime_ENABLE_LTO=OFF"
    "-Donnxruntime_USE_CUDA=ON"
    "-Donnxruntime_CUDNN_HOME=${cudaPackages.cudnn}"
    "-DCMAKE_CUDA_ARCHITECTURES=61;62"
  ];

  # ContribOpTest.StringNormalizerTest sets locale to en_US.UTF-8"
  preCheck = lib.optionalString stdenv.isLinux ''
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
  '';
  doCheck = true;

  postInstall = ''
    rm -fr $out/bin   # ctest runner
    mkdir -p $out/include/onnxruntime/core/providers/cuda
    cp $src/onnxruntime/core/providers/cuda/cuda_provider_factory.h \
       $out/include/onnxruntime/core/providers/cuda
  '';

  enableParallelBuilding = true;

  meta = with lib; {
    description = "Cross-platform, high performance scoring engine for ML models";
    longDescription = ''
      ONNX Runtime is a performance-focused complete scoring engine
      for Open Neural Network Exchange (ONNX) models, with an open
      extensible architecture to continually address the latest developments
      in AI and Deep Learning. ONNX Runtime stays up to date with the ONNX
      standard with complete implementation of all ONNX operators, and
      supports all ONNX releases (1.2+) with both future and backwards
      compatibility.
    '';
    homepage = "https://github.com/microsoft/onnxruntime";
    changelog = "https://github.com/microsoft/onnxruntime/releases";
    # https://github.com/microsoft/onnxruntime/blob/master/BUILD.md#architectures
    platforms = platforms.unix;
    license = licenses.mit;
    maintainers = with maintainers; [];
  };

}
