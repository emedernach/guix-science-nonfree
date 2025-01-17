;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix-science-nonfree packages machine-learning)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages tls)
  #:use-module (guix-science-nonfree packages cuda))

(define googletest/gcc8
  (package
    (inherit googletest)
    (native-inputs
     (modify-inputs (package-native-inputs googletest)
       (append gcc-8)))))

(define googlebenchmark/gcc8
  (package
    (inherit googlebenchmark)
    (native-inputs
     (modify-inputs (package-native-inputs googlebenchmark)
       (append gcc-8)))))

(define onnx/gcc8
  (package
    (inherit onnx)
    (name "onnx-gcc8")
    (native-inputs
     (modify-inputs (package-native-inputs onnx)
       (append gcc-8)
       (replace "googletest" googletest/gcc8)))))

(define onnx-optimizer/gcc8
  (package
    (inherit onnx-optimizer)
    (name "onnx-optimizer-gcc8")
    (inputs
     (modify-inputs (package-inputs onnx-optimizer)
       (replace "onnx" onnx/gcc8)))
    (native-inputs
     (modify-inputs (package-native-inputs onnx-optimizer)
       (append gcc-8)))))

(define-public gloo-cuda10
  (package
    (inherit gloo)
    (name "gloo-cuda10")
    (arguments
     (list
      #:tests? #false                   ;see linker error below
      #:configure-flags
      #~'("-DBUILD_SHARED_LIBS=ON"
          ;; We cannot build the tests because of a linker error with googletest:
          ;; /lib/libgtest.so.1.11.0: undefined reference to
          ;; `std::__cxx11::basic_ostringstream<char, std::char_traits<char>,
          ;; std::allocator<char> >::basic_ostringstream()@GLIBCXX_3.4.26'
          "-DBUILD_TEST=OFF"
          "-DUSE_CUDA=ON"
          #$@(if (this-package-input "rdma-core")
                 #~("-DUSE_IBVERBS=ON")
                 #~()))))
    (inputs
     (modify-inputs (package-inputs gloo)
       (append cuda-10.2)))
    ;; When building with CUDA 10 we cannot use any more recent GCC
    ;; than version 8.
    (native-inputs
     (modify-inputs (package-native-inputs gloo)
       (append gcc-8)))))

(define-public gloo-cuda11
  (let ((version "0.0.0")
        (commit "a01540ec3dabd085ad2579aa2b7a004406e2793b")
        (revision "20230315"))
    (package
      (inherit gloo-cuda10)
      (name "gloo-cuda11")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/facebookincubator/gloo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rqcq46lmhr1xjz3bbr5mfmhyvff6qhnp88q2af5vfc9rrljvklj"))))
      (arguments
       (substitute-keyword-arguments (package-arguments gloo-cuda10)
         ((#:configure-flags flags '())
          #~'("-DBUILD_SHARED_LIBS=ON"
              "-DBUILD_TEST=OFF"
              "-DUSE_CUDA=ON"))
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (add-after 'unpack 'drop-unsupported-arch
               (lambda _
                 (substitute* "cmake/Cuda.cmake"
                   (("gloo_known_gpu_archs \"[^\"]+\"")
                    "gloo_known_gpu_archs \"35 50 52 60 61 70 75\""))))))))
      (inputs
       (list cuda-11.7 openssl))
      (native-inputs
       (package-native-inputs gloo)))))

(define-public python-pytorch-with-cuda10
  (package
    (inherit python-pytorch)
    (name "python-pytorch-with-cuda10")
    (source
     (origin
       (inherit (package-source python-pytorch))
       (snippet
        '(begin
           ;; We're using a separately built gloo, so this
           ;; target does not exist.
           (substitute* "cmake/Dependencies.cmake"
             (("add_dependencies\\(gloo_cuda nccl_external\\)") ""))
           ;; XXX: Let's be clear: this package is a bundling fest.  We
           ;; delete as much as we can, but there's still a lot left.
           (for-each (lambda (directory)
                       (delete-file-recursively
                        (string-append "third_party/" directory)))
                     '("benchmark" "cpuinfo" "eigen"

                       ;; FIXME: QNNPACK (of which XNNPACK is a fork)
                       ;; needs these.
                       ;; "FP16" "FXdiv" "gemmlowp" "psimd"

                       "gloo" "googletest"
                       "ios-cmake" "NNPACK"
                       "onnx" "protobuf" "pthreadpool"
                       "pybind11" "python-enum" "python-peachpy"
                       "python-six" "tbb" "XNNPACK" "zstd"))))))
    (name "python-pytorch-with-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytorch)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           ;; XXX: libcuda.so.1 is not on the RUNPATH.
           (delete 'validate-runpath)
           (add-after 'unpack 'do-not-build-tests
             (lambda _
               (setenv "INSTALL_TEST" "OFF")
               (setenv "BUILD_TESTS" "OFF")
               (setenv "BUILD_TEST" "OFF")))))))
    (inputs
     (modify-inputs (package-inputs python-pytorch)
       (append cuda)
       (delete "googletest")
       (delete "googlebenchmark")
       (replace "gloo" gloo-cuda10)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pytorch)
       (replace "onnx" onnx/gcc8)
       (replace "onnx-optimizer" onnx-optimizer/gcc8)))
    ;; When building with CUDA 10 we cannot use any more recent GCC
    ;; than version 8.
    (native-inputs
     (modify-inputs (package-native-inputs python-pytorch)
       (append gcc-8)))))

(define-public python-pytorch-with-cuda11
  (package
    (inherit python-pytorch-with-cuda10)
    (name "python-pytorch-with-cuda11")
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytorch-with-cuda10)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           ;; XXX: Building with the bundled NCCL <https://github.com/nvidia/nccl>
           ;; fails with "undefined reference" errors.
           (add-after 'unpack 'disable-nccl
             (lambda _
               (substitute* "CMakeLists.txt"
                 (("USE_NCCL \"Use NCCL\" ON")
                  "USE_NCCL \"Use NCCL\" OFF"))))))))
    (inputs
     (modify-inputs (package-inputs python-pytorch-with-cuda10)
       (replace "cuda-toolkit" cuda-11.7)
       (replace "gloo" gloo-cuda11)))
    (propagated-inputs
     (package-propagated-inputs python-pytorch))
    (native-inputs
     (package-native-inputs python-pytorch))))
