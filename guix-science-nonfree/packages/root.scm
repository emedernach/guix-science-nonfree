;;;
;;; Copyright © 2023, 2024 Emmanuel Medernach <Emmanuel.Medernach@iphc.cnrs.fr>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define-module (guix-science-nonfree packages root)
  
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages algebra)	;; fftw
  #:use-module (gnu packages astronomy) ;; cfitsio
  #:use-module (gnu packages base) ;; coreutils gnu-make
  #:use-module (gnu packages commencement) ;; gcc-toolchain
  #:use-module (gnu packages compression)  ;; zlib lz4 xz
  #:use-module (gnu packages crypto)    ;; libxcrypt
  #:use-module (gnu packages digest)    ;; xxhash
  #:use-module (gnu packages fontutils) ;; freetype
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)    ;; glu
  #:use-module (gnu packages image) ;; libjpeg
  #:use-module (gnu packages less)
  #:use-module (gnu packages llvm)  ;; llvm clang
  #:use-module (gnu packages maths) ;; openblas gsl
  #:use-module (gnu packages pcre) 
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz) ;; numpy 
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tls) ;; openssl
  #:use-module (gnu packages version-control) ;; git
  #:use-module (gnu packages xml)  ;; libxml2
  #:use-module (gnu packages xorg) ;; libx11 libpthread-stubs
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix-science-nonfree packages physics)
  
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  
  )


(define-public ROOT-6.32.06
  (package
   (name "ROOT")
   (version "6.32.06")
   (source (origin
	    (method url-fetch)
	    (uri (string-append
		  "https://root.cern/download/root_v"
		  version ".source.tar.gz"))
	    (sha256
	     (base32
              "0ivjmgzs2lndnfi0mviz54ira9w61a7pvd5imnjxwj787zck5h1z"))
	    (file-name (string-append name "-" version ".tar.gz"))
	    (modules '((guix build utils)))))
   
   (build-system cmake-build-system)
   (propagated-inputs
    `(
      ;; https://issues.guix.gnu.org/41038
      ;; ("binutils" ,binutils)
      ;; ("libc" ,glibc)
      ;; ("libc-debug" ,glibc "debug")
      ;; ("libc-static" ,glibc "static")

      ))
   (inputs
    `(

      ;; Package to build to avoid ROOT downloading them
      
      ("dcap" ,dcap-2.47.14)
      ("davix" ,davix-0.8.7)
      ("vdt" ,vdt-0.4.4)

      ;; Dependencies
      
      ("gcc-lib" ,gcc "lib")
      ("libAfterImage" ,libAfterImage-1.20)
      ("libcxx" ,libcxx)
      ("libjpeg-turbo" ,libjpeg-turbo) 
      ("liblzma" ,xz)
      ("libpthread-stubs" ,libpthread-stubs)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxft" ,libxft)
      ("libxml2" ,libxml2)
      ("libxpm" ,libxpm)
      ("pcre" ,pcre)
      ("zlib" ,zlib)

      ("libxcrypt" ,libxcrypt) ;; For crypt.h

      ("cfitsio" ,cfitsio)
      ("coreutils" ,coreutils)
      ("fftw" ,fftw)
      ("freetype" ,freetype)
      ("gcc-toolchain" ,gcc-toolchain)
      ("git" ,git)
      ("glu" ,glu)
      ("gsl" ,gsl)
      ("less" ,less)
      ("clang" ,clang-16) 
      ("llvm-16" ,llvm-16) 
      ("lz4" ,lz4)
      ("mesa" ,mesa)
      ("openblas" ,openblas)
      ;; ("openssl" ,openssl)
      ("openssl" ,openssl-1.1)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python@3" ,python-3)
      ("python@2.7" ,python-2.7)
      ("python-numpy" ,python-numpy)
      ("tbb" ,tbb)
      ("xxhash" ,xxhash)
      ("zstd" ,zstd)
      ;; ("libjpeg" ,libjpeg) ;; Deprecated
      ))

   (arguments 
    `(#:configure-flags 
      
      (list

       ;; To avoid error "fatal error: module file not found"
       ;; "-DCMAKE_INSTALL_PREFIX=/opt/root/"  ;; error

       (let ((out (assoc-ref %outputs "out")))
	 (string-append "-DCMAKE_INSTALL_PREFIX=" out))

       ;; From https://root.cern.ch/building-root
       "-Dgnuinstall=ON"
       "-Drpath=ON"
       "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
       
       ;; To avoid downloading clad, llvm, davix
       "-Dclad=OFF"
       ;; "-Dbuiltin_clang=OFF" ;; Too many cling errors
       "-Dbuiltin_llvm=OFF"
       "-Dbuiltin_davix=OFF"
       
       "-DCMAKE_INSTALL_LIBDIR=lib"
       (string-append "-DCLANG_LIBRARY_DIR="
		      (assoc-ref %build-inputs "clang")
		      "/lib")
       (string-append "-DCMAKE_C_FLAGS="
		      (let ((inputs (alist-delete "source" %build-inputs)))
			(apply string-append
			       (map (lambda (p)
				      (string-append "-L" (cdr p) "/lib "))
				    inputs))))
       "-Dastiff=ON"
       "-Dbuiltin_afterimage=OFF"
       "-Dbuiltin_ftgl=OFF"
       "-Dbuiltin_glew=OFF"
       "-Dbuiltin_gsl=OFF"
       "-Dbuiltin_zlib=OFF"
       "-Dcfitsio=ON"
       "-Ddavix=ON"
       "-Dhttp=ON"
       "-Djemmaloc=ON"
       "-Dmathmore=ON"
       "-Dopengl=ON"
       ;; "-Dminuit2=ON" ;; Option 'minuit2' is no longer supported in ROOT 6.32.06
       ;; "-Dpythia6=ON" ;; Option 'pythia6' is no longer supported in ROOT 6.32.06
       ;; "-Dpythia6_nolink=ON"
       "-Dpythia8=OFF"
       "-Droofit=ON"
       "-Drpath=ON"
       "-Dshadowpw=OFF"
       "-Dsoversion=ON"
       "-Dtmva=OFF"
       "-Dvdt=ON"
       "-Dx11=ON"
       
       (string-append "-DOPENGL_INCLUDE_DIR="
		      (assoc-ref %build-inputs "mesa")
		      "/include")
       (string-append "-DOPENGL_gl_LIBRARY="
		      (assoc-ref %build-inputs "mesa")
		      "/lib/libGL.so")
       (string-append "-DOPENGL_glu_LIBRARY="
		      (assoc-ref %build-inputs "glu")
		      "/lib/libGLU.so")

       ;; From llvm.scm
       (string-append "-DC_INCLUDE_DIRS="
		      (assoc-ref %build-inputs "libc")
		      "/include")
       (string-append "-DCMAKE_LIBRARY_PATH="
		      (let ((inputs (alist-delete "source" %build-inputs)))
			(apply string-append
			       (map (lambda (p)
				      (string-append (cdr p) "/lib:"))
				    inputs)))))
		     
      ;; To avoid "depends on .. which cannot be found in RUNPATH"
      #:validate-runpath? #f

      ;; #:tests? #f
      
      #:phases
      (modify-phases
       %standard-phases
       (add-before
	;; avec build LD_LIBRARY_PATH est vide pour G__Core.cxx
	'configure 'fix-library-path
	;; Sinon error while loading shared libraries: libLLVMTableGen.so.5
	(lambda*
	 (#:key inputs outputs #:allow-other-keys)
         
	 (define (add-libraries libpath inputs)
	    (let ((inputs (alist-delete "source" inputs)))
	     (apply string-append
		    libpath
		    (map (lambda (p)
			   (string-append (cdr p) "/lib:"))
			 inputs))))

	 ;; Set LD_LIBRARY_PATH to find shared libraries
	 ;; during compilation
	 (let* ((libpath (getenv "LD_LIBRARY_PATH"))
		 (libpath (if libpath (string-append libpath ":") ""))
		 (libpath (add-libraries libpath inputs)))
           
	    (display (list "LD_LIBRARY_PATH" libpath)) (newline)
	    (setenv "LD_LIBRARY_PATH" libpath))

	 #t )))
      
      ))

   ;; From llvm.scm
   (native-search-paths
    (list (search-path-specification
	   (variable "CPATH")
	   (files '("include")))
	  (search-path-specification
	   (variable "LIBRARY_PATH")
	   (files '("lib" "lib64")))))
   
   (home-page "https://root.cern.ch/")
   (synopsis "ROOT: Data Analysis Framework")
   (description
    "A modular scientific software toolkit.  It provides all the
functionalities needed to deal with big data processing, statistical
analysis, visualisation and storage.  It is mainly written in C++ but
integrated with other languages such as Python and R.")
   (license license:lgpl2.1+)))
