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


(define-module (guix-science-nonfree packages physics)
  #:use-module  (guix-science-nonfree packages geant4)
  #:use-module  (guix-science-nonfree packages root)
  #:use-module  ((guix licenses) #:prefix license:)
  #:use-module  (gnu packages adns)  ;; c-ares
  #:use-module  (gnu packages algebra)	 ;; FFTW
  #:use-module  (gnu packages astronomy) ;; cfitsio
  #:use-module  (gnu packages autotools)
  #:use-module  (gnu packages backup)
  #:use-module  (gnu packages base) ;; gnu-make
  #:use-module  (gnu packages bash)
  #:use-module  (gnu packages boost)
  #:use-module  (gnu packages bootstrap)
  #:use-module  (gnu packages check) 
  #:use-module  (gnu packages commencement) ;; gcc-toolchain
  #:use-module  (gnu packages compression)  ;; zlib lz4
  #:use-module  (gnu packages curl)
  #:use-module  (gnu packages crypto)
  #:use-module  (gnu packages databases)
  #:use-module  (gnu packages digest) ;; xxhash
  #:use-module  (gnu packages documentation)
  #:use-module  (gnu packages file) 
  #:use-module  (gnu packages fontutils) ;; fontconfig
  #:use-module  (gnu packages fontutils) ;; freetype
  #:use-module  (gnu packages gcc)
  #:use-module  (gnu packages geo)
  #:use-module  (gnu packages gl)
  #:use-module  (gnu packages glib)
  #:use-module  (gnu packages hunspell)
  #:use-module  (gnu packages icu4c) 
  #:use-module  (gnu packages image) ;; libjpeg
  #:use-module  (gnu packages less)
  #:use-module  (gnu packages libevent) ;; libuv
  #:use-module  (gnu packages libreoffice) ;; hunspell
  #:use-module  (gnu packages linux)
  #:use-module  (gnu packages llvm)  ;; llvm clang
  #:use-module  (gnu packages maths) ;; openblas gsl
  #:use-module  (gnu packages monitoring)
  #:use-module  (gnu packages openstack)
  #:use-module  (gnu packages pcre)
  #:use-module  (gnu packages pdf) ;; poppler-qt5
  #:use-module  (gnu packages perl)
  #:use-module  (gnu packages pkg-config)
  #:use-module  (gnu packages python)
  #:use-module  (gnu packages python-build)
  #:use-module  (gnu packages python-crypto) ;; python-cryptography
  #:use-module  (gnu packages python-web) ;; python-oauthlib
  #:use-module  (gnu packages python-xyz) ;; numpy 
  #:use-module  (gnu packages qt)
  #:use-module  (gnu packages serialization)
  #:use-module  (gnu packages shells)
  #:use-module  (gnu packages shells)
  #:use-module  (gnu packages tbb)
  #:use-module  (gnu packages time)
  #:use-module  (gnu packages tls) ;; openssl
  #:use-module  (gnu packages version-control) ;; git
  #:use-module  (gnu packages web)   ;; http-parser
  #:use-module  (gnu packages xml)
  #:use-module  (gnu packages xorg) ;; libx11
  #:use-module  (gnu packages)
  #:use-module  (guix build-system cmake)
  #:use-module  (guix build-system gnu)
  #:use-module  (guix build-system python)  
  #:use-module  (guix build-system trivial)
  #:use-module  (guix download)
  #:use-module  (guix gexp)
  #:use-module  (guix git-download)
  #:use-module  (guix packages)
  #:use-module  (guix utils)

  #:use-module  (ice-9 match)
  #:use-module  (ice-9 regex)

  )


(define-public dcap-2.47.14 
  (package
   (name "dcap-2.47.14")
   (version "2.47.14")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/dCache/dcap/archive/"
			 version ".tar.gz"))
     (sha256
      (base32
       "0jfcx60f749zbm6ksszpzww89x6j99kh2la2xqaxipiwv688kafx"))))
   (build-system gnu-build-system)
   (inputs
    `(("bash" ,bash)
      ("gcc-toolchain" ,gcc-toolchain)
      ("python" ,python-2.7)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("cunit" ,cunit)
      ("zlib" ,zlib)
      ("zsh" ,zsh)
      ("libtool" ,libtool)
      ("libxcrypt" ,libxcrypt)
      ("pkg-config" ,pkg-config)))
   
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (add-before 'configure 'bootstrap
		   (lambda args
		     (invoke "sh" "./bootstrap.sh"))))))
   
   (home-page "https://www.dcache.org/downloads/dcap/")
   (synopsis "DCAP")
   (description "dCache access protocol client library ")
   (license license:lgpl2.1+)))


(define-public davix-0.8.7
  (package
   (name "davix-0.8.7")
   (version "0.8.7")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/cern-fts/davix")
           (commit (string-append "R_" (string-replace-substring version "." "_")))
           (recursive? #t))) ;; submodules
     (sha256
      (base32
       "1w0alvkhc23iy42kqrb543jja4v6xs7z600vhn890hprj3fmpis2"))))
   (build-system cmake-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain-8)
      ("python" ,python-2.7)
      ("libxml2" ,libxml2)
      ("openssl" ,openssl-1.1)
      ("libuuid" ,util-linux+udev)
      ("boost" ,boost)
      ("util-linux" ,util-linux "lib") ;; For <uuid/uuid.h>
      ("curl" ,curl)
      ;; ("googletest" ,googletest)
      ))
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (add-before
	'configure
      	'patch
      	(lambda args
          '(substitute*
	    "src/fileops/AzureIO.cpp"
	    (("#include <uuid/uuid.h>") "#include <linux/uuid.h>")
	    (("uuid_t uuid") "guid_t uuid")
	    (("uuid\\[i\\]") "uuid.b[i]") 
      	    ))))

      #:configure-flags
      (list
       "-DEMBEDDED_LIBCURL=FALSE"
       "-DLIBCURL_BACKEND_BY_DEFAULT=TRUE"
       )))
      
   (home-page "https://davix.web.cern.ch/")
   (synopsis "Davix")
   (description "The davix project aims to make file management over HTTP-based protocols simple.")
   (license license:lgpl2.1+)))


(define-public vdt-0.4.4
  (package
   (name "vdt-0.4.4")
   (version "0.4.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "http://lcgpackages.web.cern.ch/lcgpackages/tarFiles/sources/"
	   name "-" version ".tar.gz"))
     (sha256
      (base32 "1qdc10p4j6jl0as3a8pfvrygxdry2x6izxm8clmihp5v5rhp8mkh"))))
   (build-system cmake-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("python" ,python-2.7)))
   (arguments 
    `(#:tests? #f)) ; no tests
   (home-page "https://root.cern/root/html606/md_math_vdt_ReadMe.html")
   (synopsis "VDT")
   (description "VDT is a library of mathematical functions, implemented in double and single precision. The implementation is fast and vectorisable with the aid of modern compilers. VDT exploits also Pade polynomials. A lot of ideas were inspired by the cephes math library (by Stephen L. Moshier)")
   (license license:lgpl2.1+)))


(define-public libAfterImage-1.20
  (package
   (name "libAfterImage-1.20")
   (version "1.20")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "ftp://ftp.afterstep.org/stable/libAfterImage/"
	   name "-" version ".tar.gz"))
     (sha256
      (base32 "125y119fbr3g389nr7yls4i7x5zd5pz7h8qn12k8b21b4xk1h6y5"))
     (modules '((guix build utils)))
     (snippet
      '(begin
         (substitute*
	  "Makefile.in"
	  (("ar clq") "ar cq")
	  )))))
   (build-system gnu-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("zlib" ,zlib)))
   (arguments 
    `(#:tests? #f)) ; no tests in Makefile
   (home-page "http://www.afterstep.org/afterimage/")
   (synopsis "LibAfterImage")
   (description "libAfterImage is a generic image manipulation library. It
was initially implemented to address AfterStep Window
Manager's needs for image handling, but it evolved into
extremely powerful and flexible software, suitable for
virtually any project that has needs for loading,
manipulating, displaying images, as well as writing images
in files. Most of the popular image formats are supported
using standard libraries, with XCF, XPM, PPM/PNM, BMP, ICO,
TGA and GIF being supported internally.")
   (license license:lgpl2.1+)))


(define-public TALYS-1.96
  (package
   (name "TALYS") ;; https://nds.iaea.org/talys/codes/talys.tar
   (version "1.96")
   (source
    (origin
     (method url-fetch)
     (uri "https://tendl.web.psi.ch/tendl_2019/talys/talys.tar")
     (sha256
      (base32 "1g8mj7jj8ld5mzj8k79hbypfhhzksx5k0b8fqmavlrlvx424v9zz"))))
       
   (build-system gnu-build-system)
   (inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("sed" ,sed)
      ("tar" ,tar)
      ("gzip" ,gzip)
      ("gfortran-toolchain" ,gfortran-toolchain)
      ))
   (arguments
    `(#:tests? #f

      #:phases
      (modify-phases
       %standard-phases
       
       (add-before
        'configure
        'run-script
        (lambda*
	 (#:key inputs #:allow-other-keys)
         (define (string-fortranize str)

           (define (string-cut str maxchar)
             (let ((len (string-length str)))
               (let loop ((index 0)
                          (result '()))
                 (if (>= index len)
                     (reverse result)
                     (let* ((stop (min len (+ index maxchar)))
                            (sub (substring str index stop)))
                       (loop (+ index maxchar)
                             (cons sub result)))))))

           (let ((fortran-max-length 72)
                 (len (string-length str)))
             (if (<= len fortran-max-length) str
                 (let* ((first  (substring str 0 fortran-max-length))
                        (rest   (substring str fortran-max-length len))
                        ;; The first 6 characters must be " " followed
                        ;; by a continuation character (here '&')
                        (mylist (string-cut rest (- fortran-max-length 6))))
                   (apply
                    string-append
                    (cons (string-append first (string #\newline))
                          ;; FORTRAN 77:
                          ;; https://gcc.gnu.org/onlinedocs/gcc-3.4.6/g77/Continuation-Line.html 
                          ;; A continuation line is any line that both
                          ;; - Contains a continuation character, and
                          ;; - Contains only spaces in columns 1 through 5 
                          (map (lambda (str)
                                 (string-append "     &" str (string #\newline)))
                               mylist)))))))
         
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (BASH (string-append (assoc-ref %build-inputs "bash") "/bin/bash"))
                (TAR  (string-append (assoc-ref %build-inputs "tar") "/bin/tar")))
       
           ;; (set-path)

           (substitute*
            "talys.setup"
            (("mv -f talys.*$") (string-append "pwd; ls -lh talys" (string #\newline)))
            (("\\$\\{compiler\\} -c \\*\\.f")
              "${compiler} -Wall -c *.f"))
           
           (substitute*
            "source/machine.f"
            (("      path=trim.*$") "c")
            ;; Replace source directory with output directory
            ;; Beware of FORTRAN char line limit
            ;; We must have 6 spaces before "home"
            (("      home=.*$") (string-fortranize (string-append "      path=\"" out "/talys/structure/\""))))

           (let ((current-dir (getcwd)))
             (invoke BASH "talys.setup")
             (chdir current-dir))
           
           (mkdir-p (string-append out "/bin"))
           (copy-file "source/talys" (string-append out "/bin/talys"))
           
           (mkdir-p (string-append out "/talys"))
           (let ((talys (string-append out "/talys/")))
             (for-each
              (lambda (dir)
                (copy-recursively
                 dir
                 (string-append talys dir)))
              '("doc" "samples" "structure"
                "source"
                )))

           (copy-file "README" (string-append out "/README"))

           )))
       
       (delete 'bootstap)
       (delete 'configure)
       (delete 'build)
       (delete 'install)
       
       )))
	  

   (home-page "https://tendl.web.psi.ch/tendl_2021/talys.html")
   (synopsis "TALYS")
   (description "TALYS is an open source software package (GPL license)
for the simulation of nuclear reactions.")
   (license license:gpl3+)))


(define-public TALYS-2.0
  (package
   (name "TALYS") ;; https://nds.iaea.org/talys/codes/talys.tar
   (version "2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://nds.iaea.org/talys/codes/talys.tar")
     (sha256
      (base32 "1g8mj7jj8ld5mzj8k79hbypfhhzksx5k0b8fqmavlrlvx424v9zz"))))
       
   (build-system gnu-build-system)
   (inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("sed" ,sed)
      ("tar" ,tar)
      ("gzip" ,gzip)
      ("gfortran-toolchain" ,gfortran-toolchain)
      ))
   (arguments
    `(#:tests? #f

      #:phases
      (modify-phases
       %standard-phases
       
       (add-before
        'configure
        'run-script
        (lambda*
	 (#:key inputs #:allow-other-keys)
         (define (string-fortranize str)

           (define (string-cut str maxchar)
             (let ((len (string-length str)))
               (let loop ((index 0)
                          (result '()))
                 (if (>= index len)
                     (reverse result)
                     (let* ((stop (min len (+ index maxchar)))
                            (sub (substring str index stop)))
                       (loop (+ index maxchar)
                             (cons sub result)))))))

           (let ((fortran-max-length 72)
                 (len (string-length str)))
             (if (<= len fortran-max-length) str
                 (let* ((first  (substring str 0 fortran-max-length))
                        (rest   (substring str fortran-max-length len))
                        ;; The first 6 characters must be " " followed
                        ;; by a continuation character (here '&')
                        (mylist (string-cut rest (- fortran-max-length 6))))
                   (apply
                    string-append
                    (cons (string-append first (string #\newline))
                          ;; FORTRAN 77:
                          ;; https://gcc.gnu.org/onlinedocs/gcc-3.4.6/g77/Continuation-Line.html 
                          ;; A continuation line is any line that both
                          ;; - Contains a continuation character, and
                          ;; - Contains only spaces in columns 1 through 5 
                          (map (lambda (str)
                                 (string-append "     &" str (string #\newline)))
                               mylist)))))))
         
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (BASH (string-append (assoc-ref %build-inputs "bash") "/bin/bash"))
                (TAR  (string-append (assoc-ref %build-inputs "tar") "/bin/tar")))
       
           ;; (set-path)

           (substitute*
            "talys.setup"
            (("mv -f talys.*$") (string-append "pwd; ls -lh talys" (string #\newline)))
            (("\\$\\{compiler\\} -c \\*\\.f")
              "${compiler} -Wall -c *.f"))
           
           (substitute*
            "source/machine.f"
            (("      path=trim.*$") "c")
            ;; Replace source directory with output directory
            ;; Beware of FORTRAN char line limit
            ;; We must have 6 spaces before "home"
            (("      home=.*$") (string-fortranize (string-append "      path=\"" out "/talys/structure/\""))))

           (let ((current-dir (getcwd)))
             (invoke BASH "talys.setup")
             (chdir current-dir))
           
           (mkdir-p (string-append out "/bin"))
           (copy-file "source/talys" (string-append out "/bin/talys"))
           
           (mkdir-p (string-append out "/talys"))
           (let ((talys (string-append out "/talys/")))
             (for-each
              (lambda (dir)
                (copy-recursively
                 dir
                 (string-append talys dir)))
              '("doc" "samples" "structure"
                "source"
                )))

           (copy-file "README" (string-append out "/README"))

           )))
       
       (delete 'bootstap)
       (delete 'configure)
       (delete 'build)
       (delete 'install)
       
       )))
	  

   (home-page "https://nds.iaea.org/talys/")
   (synopsis "TALYS")
   (description "TALYS is an open source software package (GPL license)
for the simulation of nuclear reactions.")
   (license license:gpl3+)))


(define-public texworks-0.6.7
  (package
   (name "texworks")
   (version "0.6.7")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/TeXworks/texworks/archive/release-0.6.7.zip")
     (sha256
      (base32 "0vwxq3i120ngr9z2hbdgrav91x64k94kss7wyxxpr48bv209pwlm"))))
  (build-system cmake-build-system)
  (inputs
   `(("unzip" ,unzip)
     ("qt5" ,qtbase-5)
     ("qttools" ,qttools-5)
     ("qtdeclarative-5" ,qtdeclarative-5)
     ("qtscript" ,qtscript)
     ("zlib" ,zlib)
     ("pkg-config" ,pkg-config)
     ("hunspell" ,hunspell)
     ("poppler-qt5" ,poppler-qt5)
     ("fontconfig" ,fontconfig)
     ))
  (arguments `(#:tests? #f))
  (home-page "https://www.tug.org/texworks/")
  (synopsis "TeXworks")
  (description "The TeXworks project is an effort to build a simple TeX front-end program (working environment) that will be available for all today's major desktop operating systems")
  (license license:gpl2+)))


(define-public timing-gen-0.9.8
  (package
   (name "timing-gen-0.9.8")
   (version "0.9.8")
   (source
    (origin
     (method url-fetch)
     (uri "https://sourceforge.net/projects/timing-gen/files/timing-gen-0.9.8.tgz")
     (sha256
      (base32 "1k9x9rcbqg9xax5z8w1fglycf5z8wiaym5kp3naia6m9ny959i3z"))))
   (build-system trivial-build-system)
   (inputs `(("gzip" ,gzip)
	     ("tar" ,tar)))

   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))

	(define (set-path)
	  (let* ((packages (alist-delete "source" %build-inputs))
                 (packages-path (map cdr packages)))
            (setenv
             "PATH"
             (apply
              string-append
              (getenv "PATH") ":"
              (map (lambda (p) (string-append p "/bin:"))
                   packages-path)))))
	
	(let* ((source (assoc-ref %build-inputs "source"))
	       (out (assoc-ref %outputs "out"))
	       (TAR (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
	       )

	  (setenv "GUIX_LD_WRAPPER_ALLOW_IMPURITIES" "no")

	  (set-path)
	  (mkdir-p (string-append out "/bin"))
	  (mkdir-p (string-append out "/share/doc"))
	  (mkdir-p (string-append out "/share/samples"))
	  
	  (invoke TAR "zxvf" source)
	  (chdir "timing-gen-0.9.8")
	  (copy-recursively "doc" (string-append out "/share/doc"))
	  (copy-recursively "samples" (string-append out "/share/samples"))
	  (copy-file "timing-gen" (string-append out "/bin/timing-gen"))
	  (copy-file "timing-gen-viewer" (string-append out "/bin/timing-gen-viewer"))
	  
	  ))))

   (home-page "https://sourceforge.net/projects/timing-gen/")
   (synopsis "Timing Gen")
   (description "Timing-gen is a tool to generate high quality Postscript timing
diagrams from text input files.")
   (license license:expat)))


(define-public Unuran-1.8.1
  (package
   (name "Unuran")
   (version "1.8.1")
   (source
    (origin
     (method url-fetch)
     (uri "http://statistik.wu-wien.ac.at/unuran/unuran-1.8.1.tar.gz")
     (sha256
      (base32
       "14si7jqq6mfk5whfbnvdhm97ylg0cpw3s12hcjndnmvqhnbaww62"))))
   (build-system gnu-build-system)
   (inputs
    `(("perl" ,perl)))

   (arguments
    '(#:configure-flags '("--with-pic")))
   
   (home-page "http://statistik.wu-wien.ac.at/unuran/")
   (synopsis "Universal Non-Uniform RAndom Number generator")
   (description "UNU.RAN (Universal Non-Uniform RAndom Number
generator) is a collection of algorithms for generating non-uniform
pseudorandom variates")
   (license license:gpl2)))


;; TODO (Help needed !)

'(define-public cubix-3.0
   (package
    (name "cubix-3.0")
    (version "3.0")
    (source (origin
	     (method url-fetch)
             (uri "https://gitlab.in2p3.fr/dudouet/Cubix/-/archive/Cubix-3.0/Cubix-Cubix-3.0.tar.gz")
	     (sha256
              (base32
	       "1vpymb22k727h62n0bzb30bc2h85kh7qimjwnd7zik6q32vhi2mr"))))
    (build-system cmake-build-system)
    (inputs
     `(
       ;; ("bash" ,bash)
       ;; ("gcc-toolchain" ,gcc-toolchain)
       ("ROOT" ,ROOT-6.26.10)
      
       ))
    (arguments `())
    (home-page "https://gitlab.in2p3.fr/dudouet/Cubix")
    (synopsis "Cubix")
    (description "Cubix")
    (license license:expat)))

;; TODO configure is an interactive script
;; https://geant4.kek.jp/~tanaka/DAWN/About_DAWN.html
;; https://twiki.cern.ch/twiki/bin/view/CLIC/DawnVisualization
'(define-public dawn-3.91a
   (package
    (name "dawn-3.91a")
    (version "3.91a")
    (source (origin
	     (method url-fetch)
             (uri "http://geant4.kek.jp/~tanaka/src/dawn_3_91a.tgz")
	     (sha256
              (base32
	       "1x7mpi77jylsv8mzsqs0ppchbq147azd0b94i2qq2xhis7m5bn41"))))
    (build-system cmake-build-system)
    (inputs
     `(("bash" ,bash)
       ("gcc-toolchain" ,gcc-toolchain)))
    (arguments `())
    (home-page "http://geant4.kek.jp/~tanaka")
    (synopsis "Dawn")
    (description "Fukui Renderer DAWN (Drawer for Academic WritiNgs)")
    (license license:expat)))

;; Problem: rucio-webui has no tags yet
'(define-public rucio-1.30.1
   (package
    (name "rucio-1.30.1")
    (version "1.30.1")
    (source
     (origin
      (method url-fetch)
      (uri "https://github.com/rucio/rucio/archive/refs/tags/1.30.1.tar.gz")
      (sha256
       (base32 "1iw6njbnwjs7chqr25mylnxi5qkx35wdj8z7b90g1j5b8a884k6a"))))
    (build-system python-build-system)
    (inputs
     `(("python-3" ,python-3)
       ("bash" ,bash)
       ("python-boto3" ,python-boto3-1.21.13) ;;
       ("python-prometheus-client-0.13.1" ,python-prometheus-client-0.13.1)
       ("python-pyoidc-1.3.0" ,python-pyoidc-1.3.0)
       ("python-flask-2.0.3" ,python-flask-2.0.3)
       ("python-redis-4.1.4" ,python-redis-4.1.4)
       ("python-google-auth-2.6.0" ,python-google-auth-2.6.0)
       ("python-typing-extensions-4.4.0" ,python-typing-extensions-4.4.0)
       ("python-defusedxml" ,python-defusedxml)
       ("python-beaker-1.12.0" ,python-beaker-1.12.0)
       ("python-pyjwkest-1.4.0" ,python-pyjwkest-1.4.0)
       ("python-pycryptodomex" ,python-pycryptodomex)
       ("python-future" ,python-future)

       ;; Requirement.parse('aiohttp<4.0.0,>=3.6.2')
       ("python-aiohttp" ,python-aiohttp)
       ;; maxminddb<3.0.0,>=2.2.0
       ("python-maxminddb" ,python-maxminddb)
       ;; deprecated>=1.2.3
       ("python-deprecated" ,python-deprecated)
       ;; packaging>=20.4
       ("python-packaging" ,python-packaging)
       ;; geoip2==4.5.0
       ("python-geoip2-4.5.0" ,python-geoip2-4.5.0)
       ;; statsd==3.3.0
       ("python-statsd" ,python-statsd)
       ;; stomp.py==6.1.1
       ("python-stomp-6.1.1" ,python-stomp-6.1.1)
       ;; pymemcache==3.5.2
       ("python-pymemcache-3.5.2" ,python-pymemcache-3.5.2)
       ;; alembic~=1.7.6
       ("python-alembic-1.7.6" ,python-alembic-1.7.6)
       ;; SQLAlchemy==1.4.31
       ("python-sqlalchemy-1.4.31" ,python-sqlalchemy-1.4.31)
       ;; python-magic~=0.4.25
       ("python-magic-0.4.25" ,python-magic-0.4.25)
       ;; argcomplete~=1.12.3
       ("python-argcomplete-1.12.3" ,python-argcomplete-1.12.3)
       ;; paramiko~=2.11.0
       ("python-paramiko-2.11.0" ,python-paramiko-2.11.0)
       ;; jsonschema~=3.2.0
       ("python-jsonschema-3.2.0" ,python-jsonschema-3.2.0)
       ;; tabulate~=0.8.0
       ("python-tabulate" ,python-tabulate)
       ;; dogpile.cache<=1.1.5,>=1.1.2
       ("python-dogpile.cache" ,python-dogpile.cache-1.1.5) 
       ;; urllib3<=1.26.8,>=1.24.2
       ("python-urllib3" ,python-urllib3-1.26.8)
       ;; requests<=2.27.1,>=2.20.0
       ("python-requests-2.27.1" ,python-requests-2.27.1)
       ;; charset_normalizer~=2.0.0
       ("python-charset-normalizer-2.0.0" ,python-charset-normalizer-2.0.0)
       ))
    (arguments
     `(#:tests? #f

       #:phases
       (modify-phases
        %standard-phases
        (add-before 'build ;; For python-build-system
		    'patches
		    (lambda*
		        (#:key inputs #:allow-other-keys)
		      (let ((BASH_DIR (assoc-ref inputs "bash")))
		        (substitute*
		         '("setuputil.py"
		           "tools/generate_version.py"
		           "tools/prepare-commit-msg"
		           "tools/test/oracle_setup.sh"
		           "tools/test/run_tests.py"
		           "tools/add_header")
		         (("/bin/sh")
                          (string-append BASH_DIR "/bin/sh")))
		        (substitute*
		         "requirements.txt"
		         ;; DistributionNotFound(Requirement.parse('stomp.py==6.1.1'), {'rucio'})
		         (("stomp.py==6.1.1") "# stomp.py==6.1.1")
		         ;; ContextualVersionConflict(alembic 1.7.6.dev0 ...)
		         (("alembic~=1.7.6") "# alembic~=1.7.6")
		         ;; ContextualVersionConflict(SQLAlchemy 1.4.31.dev0
		         (("SQLAlchemy==1.4.31") "# SQLAlchemy==1.4.31")
		         )))))))
   
    (home-page "http://rucio.cern.ch/")
    (synopsis "Rucio")
    (description "Rucio is a software framework that provides functionality
to organize, manage, and access large volumes of scientific
data using customisable policies. The data can be spread
across globally distributed locations and across
heterogeneous data centers, uniting different storage and
network technologies as a single federated entity. Rucio
offers advanced features such as distributed data recovery
or adaptive replication, and is highly scalable, modular,
and extensible. Rucio has been originally developed to meet
the requirements of the high-energy physics experiment
ATLAS, and is continuously extended to support LHC
experiments and other diverse scientific communities.")
    (license license:asl2.0)))

'(define-public dirac-8.0.6
   (package
    (name "dirac-8.0.6")
    (version "8.0.6")
    (source (origin
	     (method url-fetch)
             (uri "https://github.com/DIRACGrid/DIRAC/archive/refs/tags/v8.0.6.tar.gz")
	     (sha256
              (base32
	       "1f7r9d843xfcq1jbm5csajdmhndrghdli9nwkhzv20s0qg6rm8cv"))))
    (build-system python-build-system)
    (inputs
     `(
       ("python-3" ,python-3)
       ("python-authlib" ,python-authlib)
       ("python-boto3" ,python-boto3)
       ("python-botocore" ,python-botocore)
       ("python-cachetools" ,python-cachetools)
       ("python-certifi" ,python-certifi)
       ("python-dateutil" ,python-dateutil)
       ("python-dominate" ,python-dominate)
       ("python-importlib-metadata" ,python-importlib-metadata)
       ("python-m2crypto" ,python-m2crypto)
       ("python-pexpect" ,python-pexpect)
       ("python-prompt-toolkit" ,python-prompt-toolkit)
       ("python-psutil" ,python-psutil)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pyasn1-modules" ,python-pyasn1-modules)
       ("python-pyjwt" ,python-pyjwt)
       ("python-pyparsing" ,python-pyparsing)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-setuptools" ,python-setuptools)
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-typing-extensions" ,python-typing-extensions-4.4.0) ;; >= 4.3.0 !
       ("python-wheel" ,python-wheel)

       ("rucio" ,rucio-1.30.0)

       ;; Dependencies in environment.yml
       ;; TODO: diraccfg db12 fts3 gfal2-python 

       ))
    (arguments `(#:tests? #f ;; No network 
		 ))
    (home-page "http://diracgrid.org/")
    (synopsis "DIRAC")
    (description "DIRAC provides a complete solution to one or more user community requiring access to distributed resources. DIRAC builds a layer between the users and the resources offering a common interface to a number of heterogeneous providers, integrating them in a seamless manner, providing interoperability, at the same time as an optimized, transparent and reliable usage of the resources.")
    (license license:gpl3+)))

'(define-public gammaware-AGATAD_P2_COM_001
   (package
    (name "gammaware-AGATAD_P2_COM_001")
    (version "AGATAD_P2_COM_001")
    (source
     (origin
      (method url-fetch)
      (uri "https://gitlab.in2p3.fr/IPNL_GAMMA/gammaware/-/archive/AGATAD_P2_COM_001/gammaware-AGATAD_P2_COM_001.tar.gz")
      (sha256
       (base32
        "1gmnrrnw7sg7d9y80761y89a35z01568jbj59sdi4mjigffijd3x"))))
    (build-system cmake-build-system)
    (inputs
     `(("bash" ,bash)
       ("gcc-toolchain" ,gcc-toolchain)
       ))

    (home-page "https://gitlab.in2p3.fr/IPNL_GAMMA/gammaware")
    (synopsis "gammaware")
    (description "General Tools to play with gamma-ray spectroscopy related data")
    (license license:gpl2+)))

