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


(define-module (guix-science-nonfree packages gate)

  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xml) 
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix-science-nonfree packages geant4)
  #:use-module (guix-science-nonfree packages physics)
  #:use-module (guix-science-nonfree packages root)

  )



(define-public GATE-9.4
  (package
   (name "GATE")
   (version "9.4")
   (source
    (origin
     (method url-fetch) 
     (uri (string-append "https://github.com/OpenGATE/Gate/archive/refs/tags/v" version ".tar.gz"))
     (sha256
      (base32
       "0z0idi687y0gi88sg06yg2rp34q4w2y695cg1ma0wp5jn5m3zicn"))))
   (build-system cmake-build-system)
   (inputs
    `(("GEANT4" ,geant4-no-multithreaded-11-2)
      ("ROOT" ,ROOT-6.32.06)
      ("VDT" ,vdt-0.4.4)
      ("libxml2" ,libxml2)
      
      ))

   (arguments

    ;; No tests
    '(#:tests? #f))
   
   (home-page "http://www.opengatecollaboration.org/")                                                                                  
   (synopsis "GATE") 
   (description "GATE is an advanced opensource software developed by the international OpenGATE collaboration and dedicated to numerical simulations in medical imaging and radiotherapy. GATE is based on the Geant4 toolkit.") 
   (license license:lgpl3)))
