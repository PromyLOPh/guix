;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;;
;;; This file is part of GNU Guix.
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

(define-module (gnu packages valgrind)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages))

(define-public valgrind
  (package
    (name "valgrind")
    (version "3.10.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://valgrind.org/downloads/valgrind-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "15xrzhfnwwn7n1sfbkwvdbvs6zk0zx718n6zd5i1nrnvdp13s9gs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'install 'patch-suppression-files
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Don't assume the FHS.
                   (let* ((out (assoc-ref outputs "out"))
                          (dir (string-append out "/lib/valgrind")))
                     (substitute* (find-files dir "\\.supp$")
                       (("obj:/lib") "obj:*/lib")
                       (("obj:/usr/X11R6/lib") "obj:*/lib")
                       (("obj:/usr/lib") "obj:*/lib"))
                     #t))
                 %standard-phases)))
    (inputs `(;; GDB is needed to provide a sane default for `--db-command'.
              ("gdb" ,gdb)))
    (native-inputs `(("perl" ,perl)))
    (home-page "http://www.valgrind.org/")
    (synopsis "Debugging and profiling tool suite")
    (description
     "Valgrind is an instrumentation framework for building dynamic analysis
tools.  There are Valgrind tools that can automatically detect many memory
management and threading bugs, and profile your programs in detail.  You can
also use Valgrind to build new tools.")
    (license gpl2+)

    ;; Building VEX on mips64el-linux fails with "opcode not supported on this
    ;; processor: mips3".
    (supported-systems (delete "mips64el-linux" %supported-systems))))
