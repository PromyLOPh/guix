;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2019 Ludovic Courtès <ludo@gnu.org>
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


(define-module (test-builders)
  #:use-module (guix download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module (guix derivations)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module (guix packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;; Test the higher-level builders.

(define %store
  (open-connection-for-tests))

(define url-fetch*
  (store-lower url-fetch))


(test-begin "builders")

(unless (network-reachable?) (test-skip 1))
(test-assert "url-fetch"
  (let* ((url      '("http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz"
                     "ftp://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz"))
         (hash     (nix-base32-string->bytevector
                    "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))
         (drv      (url-fetch* %store url 'sha256 hash
                               #:guile %bootstrap-guile))
         (out-path (derivation->output-path drv)))
    (and (build-derivations %store (list drv))
         (file-exists? out-path)
         (valid-path? %store out-path))))

(test-assert "url-fetch, file"
  (let* ((file (search-path %load-path "guix.scm"))
         (hash (call-with-input-file file port-sha256))
         (out  (url-fetch* %store file 'sha256 hash)))
    (and (file-exists? out)
         (valid-path? %store out))))

(test-assert "url-fetch, file URI"
  (let* ((file (search-path %load-path "guix.scm"))
         (hash (call-with-input-file file port-sha256))
         (out  (url-fetch* %store
                           (string-append "file://" (canonicalize-path file))
                           'sha256 hash)))
    (and (file-exists? out)
         (valid-path? %store out))))

(test-assert "gnu-build-system"
  (build-system? gnu-build-system))


(define* (make-python-dummy name #:key (setup-py-extra "") (init-py "") (use-setuptools? #t))
  (package
    (name (string-append "python-dummy-" name))
    (version "0.1")
    (source #f) ; source is generated in 'unpack
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:use-setuptools? ,use-setuptools?
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda _
             (mkdir-p "src/dummy")
             (chdir "src")
             (with-output-to-file "dummy/__init__.py"
               (lambda _
                 (display ,init-py)))
             (with-output-to-file "setup.py"
               (lambda _
                 (format #t "\
~a
setup(
     name='dummy-~a',
     version='0.1',
     packages=['dummy'],
     ~a
     )"
                    (if ,use-setuptools?
                      "from setuptools import setup"
                      "from distutils.core import setup")
                    ,name ,setup-py-extra)))
               #t)))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define python-dummy-ok
  (make-python-dummy "ok"))

(define python2-dummy-ok
  (package-with-python2 python-dummy-ok))

;; distutil won’t install any metadata, so make sure our script does not fail
;; on a otherwise fine package.
(define python-dummy-no-setuptools
  (make-python-dummy
    "no-setuptools" #:use-setuptools? #f))

(define python2-dummy-no-setuptools
  (package-with-python2 python-dummy-no-setuptools))

(define python-dummy-fail-requirements
  (make-python-dummy "fail-requirements"
    #:setup-py-extra "install_requires=['nonexistent'],"))

(define python2-dummy-fail-requirements
  (package-with-python2 python-dummy-fail-requirements))

(define python-dummy-fail-import
  (make-python-dummy "fail-import" #:init-py "import nonexistent"))

(define python2-dummy-fail-import
  (package-with-python2 python-dummy-fail-import))

(define python-dummy-fail-console-script
  (make-python-dummy "fail-console-script"
    #:setup-py-extra (string-append "entry_points={'console_scripts': "
                                    "['broken = dummy:nonexistent']},")))

(define python2-dummy-fail-console-script
  (package-with-python2 python-dummy-fail-console-script))

(define (check-build-success store p)
  (unless store (test-skip 1))
  (test-assert (string-append "python-build-system: " (package-name p))
    (let* ((drv (package-derivation store p)))
      (build-derivations store (list drv)))))

(define (check-build-failure store p)
  (unless store (test-skip 1))
  (test-assert (string-append "python-build-system: " (package-name p))
    (not (false-if-exception (package-derivation store python-dummy-fail-requirements)))))

(with-external-store store
  (for-each (lambda (p) (check-build-success store p))
            (list
              python-dummy-ok
              python-dummy-no-setuptools
              python2-dummy-ok
              python2-dummy-no-setuptools))
  (for-each (lambda (p) (check-build-failure store p))
            (list
              python-dummy-fail-requirements
              python-dummy-fail-import
              python-dummy-fail-console-script
              python2-dummy-fail-requirements
              python2-dummy-fail-import
              python2-dummy-fail-console-script)))

(test-end "builders")
