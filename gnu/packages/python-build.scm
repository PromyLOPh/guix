;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2018, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages python-build)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

;;; Commentary:
;;;
;;; Python packages to build... Python packages.  Since they are bound to be
;;; relied on by many, their dependencies should be kept minimal, and this
;;; module should not depend on other modules containing Python packages.
;;;
;;; Code:

(define-public python-setuptools
  (package
    (name "python-setuptools")
    (version "52.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "setuptools" version))
      (sha256
       (base32
        "15ibjdjhkwgj6qbmpsxikkqdfsb1550z46fly7dm15ah4bk1wfpv"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Remove included binaries which are used to build self-extracting
          ;; installers for Windows.
          ;; TODO: Find some way to build them ourself so we can include them.
          (for-each delete-file (find-files "setuptools" "^(cli|gui).*\\.exe$"))
          #t))))
    (outputs '("out" "wheel"))
    (build-system python-build-system)
    ;; FIXME: Tests require pytest, which itself relies on setuptools.
    ;; One could bootstrap with an internal untested setuptools.
    (arguments
     `(#:tests? #f
       #:python ,python-wrapper ; Break cycle with default build system’s setuptools dependency.
       #:phases (modify-phases %standard-phases
                  ;; Use this setuptools’ sources to bootstrap themselves.
                  (add-before 'build 'set-PYTHONPATH
                    (lambda _
                      (format #t "current working dir ~s~%" (getcwd))
                      (setenv "GUIX_PYTHONPATH"
                              (string-append ".:" (getenv "GUIX_PYTHONPATH")))
                      #t)))))
    ;; Required to build wheels.
    (propagated-inputs `(("python-wheel" ,python-wheel)))
    (home-page "https://pypi.org/project/setuptools/")
    (synopsis
     "Library designed to facilitate packaging Python projects")
    (description
     "Setuptools is a fully-featured, stable library designed to facilitate
packaging Python projects, where packaging includes:
Python package and module definitions,
distribution package metadata,
test hooks,
project installation,
platform-specific details,
Python 3 support.")
    ;; TODO: setuptools now bundles the following libraries:
    ;; packaging, pyparsing, six and appdirs. How to unbundle?
    (license (list license:psfl        ; setuptools itself
                   license:expat       ; six, appdirs, pyparsing
                   license:asl2.0      ; packaging is dual ASL2/BSD-2
                   license:bsd-2))))
    ;(properties `((python2-variant . ,(delay python2-setuptools))))))

;; Break loop between python-setuptools and python-wheel.
(define-public python-setuptools-bootstrap
  (package
    (inherit python-setuptools)
    (name "python-setuptools-bootstrap")
    (propagated-inputs `(("python-wheel" ,python-wheel-bootstrap)))))

(define-public python-wheel
  (package
    (name "python-wheel")
    (version "0.33.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "wheel" version))
        (sha256
         (base32
          "0ii6f34rvpjg3nmw4bc2h7fhdsy38y1h93hghncfs5akfrldmj8h"))))
    (outputs '("out" "wheel"))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-wrapper)) ; Break cycle with python-toolchain-for-build.
    (native-inputs `(("python-setuptools" ,python-setuptools-bootstrap)))
    (home-page "https://bitbucket.org/pypa/wheel/")
    (synopsis "Format for built Python packages")
    (description
     "A wheel is a ZIP-format archive with a specially formatted filename and
the @code{.whl} extension.  It is designed to contain all the files for a PEP
376 compatible install in a way that is very close to the on-disk format.  Many
packages will be properly installed with only the @code{Unpack} step and the
unpacked archive preserves enough information to @code{Spread} (copy data and
scripts to their final locations) at any later time.  Wheel files can be
installed with a newer @code{pip} or with wheel's own command line utility.")
    (license license:expat)))

(define-public python-wheel-bootstrap
  (package
    (inherit python-wheel)
    (name "python-wheel-bootstrap")
    (build-system copy-build-system)
    (native-inputs '()) ; Break cycle to setuptools.
    (arguments
     `(#:install-plan
       ;; XXX: Do not hard-code Python version.
       '(("wheel" "lib/python3.9/site-packages/wheel"))
       #:phases
       (modify-phases %standard-phases
         ;; Add metadata for setuptools, so it will find the wheel-building code.
         (add-after 'install 'install-metadata
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site-dir (string-append out "/lib/python3.9/site-packages"))
                    (metadata-dir (string-append site-dir "/wheel.egg-info")))
               (mkdir-p metadata-dir)
               (call-with-output-file (string-append metadata-dir "/entry_points.txt")
                 (lambda (port)
                   (format port "~
                           [distutils.commands]~@
                           bdist_wheel = wheel.bdist_wheel:bdist_wheel~%")))))))))))

(define-public python2-wheel
  (package-with-python2 python-wheel))

;;; XXX: Not really at home, but this seems the best place to prevent circular
;;; module dependencies.
(define-public python-toml
  (package
    (name "python-toml")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "toml" version))
       (sha256
        (base32 "13z6rff86bzdpl094x0vmfvls779931xj90dlbs9kpfm138s3gdk"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                     ;no tests suite in release
    (home-page "https://github.com/uiri/toml")
    (synopsis "Library for TOML")
    (description
     "@code{toml} is a library for parsing and creating Tom's Obvious, Minimal
Language (TOML) configuration files.")
    (license license:expat)))

(define-public python-pytoml
  (package
    (name "python-pytoml")
    (version "0.1.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytoml" version))
       (sha256
        (base32
         "1rv1byiw82k7mj6aprcrqi2vdabs801y97xhfnrz7kxds34ggv4f"))))
    (build-system python-build-system)
    (home-page "https://github.com/avakar/pytoml")
    (synopsis "Parser for TOML")
    (description "This package provides a Python parser for TOML-0.4.0.")
    (license license:expat)))

(define-public python-six-bootstrap
  (package
    (name "python-six-bootstrap")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "six" version))
       (sha256
        (base32
         "09n9qih9rpj95q3r4a40li7hk6swma11syvgwdc68qm1fxsc6q8y"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))          ;to avoid pytest dependency
    (home-page "https://pypi.org/project/six/")
    (synopsis "Python 2 and 3 compatibility utilities")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.
Six supports every Python version since 2.5.  It is contained in only one
Python file, so it can be easily copied into your project.")
    (license license:x11)))

(define-public python2-six-bootstrap
  (package-with-python2 python-six-bootstrap))

(define-public python-tomli
  (package
    (name "python-tomli")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tomli" version))
       (sha256
        (base32 "1vjg44narb7hdiazdmbv8bfv7pi6phnq7nxm6aphx0iqxcah1kn6"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                      ;disabled to avoid extra dependencies
    (native-inputs
     `(("python-flit-core" ,python-flit-core-bootstrap)))
    (home-page "https://github.com/hukkin/tomli")
    (synopsis "Small and fast TOML parser")
    (description "Tomli is a minimal TOML parser that is fully compatible with
@url{https://toml.io/en/v1.0.0,TOML v1.0.0}.  It is about 2.4 times as fast as
@code{python-toml}.")
    (license license:expat)))

;; Bootstrap variant, which does not depend on python-build-system at
;; all. This breaks the cycle between flit-core and tomli.
(define-public python-tomli-bootstrap
  (package
    (inherit python-tomli)
    (name "python-tomli-bootstrap")
    (build-system copy-build-system)
    (native-inputs '()) ; Remove native-inputs from python-tomli.
    (arguments
     '(#:install-plan
       ;; XXX: Do not hard-code Python version.
       '(("tomli" "lib/python3.9/site-packages/tomli"))))))

(define-public python-pep517-bootstrap
  (hidden-package
   (package
     (name "python-pep517-bootstrap")
     (version "0.12.0")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/pypa/pep517")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1giljizqy337lhbhx7m0lbxw9fvsjqd97fgkf3i0mw3p7wpn3jy7"))))
     (build-system python-build-system)
     (arguments
      `(#:tests? #f)) ; To avoid circular dependencies.
     (propagated-inputs
      `(("python-tomli" ,python-tomli)))
     (native-inputs
      `(;; Build system.
       ("python-flit-core" ,python-flit-core-bootstrap)))
     (home-page "https://github.com/pypa/pep517")
     (synopsis "Wrappers to build Python packages using PEP 517 hooks")
     (description
      "Wrappers to build Python packages using PEP 517 hooks.")
     (license license:expat))))

(define-public python-pyparsing
  (package
    (name "python-pyparsing")
    (version "2.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyparsing" version))
       (sha256
        (base32 "1hgc8qrbq1ymxbwfbjghv01fm3fbpjwpjwi0bcailxxzhf3yq0y2"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ;no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (string-append (assoc-ref outputs "doc")
                                        "/share/doc/" ,name "-" ,version))
                    (html-doc (string-append doc "/html"))
                    (examples (string-append doc "/examples")))
               (mkdir-p html-doc)
               (mkdir-p examples)
               (for-each
                (lambda (dir tgt)
                  (map (lambda (file)
                         (install-file file tgt))
                       (find-files dir ".*")))
                (list "docs" "htmldoc" "examples")
                (list doc html-doc examples))))))))
    (home-page "https://github.com/pyparsing/pyparsing")
    (synopsis "Python parsing class library")
    (description
     "The pyparsing module is an alternative approach to creating and
executing simple grammars, vs. the traditional lex/yacc approach, or the use
of regular expressions.  The pyparsing module provides a library of classes
that client code uses to construct the grammar directly in Python code.")
    (license license:expat)))

(define-public python-pyparsing-2.4.7
  (package
    (inherit python-pyparsing)
    (version "2.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyparsing" version))
       (sha256
        (base32 "1hgc8qrbq1ymxbwfbjghv01fm3fbpjwpjwi0bcailxxzhf3yq0y2"))))))

(define-public python2-pyparsing
  (package-with-python2 python-pyparsing))

(define-public python-packaging-bootstrap
  (package
    (name "python-packaging-bootstrap")
    (version "20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "packaging" version))
       ;; XXX: The URL in the patch file is wrong, it should be
       ;; <https://github.com/pypa/packaging/pull/256>.
       (patches (search-patches "python-packaging-test-arch.patch"))
       (sha256
        (base32
         "1y2ip3a4ykkpgnwgn85j6hkspcl0cg3mzms97f40mk57vwqq67gy"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))         ;disabled to avoid extra dependencies
    (propagated-inputs
     `(("python-pyparsing" ,python-pyparsing)
       ("python-six-bootstrap" ,python-six-bootstrap)))
    (home-page "https://github.com/pypa/packaging")
    (synopsis "Core utilities for Python packages")
    (description "Packaging is a Python module for dealing with Python packages.
It offers an interface for working with package versions, names, and dependency
information.")
    ;; From 'LICENSE': This software is made available under the terms of
    ;; *either* of the licenses found in LICENSE.APACHE or LICENSE.BSD.
    ;; Contributions to this software is made under the terms of *both* these
    ;; licenses.
    (license (list license:asl2.0 license:bsd-2))))

(define-public python2-packaging-bootstrap
  (package-with-python2 python-packaging-bootstrap))

;;; The name 'python-pypa-build' is chosen rather than 'python-build' to avoid
;;; a name clash with python-build from (guix build-system python).
(define-public python-pypa-build
  (package
    (name "python-pypa-build")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "build" version))
              (sha256
               (base32
                "17xqija27x4my1yrnk6q2vwln60r39g2dhby9zg2l99qjgbdrahs"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ;to tests in the PyPI release
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'use-toml-instead-of-tomli
                    ;; Using toml instead of tomli eases bootstrapping.
                    (lambda _
                      (substitute* "setup.cfg"
                        (("tomli>=.*")
                         "toml\n")))))))
    (propagated-inputs
     `(("python-packaging" ,python-packaging-bootstrap)
       ("python-pep517", python-pep517-bootstrap)
       ("python-toml" ,python-toml)))
    (home-page "https://pypa-build.readthedocs.io/en/latest/")
    (synopsis "Simple Python PEP 517 package builder")
    (description "The @command{build} command invokes the PEP 517 hooks to
build a distribution package.  It is a simple build tool and does not perform
any dependency management.  It aims to keep dependencies to a minimum, in
order to make bootstrapping easier.")
    (license license:expat)))

(define-public python-poetry-core
  (package
    (name "python-poetry-core")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "poetry-core" version))
       (sha256
        (base32 "01n2rbsvks7snrq3m1d08r3xz9q2715ajb62fdb6rvqnb9sirhcq"))))
    (build-system python-build-system)
    (home-page "https://github.com/python-poetry/poetry-core")
    (synopsis "Poetry PEP 517 build back-end")
    (description
     "The @code{poetry-core} module provides a PEP 517 build back-end
implementation developed for Poetry.  This project is intended to be
a light weight, fully compliant, self-contained package allowing PEP 517
compatible build front-ends to build Poetry managed projects.")
    (license license:expat)))

;; Be aware that python-flit inherits from python-flit-core and must be
;; updated too, if you change anything here.
(define-public python-flit-core-bootstrap
  (package
    (name "python-flit-core-bootstrap")
    (version "3.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pypa/flit.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nsiksar883bz8kjcz5ap7jax3pz2ysmq9p79ggavq8imp5vxapl"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-tomli" ,python-tomli-bootstrap)))
    (native-inputs '()) ; Remove self-dependency, because of inheritance.
    (outputs '("out" "wheel"))
    (arguments
     `(#:tests? #f
       #:build-backend "flit_core.build_thyself"
       #:phases
       (modify-phases %standard-phases
         ;; python-tomli-bootstrap is not installed with metadata and triggers the sanity check.
         (delete 'sanity-check)
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "flit_core"))))))
    (home-page "https://flit.readthedocs.io/")
    (synopsis
      "Core package of the Flit Python build system")
    (description
      "This package provides @code{flit-core}, a PEP 517 build
backend for packages using Flit.  The only public interface is the API
specified by PEP 517, @code{flit_core.buildapi}.")
    (license license:bsd-3)))

(define-public python-flit-core
  (package
    (inherit python-flit-core-bootstrap)
    (name "python-flit-core")
    (propagated-inputs
      `(("python-tomli" ,python-tomli)))))

