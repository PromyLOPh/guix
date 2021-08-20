;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
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

(define-module (gnu packages jupyter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public python-ipython
  (package
    (name "python-ipython")
    (version "7.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipython" version ".tar.gz"))
       (sha256
        (base32 "103jkw18z7fnwdal1mdbijjxi1fndzn31g887lmj7ddpf2r07lyz"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-backcall" ,python-backcall)
       ("python-pyzmq" ,python-pyzmq)
       ("python-prompt-toolkit" ,python-prompt-toolkit-2)
       ("python-terminado" ,python-terminado)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-numpydoc" ,python-numpydoc)
       ("python-jedi" ,python-jedi)
       ("python-jinja2" ,python-jinja2)
       ("python-mistune" ,python-mistune)
       ("python-pexpect" ,python-pexpect)
       ("python-pickleshare" ,python-pickleshare)
       ("python-simplegeneric" ,python-simplegeneric)
       ("python-jsonschema" ,python-jsonschema)
       ("python-traitlets" ,python-traitlets)
       ("python-nbformat" ,python-nbformat)
       ("python-pygments" ,python-pygments)))
    (inputs
     `(("readline" ,readline)
       ("which" ,which)))
    (native-inputs
     `(("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)
       ("python-requests" ,python-requests) ;; for tests
       ("python-testpath" ,python-testpath)
       ("python-nose" ,python-nose)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-docs-reproducible
           (lambda _
             (substitute* "IPython/sphinxext/ipython_directive.py"
               ((".*import datetime") "")
               ((".*datetime.datetime.now\\(\\)") "")
               (("%timeit") "# %timeit"))
             #t))
         ;; Tests can only be run after the library has been installed and not
         ;; within the source directory.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (if tests?
                 (begin
                   ;; Make installed package available for running the tests
                   (add-installed-pythonpath inputs outputs)
                   (setenv "HOME" "/tmp/") ;; required by a test
                   ;; We only test the core because one of the other tests
                   ;; tries to import ipykernel.
                   (invoke "python" "IPython/testing/iptest.py"
                           "-v" "IPython/core/tests"))
                 #t)))
         (add-before 'check 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "./IPython/utils/_process_posix.py"
               (("/usr/bin/env', 'which") (which "which")))
             (substitute* "./IPython/core/tests/test_inputtransformer.py"
               (("#!/usr/bin/env python")
                (string-append "#!" (which "python"))))
             ;; This test introduces a circular dependency on ipykernel
             ;; (which depends on ipython).
             (delete-file "IPython/core/tests/test_display.py")
             ;; AttributeError: module 'IPython.core' has no attribute 'formatters'
             (delete-file "IPython/core/tests/test_interactiveshell.py")
             #t)))))
    (home-page "https://ipython.org")
    (synopsis "IPython is a tool for interactive computing in Python")
    (description
     "IPython provides a rich architecture for interactive computing with:
Powerful interactive shells, a browser-based notebook, support for interactive
data visualization, embeddable interpreters and tools for parallel
computing.")
    (license license:bsd-3)))

(define-public python-ipython-documentation
  (package
    (inherit python-ipython)
    (name "python-ipython-documentation")
    (version (package-version python-ipython))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (html (string-append doc "/html"))
                    (man1 (string-append data "/man/man1"))
                    (info (string-append data "/info"))
                    (examples (string-append doc "/examples"))
                    (python-arg (string-append "PYTHON=" (which "python"))))
               (setenv "LANG" "en_US.utf8")
               (with-directory-excursion "docs"
                 ;; FIXME: pdf fails to build
                 ;;(system* "make" "pdf" "PAPER=a4")
                 (system* "make" python-arg "html")
                 ;; FIXME: the generated texi file contains ^@^@, which trips
                 ;; up the parser.
                 ;; (system* "make" python-arg "info")
                 )
               (copy-recursively "docs/man" man1)
               (copy-recursively "examples" examples)
               (copy-recursively "docs/build/html" html)
               ;; (copy-file "docs/build/latex/ipython.pdf"
               ;;            (string-append doc "/ipython.pdf"))
               (mkdir-p info)
               ;; (copy-file "docs/build/texinfo/ipython.info"
               ;;            (string-append info "/ipython.info"))
               (copy-file "COPYING.rst" (string-append doc "/COPYING.rst")))
             #t)))))
    (inputs
     `(("python-ipython" ,python-ipython)
       ("python-ipykernel" ,python-ipykernel)))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)
       ;; FIXME: It's possible that a smaller union would work just as well.
       ("texlive" ,(texlive-union (list texlive-amsfonts/patched
                                        texlive-fonts-ec
                                        texlive-generic-ifxetex
                                        texlive-generic-pdftex
                                        texlive-latex-capt-of
                                        texlive-latex-cmap
                                        texlive-latex-environ
                                        texlive-latex-eqparbox
                                        texlive-latex-etoolbox
                                        texlive-latex-expdlist
                                        texlive-latex-fancyhdr
                                        texlive-latex-fancyvrb
                                        texlive-latex-fncychap
                                        texlive-latex-float
                                        texlive-latex-framed
                                        texlive-latex-geometry
                                        texlive-latex-graphics
                                        texlive-latex-hyperref
                                        texlive-latex-mdwtools
                                        texlive-latex-multirow
                                        texlive-latex-needspace
                                        texlive-latex-oberdiek
                                        texlive-latex-parskip
                                        texlive-latex-preview
                                        texlive-latex-tabulary
                                        texlive-latex-threeparttable
                                        texlive-latex-titlesec
                                        texlive-latex-trimspaces
                                        texlive-latex-ucs
                                        texlive-latex-upquote
                                        texlive-latex-url
                                        texlive-latex-varwidth
                                        texlive-latex-wrapfig)))
       ("texinfo" ,texinfo)))))

(define-public python-jupyter-protocol
  (package
    (name "python-jupyter-protocol")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_protocol" version))
              (sha256
               (base32
                "1bk3as5yw9y5nmq6l15nr46aby34phmvsx9kxgqnm5pd5q2b5h57"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-pyzmq" ,python-pyzmq)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter protocol implementation")
    (description
     "This Python library is an experimental implementation of the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
protocol} to be used by both clients and kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_protocol")))))

(define-public python-jupyter-kernel-mgmt
  (package
    (name "python-jupyter-kernel-mgmt")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_kernel_mgmt" version))
              (sha256
               (base32
                "0i7a78dn89ca8h0a42giyxwcmk6y4wrdr7q8h2ax9vybb84c795q"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-entrypoints" ,python-entrypoints)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-jupyter-protocol" ,python-jupyter-protocol)
       ("python-pyzmq" ,python-pyzmq)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://jupyter.org")
    (synopsis "Discover, launch, and communicate with Jupyter kernels")
    (description
     "This package is an experimental refactoring of the machinery for
launching and using Jupyter kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_kernel_mgmt")))))

(define-public python-jupyter-kernel-test
  (package
    (name "python-jupyter-kernel-test")
    (version "0.3")
    (home-page "https://github.com/jupyter/jupyter_kernel_test")
    (source (origin
              ;; PyPI has a ".whl" file but not a proper source release.
              ;; Thus, fetch code from Git.
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00iy74i4i8is6axb9vlsm0b9wxkvyyxnbl8r0i4gaj3xd788jm83"))))
    (build-system python-build-system)
    (arguments
     ;; The repo doesn't contain a "setup.py" file so install files manually.
     '(#:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (version (python-version (assoc-ref inputs "python")))
                             (pydir (string-append out "/lib/python"
                                                   version "/site-packages/"
                                                   "jupyter_kernel_test")))
                        (for-each (lambda (file)
                                    (install-file file pydir))
                                  (find-files "jupyter_kernel_test"
                                              "\\.py$"))
                        #t))))))
    (propagated-inputs
     `(("python-jupyter-kernel-mgmt" ,python-jupyter-kernel-mgmt)
       ("python-jupyter-protocol" ,python-jupyter-protocol)
       ("python-jsonschema" ,python-jsonschema)))
    (synopsis "Test Jupyter kernels")
    (description
     "@code{jupyter_kernel_test} is a tool for testing Jupyter kernels.  It
tests kernels for successful code execution and conformance with the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
Messaging Protocol}.")
    (license license:bsd-3)))

(define-public python-jupyterlab-pygments
  (package
    (name "python-jupyterlab-pygments")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_pygments" version))
       (sha256
        (base32
         "0ij14mmnc39nmf84i0av6j9glazjic7wzv1qyhr0j5966s3s1kfg"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; there are no tests
    (propagated-inputs
     `(("python-pygments" ,python-pygments)))
    (home-page "https://jupyter.org")
    (synopsis "Pygments theme using JupyterLab CSS variables")
    (description
     "This package contains a syntax coloring theme for pygments making use of
the JupyterLab CSS variables.")
    (license license:bsd-3)))

(define-public python-jupyter-packaging
  (package
    (name "python-jupyter-packaging")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_packaging" version))
       (sha256
        (base32
         "0r015c0m713d19asmpimsw6bk2sqv2lpd2nccgjzjdj5h1crg0bg"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-deprecation" ,python-deprecation)
       ("python-packaging" ,python-packaging)
       ("python-setuptools" ,python-setuptools)
       ("python-tomlkit" ,python-tomlkit)
       ("python-wheel" ,python-wheel)))
    (native-inputs
     `(("python-pypa-build" ,python-pypa-build)
       ("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter packaging utilities")
    (description "This package provides tools to help build and install
Jupyter Python packages that require a pre-build step that may include
JavaScript build steps.")
    (license license:bsd-3)))

(define-public python-jupyterlab-widgets
  (package
    (name "python-jupyterlab-widgets")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_widgets" version))
       (sha256
        (base32
         "0y7vhhas3qndiypcpcfnhrj9n92v2w4hdc86nn620s9h9nl2j6jw"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-jupyter-packaging" ,python-jupyter-packaging)
       ("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/jupyter-widgets/ipywidgets")
    (synopsis "Interactive widgets for Jupyter Notebooks")
    (description "ipywidgets, also known as jupyter-widgets or simply widgets,
are interactive HTML widgets for Jupyter notebooks and the IPython kernel.")
    (license license:bsd-3)))

(define-public python-nbclient
  (package
    (name "python-nbclient")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbclient" version))
       (sha256
        (base32
         "172q4r6mq0lg394di0pc6ipvniy14jg38wkdsj48r366609jf5yv"))))
    (build-system python-build-system)
    ;; Tests require a kernel via python-ipykernel, and also tools from
    ;; nbconvert.
    (arguments '(#:tests? #false))
    (propagated-inputs
     `(("python-async-generator" ,python-async-generator)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-nbformat" ,python-nbformat)
       ("python-nest-asyncio" ,python-nest-asyncio)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-black" ,python-black)
       ("python-bumpversion" ,python-bumpversion)
       ("python-check-manifest" ,python-check-manifest)
       ("python-codecov" ,python-codecov)
       ("python-coverage" ,python-coverage)
       ("python-flake8" ,python-flake8)
       ;; ("python-ipykernel" ,python-ipykernel)
       ;; ("python-ipython" ,python-ipython)
       ;; ("python-ipywidgets" ,python-ipywidgets)
       ("python-mypy" ,python-mypy)
       ("python-pip" ,python-pip)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-setuptools" ,python-setuptools)
       ("python-testpath" ,python-testpath)
       ("python-tox" ,python-tox)
       ("python-twine" ,python-twine)
       ("python-wheel" ,python-wheel)
       ("python-xmltodict" ,python-xmltodict)))
    (home-page "https://jupyter.org")
    (synopsis "Client library for executing notebooks")
    (description
     "This package provides a client library for executing notebooks. Formerly
nbconvert's @code{ExecutePreprocessor.}")
    (license license:bsd-3)))

(define-public python-jupyter-core
  (package
    (name "python-jupyter-core")
    (version "4.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append (pypi-uri "jupyter_core" version)))
       (sha256
        (base32
         "1d12j5hkff0xiax87pnhmzbsph3jqqzhz16h8xld7z2y4armq0kr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               ; Some tests write to $HOME.
               (setenv "HOME" "/tmp")
               ; Some tests load the installed package.
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv"))))
         (add-after 'unpack 'patch-testsuite
           (lambda _
             ;; test_not_on_path() and test_path_priority() try to run a test
             ;; that loads jupyter_core, so we need PYTHONPATH
             (substitute* "jupyter_core/tests/test_command.py"
               (("env = \\{'PATH': ''\\}")
                "env = {'PATH': '', 'PYTHONPATH': os.environ['PYTHONPATH']}")
               (("env = \\{'PATH':  str\\(b\\)\\}")
                "env = {'PATH': str(b), 'PYTHONPATH': os.environ['PYTHONPATH']}"))
             #t))
         ;; Migration is running whenever etc/jupyter exists, but the
         ;; Guix-managed directory will never contain any migratable IPython
         ;; config files and cannot be written to anyway, so just pretend we
         ;; already did that.
         (add-after 'install 'disable-migration
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/etc/jupyter"))
             (invoke "touch"
               (string-append
                 (assoc-ref outputs "out")
                 "/etc/jupyter/migrated")))))))
    (propagated-inputs
     `(("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-six" ,python-six)
       ("python-pytest" ,python-pytest)))
    ;; This package provides the `jupyter` binary and thus also exports the
    ;; search paths.
    (native-search-paths
     (list (search-path-specification
            (variable "JUPYTER_CONFIG_DIR")
            (files '("etc/jupyter")))
           (search-path-specification
            (variable "JUPYTER_PATH")
            (files '("share/jupyter")))))
    (home-page "https://jupyter.org/")
    (synopsis "Jupyter base package")
    (description
     "Jupyter core is the base package on which Jupyter projects rely.")
    (license license:bsd-3)))

(define-public python-jupyter-client
  (package
    (name "python-jupyter-client")
    (version "6.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_client" version))
       (sha256
        (base32
         "10p7fcgvv9hvz9zical9wk68ks5ssak2ykbzx65wm1k1hk8a3g64"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-tool-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((iproute (assoc-ref inputs "iproute")))
               (substitute* "jupyter_client/localinterfaces.py"
                 (("'ip'")
                  (string-append "'" iproute "/sbin/ip'")))
               #t)))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Some tests try to write to $HOME.
               (setenv "HOME" "/tmp")
               (invoke "pytest" "-vv")))))))
    (inputs
     `(("iproute" ,iproute)))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-pyzmq" ,python-pyzmq)
       ("python-tornado" ,python-tornado-6)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-asyncio" ,python-pytest-asyncio)
       ("python-pytest-timeout" ,python-pytest-timeout)
       ("python-async-generator" ,python-async-generator)
       ("python-mock" ,python-mock)
       ("python-msgpack" ,python-msgpack)
       ("python-ipython" ,python-ipython)
       ("python-ipykernel" ,python-ipykernel-bootstrap)))
    (home-page "https://jupyter.org/")
    (synopsis "Jupyter protocol implementation and client libraries")
    (description
     "The @code{jupyter_client} package contains the reference implementation
of the Jupyter protocol.  It also provides client and kernel management APIs
for working with kernels, and the @code{jupyter kernelspec} entrypoint for
installing @code{kernelspec}s for use with Jupyter frontends.")
    (license license:bsd-3)))

;; Bootstrap variant of jupyter-client, which breaks the loop between ipykernel
;; and jupyter-client by removing the former from its native-inputs and
;; disabling tests.
(define-public python-jupyter-client-bootstrap
  (let ((base python-jupyter-client))
    (hidden-package
      (package
        (inherit base)
        (name "python-jupyter-client-bootstrap")
        (arguments
          `(#:tests? #f
            ,@(package-arguments base)))
        (native-inputs `())))))

(define-public python-traitlets
  (package
    (name "python-traitlets")
    (version "4.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "traitlets" version))
       (sha256
        (base32
         "1xsrwgivpkxlbr4dfndfsi098s29yqgswgjc1qqn69yxklvfw8yh"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (invoke "pytest" "-vv" "traitlets"))))))
    (propagated-inputs
     `(("python-ipython-genutils" ,python-ipython-genutils)
       ("python-decorator" ,python-decorator)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (properties `((python2-variant . ,(delay python2-traitlets))))
    (home-page "https://ipython.org")
    (synopsis "Configuration system for Python applications")
    (description
     "Traitlets is a framework that lets Python classes have attributes with
type checking, dynamically calculated default values, and ‘on change’
callbacks.  The package also includes a mechanism to use traitlets for
configuration, loading values from files or from command line arguments.  This
is a distinct layer on top of traitlets, so you can use traitlets in your code
without using the configuration machinery.")
    (license license:bsd-3)))

(define-public python2-traitlets
  (let ((traitlets (package-with-python2 (strip-python2-variant python-traitlets))))
    (package/inherit traitlets
      (propagated-inputs
       `(("python2-enum34" ,python2-enum34)
         ,@(package-propagated-inputs traitlets))))))

(define-public python-nbformat
  (package
    (name "python-nbformat")
    (version "5.1.3")
    ;; The PyPi release tarball lacks some test cases and test data.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyter/nbformat.git")
                    (commit version)))
              (sha256
               (base32
                "033v16cfmxzh3jn5phnil4p3silr49iwh9wiigzhv0crc6sanvwz"))
              (file-name (git-file-name name version))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (propagated-inputs
     `(("python-ipython-genutils" ,python-ipython-genutils)
       ("python-jsonschema" ,python-jsonschema)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-fastjsonschema" ,python-fastjsonschema) ; This is only active
       ; when setting NBFORMAT_VALIDATOR="fastjsonschema", so include it for
       ; testing only.
       ("python-testpath" ,python-testpath)))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter Notebook format")
    (description "This package provides the reference implementation of the
Jupyter Notebook format and Python APIs for working with notebooks.")
    (license license:bsd-3)))

(define-public python-nbconvert
  (package
    (name "python-nbconvert")
    (version "6.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbconvert" version))
       (sha256
        (base32
         "00lhqaxn481qvk2w5568asqlsnvrw2fm61p1vssx3m7vdnl17g6b"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths-and-tests
           (lambda _
             ;; Use pandoc binary from input.
             (substitute* "nbconvert/utils/pandoc.py"
               (("'pandoc'") (string-append "'" (which "pandoc") "'")))
             ;; Same for LaTeX.
             (substitute* "nbconvert/exporters/pdf.py"
               (("\"xelatex\"") (string-append "\"" (which "xelatex") "\""))
               (("\"bibtex\"") (string-append "\"" (which "bibtex") "\"")))
             ;; Make sure tests are not skipped.
             (substitute* (find-files "." "test_.+\\.py$")
               (("@onlyif_cmds_exist\\(('(pandoc|xelatex)'(, )?)+\\)") ""))
             ;; Pandoc is never missing, disable test.
             (substitute* "nbconvert/utils/tests/test_pandoc.py"
               (("import os" all) (string-append all "\nimport pytest"))
               (("(.+)(def test_pandoc_available)" all indent def)
                (string-append indent "@pytest.mark.skip('disabled by guix')\n"
                               indent def)))
             ;; Not installing pyppeteer, delete test.
             (delete-file "nbconvert/exporters/tests/test_webpdf.py")
             (substitute* "nbconvert/tests/test_nbconvertapp.py"
               (("(.+)(def test_webpdf_with_chromium)" all indent def)
                (string-append indent "@pytest.mark.skip('disabled by guix')\n"
                               indent def)))))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               ;; Some tests invoke the installed nbconvert binary.
               (add-installed-pythonpath inputs outputs)
               ;; Tries to write to this path.
               (unsetenv "JUPYTER_CONFIG_DIR")
               ;; Tests depend on templates installed to output.
               (setenv "JUPYTER_PATH"
                       (string-append
                        (assoc-ref outputs "out")
                        "/share/jupyter:"
                        (getenv "JUPYTER_PATH")))
               ;; Some tests need HOME
               (setenv "HOME" "/tmp")
               (invoke "pytest" "-vv")))))))
    (inputs
     `(("pandoc" ,pandoc)
       ;; XXX: Disabled, needs substitute*.
       ;;("inkscape" ,inkscape)
       ))
    (native-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ;; XXX: Disabled, not in guix.
       ;;("python-pyppeteer" ,python-pyppeteer)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-dependency" ,python-pytest-dependency)
       ("texlive" ,(texlive-union (list texlive-adjustbox
                                        texlive-amsfonts/patched
                                        texlive-booktabs
                                        texlive-caption
                                        texlive-eurosym
                                        texlive-fonts-rsfs
                                        texlive-generic-ulem
                                        texlive-iftex
                                        texlive-jknappen
                                        texlive-latex-amsmath
                                        texlive-latex-enumitem
                                        texlive-latex-fancyvrb
                                        texlive-latex-float
                                        texlive-latex-fontspec
                                        texlive-latex-geometry
                                        texlive-latex-hyperref
                                        texlive-latex-jknapltx
                                        texlive-latex-ms
                                        texlive-latex-oberdiek
                                        texlive-latex-parskip
                                        texlive-latex-trimspaces
                                        texlive-latex-upquote
                                        texlive-latex-ucs
                                        texlive-lm
                                        texlive-mathpazo
                                        texlive-tcolorbox
                                        texlive-titling
                                        texlive-tools
                                        texlive-xcolor
                                        texlive-zapfding)))))
    (propagated-inputs
     `(("python-bleach" ,python-bleach)
       ("python-defusedxml" ,python-defusedxml)
       ("python-entrypoints" ,python-entrypoints)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-mistune" ,python-mistune)
       ("python-nbclient" ,python-nbclient)
       ("python-nbformat" ,python-nbformat)
       ("python-pandocfilters" ,python-pandocfilters)
       ("python-pygments" ,python-pygments)
       ("python-jupyterlab-pygments" ,python-jupyterlab-pygments)
       ("python-testpath" ,python-testpath)
       ("python-traitlets" ,python-traitlets)
       ;; Required, even if [serve] is not used.
       ("python-tornado" ,python-tornado-6)))
    (home-page "https://jupyter.org")
    (synopsis "Converting Jupyter Notebooks")
    (description "The @code{nbconvert} tool, @{jupyter nbconvert}, converts
notebooks to various other formats via Jinja templates.  It allows you to
convert an @code{.ipynb} notebook file into various static formats including:

@enumerate
@item HTML
@item LaTeX
@item PDF
@item Reveal JS
@item Markdown (md)
@item ReStructured Text (rst)
@item executable script
@end enumerate\n")
    (license license:bsd-3)))

(define-public python-notebook
  (package
    (name "python-notebook")
    (version "6.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "notebook" version))
              (sha256
               (base32
                "0zfwr87ndjzmdp9adpc9lby1hdqdkjp2q7c9vff3wiw1dj6kkjfb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             ;; These tests require a browser
             (delete-file-recursively "notebook/tests/selenium")
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; Some tests do not expect all files to be installed in the
               ;; same directory, but JUPYTER_PATH contains multiple entries.
               (unsetenv "JUPYTER_PATH")
               ;; Some tests need HOME
               (setenv "HOME" "/tmp")
               (with-directory-excursion "/tmp"
                 (invoke "pytest" "-vv"
                         ;; TODO: This tests fails because nbconvert does not
                         ;; list "python" as a format.
                         "-k" "not test_list_formats"))))))))
    (propagated-inputs
     `(("python-argon2-cffi" ,python-argon2-cffi)
       ("python-ipykernel" ,python-ipykernel)
       ("python-ipython-genutils" ,python-ipython-genutils)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-nbconvert" ,python-nbconvert)
       ("python-nbformat" ,python-nbformat)
       ("python-prometheus-client" ,python-prometheus-client)
       ("python-pyzmq" ,python-pyzmq)
       ("python-send2trash" ,python-send2trash)
       ("python-terminado" ,python-terminado)
       ("python-tornado" ,python-tornado-6)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nbval" ,python-nbval)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-requests" ,python-requests)
       ("python-requests-unixsocket" ,python-requests-unixsocket)))
    (home-page "https://jupyter.org/")
    (synopsis "Web-based notebook environment for interactive computing")
    (description
     "The Jupyter HTML notebook is a web-based notebook environment for
interactive computing.")
    (license license:bsd-3)))

(define-public python-widgetsnbextension
  (package
    (name "python-widgetsnbextension")
    (version "3.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "widgetsnbextension" version))
       (sha256
        (base32
         "1ismyaxbv9d56yqqqb8xl58hg0iq0bbyy014a53y1g3hfbc8g7q7"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-notebook" ,python-notebook)))
    (home-page "https://ipython.org")
    (synopsis "IPython HTML widgets for Jupyter")
    (description "This package provides interactive HTML widgets for Jupyter
notebooks.")
    (license license:bsd-3)))

(define-public python-ipywidgets
  (package
    (name "python-ipywidgets")
    (version "7.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipywidgets" version))
       (sha256
        (base32
         "1w217j8i53x14l7b05fk300k222zs9vkcjaa1rbrw3sk43k466lz"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-jupyterlab-widgets" ,python-jupyterlab-widgets)
       ("python-nbformat" ,python-nbformat)
       ("python-traitlets" ,python-traitlets)
       ("python-widgetsnbextension" ,python-widgetsnbextension)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://ipython.org")
    (synopsis "IPython HTML widgets for Jupyter")
    (description "Ipywidgets are interactive HTML widgets for Jupyter
notebooks and the IPython kernel.  Notebooks come alive when interactive
widgets are used.  Users gain control of their data and can visualize changes
in the data.")
    (license license:bsd-3)))

(define-public python-jupyter-console
  (package
    (name "python-jupyter-console")
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_console" version))
       (sha256
        (base32
         "06s3kr5vx0l1y1b7fxb04dmrppscl7q69sl9yyfr0d057d1ssvkg"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-prompt-toolkit" ,python-prompt-toolkit-2)
       ("python-pygments" ,python-pygments)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter terminal console")
    (description "This package provides a terminal-based console frontend for
Jupyter kernels.  It also allows for console-based interaction with non-Python
Jupyter kernels such as IJulia and IRKernel.")
    (license license:bsd-3)))

;; The python-ipython and python-jupyter-console require each other. To get
;; the functionality in both packages working, strip down the
;; python-jupyter-console package when using it as an input to python-ipython.
(define python-jupyter-console-minimal
  (package/inherit python-jupyter-console
    (name "python-jupyter-console-minimal")
    (arguments
     (substitute-keyword-arguments
         (package-arguments python-jupyter-console)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'delete-bin
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Delete the bin files, to avoid conflicts in profiles
               ;; where python-ipython and python-jupyter-console are
               ;; both present.
               (delete-file-recursively
                (string-append
                 (assoc-ref outputs "out") "/bin"))))))))
    ;; Remove the python-ipython propagated input, to avoid the cycle
    (propagated-inputs
     (alist-delete
      "python-ipython"
      (package-propagated-inputs python-jupyter-console)))))

(define-public python-qtconsole
  (package
    (name "python-qtconsole")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qtconsole" version))
       (sha256
        (base32
         "1b03n1ixzscm0jw97l4dq5iy4fslnqxq5bb8287xb7n2a1gs26xw"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: Tests are disabled, because this package needs python-ipython 7,
     ;; but we only have the LTS version 5.x.  This means that there might be
     ;; runtime errors, but since this is a dependency of the Jupyter package,
     ;; and Jupyter can be used without the qtconsole we can overlook this for
     ;; now.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter Qt console")
    (description "This package provides a Qt-based console for Jupyter with
support for rich media output.")
    (license license:bsd-3)))

(define-public jupyter
  (package
    (name "jupyter")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter" version))
       (sha256
        (base32
         "0pwf3pminkzyzgx5kcplvvbvwrrzd3baa7lmh96f647k30rlpp6r"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; there are none.
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-ipywidgets" ,python-ipywidgets)
       ("python-jupyter-console" ,python-jupyter-console)
       ("python-nbconvert" ,python-nbconvert)
       ("python-notebook" ,python-notebook)
       ("python-qtconsole" ,python-qtconsole)))
    (home-page "https://jupyter.org")
    (synopsis "Web application for interactive documents")
    (description
     "The Jupyter Notebook is a web application that allows you to create and
share documents that contain live code, equations, visualizations and
explanatory text.  Uses include: data cleaning and transformation, numerical
simulation, statistical modeling, machine learning and much more.")
    (license license:bsd-3)))

(define-public python-ipython-genutils
  ;; TODO: This package is retired, check if can be removed, see description.
  (package
    (name "python-ipython-genutils")
    (version "0.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "ipython_genutils" version))
      (sha256
       (base32 "19l2pp1c64ansr89l3cqh19jdi2ixhssdzx0vz4n6r52a6i281is"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no tests
    (home-page "https://ipython.org")
    (synopsis "Vestigial utilities from IPython")
    (description
     "This package provides retired utilities from IPython.  No packages
outside IPython/Jupyter should depend on it.

This package shouldn't exist.  It contains some common utilities shared by
Jupyter and IPython projects during The Big Split.  As soon as possible, those
packages will remove their dependency on this, and this package will go
away.")
    (license license:bsd-3)))

(define-public python-ipykernel
  (package
    (name "python-ipykernel")
    (version "5.5.3")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "ipykernel" version))
      (sha256
       (base32 "02f55cjkp5q64x7ikjxznbxwjpkdmfy237b9kg7dk1pxmzvy90m6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "HOME" "/tmp")
             (invoke "pytest" "-v")
             #t))
         (add-after 'install 'set-python-file-name
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Record the absolute file name of the 'python' executable in
             ;; 'kernel.json'.
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/share/jupyter"
                                           "/kernels/python3/kernel.json")
                 (("\"python\"")
                  (string-append "\"" (which "python") "\"")))
               #t))))))
    (propagated-inputs
     `(("python-ipython" ,python-ipython)
       ("python-tornado" ,python-tornado-6)
       ("python-traitlets" ,python-traitlets)
       ;; imported at runtime during connect
       ("python-jupyter-client" ,python-jupyter-client)))
    (native-inputs
     `(("python-flaky" ,python-flaky)
       ("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest)))
    (home-page "https://ipython.org")
    (synopsis "IPython Kernel for Jupyter")
    (description
     "This package provides the IPython kernel for Jupyter.")
    (license license:bsd-3)))

;; Bootstrap variant of ipykernel, which uses the bootstrap jupyter-client to
;; break the cycle between ipykernel and jupyter-client.
(define-public python-ipykernel-bootstrap
  (let ((parent python-ipykernel))
    (hidden-package
      (package
        (inherit parent)
        (name "python-ipykernel-bootstrap")
        (propagated-inputs
          `(("python-jupyter-client" ,python-jupyter-client-bootstrap)
            ,@(fold alist-delete (package-propagated-inputs parent)
                    '("python-jupyter-client"))))))))

(define-public python-bash-kernel
  (package
   (name "python-bash-kernel")
   (version "0.7.2")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "bash_kernel" version))
            (sha256
             (base32
              "0w0nbr3iqqsgpk83rgd0f5b02462bkyj2n0h6i9dwyc1vpnq9350"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'bash-references
          (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bash_kernel/kernel.py"
               (("\"bash\"")
                (string-append "\"" (assoc-ref inputs "bash") "/bin/bash\""))
               (("\\['bash', ")
                (string-append "['" (assoc-ref inputs "bash") "/bin/bash', ")))
             #t))
        (add-after 'install 'install-kernelspec
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (setenv "HOME" "/tmp")
              (invoke "python" "-m" "bash_kernel.install" "--prefix" out)
              #t))))))
   (inputs
     `(("bash" ,bash)))
   (propagated-inputs
     `(("python-pexpect" ,python-pexpect)
       ("python-ipykernel" ,python-ipykernel)
       ("python-jupyter-client" ,python-jupyter-client)))
   (home-page "https://github.com/takluyver/bash_kernel")
   (synopsis "Jupyter kernel for Bash")
   (description "A bash shell kernel for Jupyter.")
   (license license:expat)))

(define-public python-sparqlkernel
  (package
    (name "python-sparqlkernel")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sparqlkernel" version))
              (sha256
               (base32
                "004v22nyi5cnpxq4fiws89p7i5wcnzv45n3n70axdd6prh6rkapx"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'no-custom-css
           (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "sparqlkernel/install.py"
                (("install_custom_css\\( destd, PKGNAME \\)") ""))
              #t))
         (add-after 'install 'install-kernelspec
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "HOME" "/tmp")
               (add-installed-pythonpath inputs outputs)
               (invoke
                 (string-append out "/bin/jupyter-sparqlkernel")
                 "install"
                 (string-append "--InstallKernelSpec.prefix=" out))
               #t))))))
    (native-inputs
     `(("python-traitlets" ,python-traitlets)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-notebook" ,python-notebook)
       ("python-ipykernel" ,python-ipykernel)
       ("python-html5lib" ,python-html5lib-0.9)))
    (propagated-inputs
     `(("python-sparqlwrapper" ,python-sparqlwrapper)
       ("python-pygments" ,python-pygments)))
    (home-page "https://github.com/paulovn/sparql-kernel")
    (synopsis "Jupyter kernel for SPARQL")
    (description "This module installs a Jupyter kernel for SPARQL.  It allows
sending queries to an SPARQL endpoint and fetching & presenting the results in
a notebook.")
    (license license:bsd-3)))

(define-public python-pari-jupyter
  (package
    (name "python-pari-jupyter")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pari_jupyter" version))
       (sha256
        (base32
         "1yash0p422nnin7z58b99d0p23nx79f5m0mainc9hsjg72jhdhr6"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)))
    (inputs
     `(("pari-gp" ,pari-gp)
       ("readline" ,readline)))
    (arguments
     `(#:tests? #f)) ; no test suite
    (home-page
     "https://github.com/jdemeyer/pari_jupyter")
    (synopsis "A Jupyter kernel for PARI/GP")
    (description "The package provides a PARI/GP kernel for Jupyter.")
    (license license:gpl3+)))

(define-public repo2docker
  (package
    (name "repo2docker")
    (version "2021.03.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyterhub/repo2docker/")
                    (commit "2021.03.0")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18w8rgf7fpf79kx36y2c3xi3d52i41z112l3sz719d8kg0bir16m"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'patch-shebangs 'fix-install-miniforge
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (substitute* (find-files
                                      out "^(install-miniforge|install-nix|\
nix-shell-wrapper|repo2docker-entrypoint)")
                          (("^#!(.*)/bin/bash")
                           "#!/bin/bash"))
                        (substitute* (find-files out "^freeze\\.py$")
                          (("^#!(.*)/bin/python3")
                           "#!/bin/python3\n")))))
                  (add-after 'install 'make-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "doc"))
                             (doc (string-append out "/share/doc/"
                                                 ,name)))
                        (setenv "PYTHONPATH"
                                (string-append (getcwd) ":"
                                               (getenv "PYTHONPATH")))
                        (with-directory-excursion "docs"
                          (invoke  "make" "html")
                          (copy-recursively "build/html"
                                            (string-append doc "/html")))))))))
    (inputs
     `(("python-traitlets" ,python-traitlets)
       ("python-toml" ,python-toml)
       ("python-semver" ,python-semver)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-requests" ,python-requests)
       ("python-json-logger" ,python-json-logger)
       ("python-jinja2" ,python-jinja2)
       ("python-escapism" ,python-escapism)
       ("python-docker" ,python-docker)))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)
       ("python-recommonmark" ,python-recommonmark)
       ("python-sphinxcontrib-autoprogram" ,python-sphinxcontrib-autoprogram)
       ("python-pydata-sphinx-theme" ,python-pydata-sphinx-theme)))
    (home-page "https://repo2docker.readthedocs.io/en/latest/index.html#")
    (synopsis "Generate docker images from repositories")
    (description
     "repo2docker fetches a repository (from GitHub, GitLab, Zenodo, Figshare,
Dataverse installations, a Git repository or a local directory) and builds a
container image in which the code can be executed.  The image build process is
based on the configuration files found in the repository.  repo2docker can be
used to explore a repository locally by building and executing the constructed
image of the repository, or as a means of building images that are pushed to a
Docker registry.")
    (license license:bsd-3)))

(define-public python-ipyparallel
  (package
    (name "python-ipyparallel")
    (version "6.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ipyparallel" version))
        (sha256
         (base32
          "0rf0dbpxf5z82bw8lsjj45r3wdd4wc74anz4wiiaf2rbjqlb1ivn"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; RuntimeError: IO Loop failed to start
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'prepare-for-tests
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-decorator" ,python-decorator)
       ("python-ipykernel" ,python-ipykernel)
       ("python-ipython" ,python-ipython)
       ("python-ipython-genutils" ,python-ipython-genutils)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-pyzmq" ,python-pyzmq)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(("python-ipython" ,python-ipython)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-testpath" ,python-testpath)))
    (home-page "https://ipython.org/")
    (synopsis "Interactive Parallel Computing with IPython")
    (description
     "@code{ipyparallel} is a Python package and collection of CLI scripts for
controlling clusters for Jupyter.  @code{ipyparallel} contains the following
CLI scripts:
@enumerate
@item ipcluster - start/stop a cluster
@item ipcontroller - start a scheduler
@item ipengine - start an engine
@end enumerate")
    (license license:bsd-3)))

(define-public python-ipython-cluster-helper
  (package
    (name "python-ipython-cluster-helper")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ipython-cluster-helper" version))
        (sha256
         (base32
          "1l6mlwxlkxpbvawfwk6qffich7ahg9hq2bxfissgz6144p3k4arj"))
        (modules '((guix build utils)))
        (snippet
         '(begin (substitute* "requirements.txt"
                   (("ipython.*") "ipython\n"))
                 #t))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f      ; Test suite can't find IPython.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (if tests?
               (begin
                 (setenv "HOME" (getcwd))
                 (add-installed-pythonpath inputs outputs)
                 (invoke "python" "example/example.py" "--local"))
               #t))))))
    (propagated-inputs
     `(("python-ipyparallel" ,python-ipyparallel)
       ("python-ipython" ,python-ipython)
       ("python-netifaces" ,python-netifaces)
       ("python-pyzmq" ,python-pyzmq)
       ("python-setuptools" ,python-setuptools)
       ("python-six" ,python-six)))
    (home-page "https://github.com/roryk/ipython-cluster-helper")
    (synopsis
     "Simplify IPython cluster start up and use for multiple schedulers")
    (description
     "@code{ipython-cluster-helper} creates a throwaway parallel IPython
profile, launches a cluster and returns a view.  On program exit it shuts the
cluster down and deletes the throwaway profile.")
    (license license:expat)))

(define-public xeus
  (package
    (name "xeus")
    (version "0.23.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/QuantStack/xeus")
                    (commit version)))
              (sha256
               (base32
                "1m1b6z1538r7mv2ggn7bdbd9570ja7cadplq64zl8rgl2c8vdi2a"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_STATIC_LIBS=OFF"
                           "-DDISABLE_ARCH_NATIVE=ON" ;no '-march=native'
                           "-DBUILD_TESTING=ON")))
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; The following inputs are used by the test suite.
       ("googletest" ,googletest)
       ("python-pytest" ,python-pytest)
       ("python" ,python-3)
       ("python-jupyter-kernel-test" ,python-jupyter-kernel-test)
       ("python-jupyter-client" ,python-jupyter-client)))
    (inputs
     `(("xtl" ,xtl)
       ("json-modern-cxx" ,json-modern-cxx)
       ("cppzmq" ,cppzmq)
       ("zeromq" ,zeromq)
       ("openssl" ,openssl)
       ("util-linux" ,util-linux "lib")))         ;libuuid
    (home-page "https://quantstack.net/xeus")
    (synopsis "C++ implementation of the Jupyter Kernel protocol")
    (description
     "@code{xeus} is a library meant to facilitate the implementation of
kernels for Jupyter.  It takes the burden of implementing the Jupyter Kernel
protocol so developers can focus on implementing the interpreter part of the
kernel.

Several Jupyter kernels are built upon @code{xeus}, such as @code{xeus-cling},
a kernel for the C++ programming language, and @code{xeus-python}, an
alternative Python kernel for Jupyter.")
    (license license:bsd-3)))

