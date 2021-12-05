;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
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

(define-module (gnu packages python-check)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages django)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system python))

(define-public python-tappy
  (package
    (name "python-tappy")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tap.py" version))
       (sha256
        (base32
         "0w4w6pqjkv54j7rv6vdrpfxa72c5516bnlhpcqr3vrb4zpmyxvpm"))))
    (build-system python-build-system)
    (home-page "https://github.com/python-tap/tappy")
    (synopsis "Tools for Test Anything Protocol")
    (description "Tappy is a set of tools for working with the Test Anything
Protocol (TAP) in Python.  TAP is a line based test protocol for recording test
data in a standard way.")
    (license license:bsd-3)))

(define-public python-pytest-click
  (package
    (name "python-pytest-click")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (pypi-uri "pytest_click" version))
       (sha256
        (base32 "1rcv4m850rl7djzdgzz2zhjd8g5ih8w6l0sj2f9hsynymlsq82xl"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/Stranger6667/pytest-click")
    (synopsis "Py.test plugin for Click")
    (description "This package provides a plugin to test Python click
interfaces with pytest.")
    (license license:expat)))

(define-public python-pytest-csv
  (package
    (name "python-pytest-csv")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/nicoulaj/pytest-csv")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17518f2fn5l98lyk9p8r7215c1whi61imzrh6ahrmcksr8w0zz04"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest")))))))
    (native-inputs
     `(("python-pytest-flake8" ,python-pytest-flake8)
       ("python-pytest-xdist" ,python-pytest-xdist-next)
       ("python-tabulate" ,python-tabulate)))
    (propagated-inputs
     `(("python-pytest" ,python-pytest-6)
       ("python-six" ,python-six)))
    (home-page "https://github.com/nicoulaj/pytest-csv")
    (synopsis "CSV reporter for Pytest")
    (description "This package provides a plugin for Pytest that enables a
CSV output mode for Pytest.  It can be enabled via the @option{--csv} option
it adds to the Pytest command line interface (CLI).")
    (license license:gpl3+)))

(define-public python-testfixtures
  (package
    (name "python-testfixtures")
    (version "6.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testfixtures" version))
       (sha256
        (base32 "1nlv2hz20czjp4a811ichl5kwg99rh84l0mw9wq4rk3idzfs1hsy"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))          ; PyTest-Django fails to build in master
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ;;("python-pytest-django" ,python-pytest-django)
       ("python-twine" ,python-twine)
       ("python-wheel" ,python-wheel)))
    (synopsis "Tests components for Python")
    (description "Testfixtures is a collection of helpers and mock objects that
are useful when writing automated tests in Python.")
    (home-page "https://testfixtures.readthedocs.io/en/latest/")
    (license license:expat)))

(define-public python-coveralls
  (package
    (name "python-coveralls")
    (version "3.2.0")
    (home-page "https://github.com/coveralls-clients/coveralls-python")
    (source
     (origin
       ;; The PyPI release lacks tests, so we pull from git instead.
       (method git-fetch)
       (uri (git-reference (url home-page) (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1915ab77nfb1rfw4i2ps0zy19wpf20lwxn81qxxbwyd2gy7m0fn8"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-git-test
                    (lambda _
                      ;; Remove test that requires 'git' and the full checkout.
                      (delete-file "tests/git_test.py")))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "pytest" "-vv")
                          (format #t "test suite not run~%")))))))
    (propagated-inputs
     `(("python-coverage" ,python-coverage)
       ("python-docopt" ,python-docopt)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-responses" ,python-responses)))
    (synopsis "Show coverage stats online via coveralls.io")
    (description
     "Coveralls.io is a service for publishing code coverage statistics online.
This package provides seamless integration with coverage.py (and thus pytest,
nosetests, etc...) in Python projects.")
    (license license:expat)))

(define-public python-junit-xml
  ;; XXX: There are no tags or PyPI releases, so take the latest commit
  ;; and use the version defined in setup.py.
  (let ((version "1.9")
        (commit "4bd08a272f059998cedf9b7779f944d49eba13a6")
        (revision "0"))
    (package
      (name "python-junit-xml")
      (version (git-version version revision commit))
      (home-page "https://github.com/kyrus/python-junit-xml")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b8kbjhk3j10rk0vcniy695m3h43yip6y93h1bd6jjh0cp7s09c7"))))
      (build-system python-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda _
                        (invoke "pytest" "-vv"))))))
      (native-inputs
       `(("python-pytest" ,python-pytest)))
      (propagated-inputs
       `(("python-six" ,python-six)))
      (synopsis "Create JUnit XML test results")
      (description
       "This package provides a Python module for creating JUnit XML test
result documents that can be read by tools such as Jenkins or Bamboo.")
      (license license:expat))))

(define-public python-vcrpy
  (package
    (name "python-vcrpy")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "vcrpy" version))
        (sha256
         (base32
          "0kws7l3hci1dvjv01nxw3805q9v2mwldw58bgl8s90wqism69gjp"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; tests require more packages for python-pytest-httpbin
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)
       ("python-yarl" ,python-yarl)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-httpbin" ,python-pytest-httpbin)))
    (home-page "https://github.com/kevin1024/vcrpy")
    (synopsis "Automatically mock your HTTP interactions")
    (description
     "VCR.py simplifies and speeds up tests that make HTTP requests.  The first
time you run code that is inside a VCR.py context manager or decorated function,
VCR.py records all HTTP interactions that take place through the libraries it
supports and serializes and writes them to a flat file (in yaml format by
default).  This flat file is called a cassette.  When the relevant piece of code
is executed again, VCR.py will read the serialized requests and responses from
the aforementioned cassette file, and intercept any HTTP requests that it
recognizes from the original test run and return the responses that corresponded
to those requests.  This means that the requests will not actually result in
HTTP traffic, which confers several benefits including:
@enumerate
@item The ability to work offline
@item Completely deterministic tests
@item Increased test execution speed
@end enumerate
If the server you are testing against ever changes its API, all you need to do
is delete your existing cassette files, and run your tests again.  VCR.py will
detect the absence of a cassette file and once again record all HTTP
interactions, which will update them to correspond to the new API.")
    (license license:expat)))

(define-public python-pytest-ordering
  (package
    (name "python-pytest-ordering")
    (version "0.6")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ftobia/pytest-ordering")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14msj5gyqza0gk3x7h1ivmjrwza82v84cj7jx3ks0fw9lpin7pjq"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "-k"
                     ;; This test fails because of a type mismatch of an
                     ;; argument passed to @code{pytest.main}.
                     "not test_run_marker_registered"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/ftobia/pytest-ordering")
    (synopsis "Pytest plugin to run your tests in a specific order")
    (description
     "This plugin defines Pytest markers to ensure that some tests, or groups
of tests run in a specific order.")
    (license license:expat)))

(define-public python-pytest-astropy-header
(package
  (name "python-pytest-astropy-header")
  (version "0.1.2")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pytest-astropy-header" version))
      (sha256
        (base32 "1y87agr324p6x5gvhziymxjlw54pyn4gqnd49papbl941djpkp5g"))))
  (build-system python-build-system)
  (native-inputs
   `(("pytest" ,python-pytest)
     ("setuptools-scm" ,python-setuptools-scm)))
  (home-page "https://www.astropy.org/")
  (synopsis
   "Pytest plugin adding diagnostic data to the header of the test output")
  (description
    "This plugin package provides a way to include information about the system,
Python installation, and select dependencies in the header of the output when
running pytest.  It can be used with packages that are not affiliated with the
Astropy project, but is optimized for use with astropy-related projects.")
  (license license:bsd-3)))

(define-public python-pytest-astropy
  (package
    (name "python-pytest-astropy")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-astropy" version))
       (sha256
        (base32 "18j6z6y2fvykmcs5z0mldhhaxxn6wzpnhlm2ps7m8r5z5kmh1631"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         ;; There is a bug somewhere that makes pytest-filter-subpackage appear
         ;; as version 0.0.0 to setup.py.  Remove it from the requirements.
         (add-after 'unpack 'remove-requirement
           (lambda _
             (substitute* "setup.cfg"
               ((".*pytest-filter-subpackage.*") "")))))))
    (native-inputs
     `(("python-attrs" ,python-attrs)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-hypothesis" ,python-hypothesis)
       ("python-pytest-arraydiff" ,python-pytest-arraydiff)
       ("python-pytest-astropy-header" ,python-pytest-astropy-header)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-doctestplus" ,python-pytest-doctestplus)
       ("python-pytest-filter-subpackage" ,python-pytest-filter-subpackage)
       ("python-pytest-openfiles" ,python-pytest-openfiles)
       ("python-pytest-remotedata" ,python-pytest-remotedata)))
    (home-page "https://github.com/astropy/pytest-astropy")
    (synopsis
     "Metapackage for all the testing machinery used by the Astropy Project")
    (description
     "This is a meta-package that pulls in the dependencies that are used by
astropy related packages.")
    (license license:bsd-3)))

(define-public python-pytest-arraydiff
  (package
    (name "python-pytest-arraydiff")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-arraydiff" version))
       (sha256
        (base32 "05bcvhh2ycxa35znl8b3l9vkcmx7vwm5c3fpakbpw46c7vsn4bfy"))))
    (build-system python-build-system)
    (arguments
     ;; Tests require python-astropy, which itself requires this package.
     ;; Disable tests to avoid the circular dependency problem.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-six" ,python-six)))
    (home-page "https://github.com/astropy/pytest-arraydiff")
    (synopsis "Pytest plugin to help with comparing array output from tests")
    (description
     "This is a py.test plugin to facilitate the generation and comparison of
data arrays produced during tests, in particular in cases where the arrays
are too large to conveniently hard-code them in the tests.")
    (license license:bsd-3)))

(define-public python-pytest-doctestplus
  (package
    (name "python-pytest-doctestplus")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-doctestplus" version))
       (sha256
        (base32 "1ai9kvd7xbq2jg2h8gmkb8lqzyrxvdh4zg3vxndg149iwd1hyi7d"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/astropy/pytest-doctestplus")
    (synopsis "Pytest plugin with advanced doctest features")
    (description
     "This package contains a plugin for the Pytest framework that provides
advanced doctest support and enables the testing of reStructuredText files.")
    (license license:bsd-3)))

(define-public python-pytest-exploratory
  (package
    (name "python-pytest-exploratory")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_exploratory" version))
       (sha256
        (base32 "159rcqv6wrdqdlag1gz39n6fk58232hbxshan043ljgpp1qfs6xk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "tests")))))))
    (propagated-inputs
     `(("python-ipython" ,python-ipython)
       ("python-py" ,python-py)
       ("python-pytest" ,python-pytest)))
    (native-inputs `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/nokia/pytest-exploratory")
    (synopsis "Interactive console for Pytest")
    (description "This Pytest plugin provides an IPython extension that allows
for interactively selecting and running Pytest tests.")
    (license license:expat)))

(define-public python-pytest-filter-subpackage
  (package
    (name "python-pytest-filter-subpackage")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-filter-subpackage" version))
       (sha256
        (base32 "1s4s2kd31yc65rfvl4xhy8xx806xhy59kc7668h6b6wq88xgrn5p"))))
    (build-system python-build-system)
    (arguments
     '(;; One test is failing. There's an issue reported upstream. See
       ;; https://github.com/astropy/pytest-filter-subpackage/issues/3.
       ;; Disable it for now.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "-k" "not test_with_rst"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-doctestplus"
        ,python-pytest-doctestplus)))
    (home-page "https://github.com/astropy/pytest-filter-subpackage")
    (synopsis "Pytest plugin for filtering based on sub-packages")
    (description
     "This package contains a simple plugin for the pytest framework that
provides a shortcut to testing all code and documentation for a given
sub-package.")
    (license license:bsd-3)))

(define-public python-pytest-helpers-namespace
  (package
    (name "python-pytest-helpers-namespace")
    (version "2021.3.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-helpers-namespace" version))
       (sha256
        (base32
         "0pyj2d45zagmzlajzqdnkw5yz8k49pkihbydsqkzm413qnkzb38q"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools" ,python-setuptools) ; needs setuptools >= 50.3.2
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-setuptools-declarative-requirements"
        ,python-setuptools-declarative-requirements)))
    (home-page "https://github.com/saltstack/pytest-helpers-namespace")
    (synopsis "Pytest Helpers Namespace Plugin")
    (description "Pytest Helpers Namespace Plugin provides a helpers pytest
namespace which can be used to register helper functions without requiring
someone to import them in their actual tests to use them.")
    (license license:asl2.0)))

(define-public python-pytest-openfiles
  (package
    (name "python-pytest-openfiles")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-openfiles" version))
       (sha256
        (base32 "0n0a7fdc9m86360y96l23fvdmd6rw04bl6h5xqgl9qxfv08jk70p"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-psutil" ,python-psutil)))
    (home-page "https://github.com/astropy/pytest-openfiles")
    (synopsis "Pytest plugin for detecting inadvertent open file handles")
    (description
     "This package provides a plugin for the pytest framework that allows
developers to detect whether any file handles or other file-like objects
were inadvertently left open at the end of a unit test.")
    (license license:bsd-3)))

(define-public python-pytest-remotedata
  (package
    (name "python-pytest-remotedata")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-remotedata" version))
       (sha256
        (base32 "1h6g6shib6z07azf12rnsa053470ggbd7hy3bnbw8nf3nza5h372"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; XXX: Hang. Needs fix.
       #:test-flags '("-vvv" "-k"
                      "not test_default_behavior and not test_strict_with_decorator")))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/astropy/pytest-remotedata")
    (synopsis "Pytest plugin for controlling remote data access")
    (description
     "This package provides a plugin for the Pytest framework that allows
developers to control unit tests that require access to data from the
internet.")
    (license license:bsd-3)))

(define-public python-pytest-repeat
  (package
    (name "python-pytest-repeat")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-repeat" version))
       (sha256
        (base32 "0nxdbghjz6v4xidl5ky9wlx6z4has3vygj5r7va5ccdb8nbjilsw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/pytest-dev/pytest-repeat")
    (synopsis "Pytest plugin for repeating tests")
    (description "@code{pytest-repeat} is a plugin for Pytest that makes it
enables repeating a single test, or multiple tests, a specific number of
times.")
    (license license:mpl2.0)))

(define-public python-pytest-mockito
  (package
    (name "python-pytest-mockito")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/kaste/pytest-mockito")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hnpazaw3mglx1c405z2hkavgan99rqb3wgrcqk8x5kmhpay53xx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "-vv")))))))
    (propagated-inputs
     `(("python-mockito" ,python-mockito)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/kaste/pytest-mockito")
    (synopsis "Mockito base fixtures for Pytest")
    (description "The @code{pytest-mockito} plugin provides base Mockito
fixtures for Pytest.  It covers the main entry points of the Mockito mocking
framework and makes it easy to undo any monkey patching.  The fixtures are:
@itemize
@item when
@item when2
@item expect
@item patch
@item unstub
@item spy2
@end itemize")
    (license license:expat)))

(define-public python-pytest-mpl
  (package
    (name "python-pytest-mpl")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mpl" version))
       (sha256
        (base32 "1km202c1s5kcn52fx0266p06qb34va3warcby594dh6vixxa9i96"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-nose" ,python-nose)
       ("python-pillow" ,python-pillow)))
    (home-page "https://github.com/matplotlib/pytest-mpl")
    (synopsis "Pytest plugin to help with testing figures output from Matplotlib")
    (description
     "This is a plugin to facilitate image comparison for Matplotlib figures
in Pytest.")
    (license license:bsd-3)))

(define-public python-covdefaults
  (package
    (name "python-covdefaults")
    (version "1.1.0")
    (source
     (origin
       ;; The PyPI tarball does not include tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/covdefaults")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11a24c0wzv01n55fy4kdpnyqna4m9k0mp58kmhiaks34xw4k37hq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/asottile/covdefaults")
    (synopsis "Coverage plugin to provide opinionated default settings")
    (description
     "Covdefaults is a coverage plugin to provide opinionated default
 settings.")
    (license license:expat)))

(define-public python-pytest-vcr
  ;; This commit fixes integration with pytest-5
  (let ((commit "4d6c7b3e379a6a7cba0b8f9d20b704dc976e9f05")
        (revision "1"))
    (package
      (name "python-pytest-vcr")
      (version (git-version "1.0.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ktosiek/pytest-vcr")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1yk988zi0la6zpcm3fff0mxf942di2jiymrfqas19nyngj5ygaqs"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests/"))))))
      (propagated-inputs
       `(("python-pytest" ,python-pytest)
         ("python-vcrpy" ,python-vcrpy)))
      (home-page "https://github.com/ktosiek/pytest-vcr")
      (synopsis "Plugin for managing VCR.py cassettes")
      (description
       "Plugin for managing VCR.py cassettes.")
      (license license:expat))))

(define-public python-pytest-checkdocs
  (package
    (name "python-pytest-checkdocs")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-checkdocs" version))
       (sha256
        (base32 "0m4kn7141i6k8qr8ak3lbmk9vim11xsrlnrggcfwczfrglc6jmia"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)
       ("python-importlib-metadata" ,python-importlib-metadata)
       ("python-more-itertools" ,python-more-itertools)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/jaraco/pytest-checkdocs")
    (synopsis "Check the README when running tests")
    (description
     "This package provides a pytest plugin that checks the long description
of the project to ensure it renders properly.")
    (license license:expat)))

(define-public python-re-assert
  (package
    (name "python-re-assert")
    (version "1.1.0")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/re-assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rssq4wpqmx1c17hjfx5l3sn3zmnlz9jffddiqrs4f6h7m6cadai"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-covdefaults" ,python-covdefaults)
       ("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-regex" ,python-regex)))
    (home-page "https://github.com/asottile/re-assert")
    (synopsis "Show where your regex match assertion failed")
    (description
     "@code{re-assert} provides a helper class to make assertions of regexes
simpler.")
    (license license:expat)))

(define-public python-pytest-trio
  (package
    (name "python-pytest-trio")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-trio" version))
       (sha256
        (base32 "0c8cqf9by2884riksrqymqfp2g1d2d798a2zalcw9hmf34c786y0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-W" "error" "-ra" "-v" "--pyargs"
                       "pytest_trio" "--verbose" "--cov" "-k"
                       (string-append
                         ;; Needs network
                         "not test_async_yield_fixture_with_nursery"
                         " and not test_try"
                         ;; No keyboard interrupt in our build environment.
                         " and not test_actual_test"))))))))
    (native-inputs
     `(("python-hypothesis" ,python-hypothesis)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (propagated-inputs
      (list python-async-generator python-outcome python-pytest python-trio))
    (home-page "https://github.com/python-trio/pytest-trio")
    (synopsis "Pytest plugin for trio")
    (description
     "This is a pytest plugin to help you test projects that use Trio, a
friendly library for concurrency and async I/O in Python.")
    ;; Either license applies.
    (license (list license:expat license:asl2.0))))

(define-public python-pytest-flake8
  (package
    (name "python-pytest-flake8")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flake8" version))
       (sha256
        (base32
         "0syx68xk5ss3hgp3nr2y122w0fgkzr5936ghsqrkymh3m5hrf9gh"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-flake8" ,python-flake8)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/tholo/pytest-flake8")
    (synopsis "Pytest plugin to check FLAKE8 requirements")
    (description
     "This package provides a pytest plugin for efficiently checking PEP8
compliance.")
    (license license:bsd-3)))

(define-public python-pytest-isort
  (package
    (name "python-pytest-isort")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-isort" version))
       (sha256
        (base32 "05wi28zlqk3jafpjal8j523y5jcsx3xl3id9rx93qfjgkif8q6l2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (propagated-inputs
     `(("python-isort" ,python-isort)
       ("python-pytest" ,python-pytest)))
    (native-inputs
     `(("python-mock" ,python-mock)))
    (home-page "https://github.com/moccu/pytest-isort/")
    (synopsis "Pytest plugin to check import ordering using isort")
    (description
     "This package provides a pytest plugin to check import ordering using
isort.")
    (license license:bsd-3)))

(define-public python-pytest-shutil
  (package
    (name "python-pytest-shutil")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-shutil" version))
       (sha256
        (base32
         "0q8j0ayzmnvlraml6i977ybdq4xi096djhf30n2m1rvnvrhm45nq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (mkdir "/tmp/bin")
             (substitute* "tests/integration/test_cmdline_integration.py"
               (("dirname = '/bin'")
                "dirname = '/tmp/bin'")
               (("bindir = os.path.realpath\\('/bin'\\)")
                "bindir = os.path.realpath('/tmp/bin')"))
             #t)))))
    (propagated-inputs
     `(("python-contextlib2" ,python-contextlib2)
       ("python-execnet" ,python-execnet)
       ("python-pathpy" ,python-pathpy)
       ("python-termcolor" ,python-termcolor)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-git" ,python-setuptools-git)))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Assorted shell and environment tools for py.test")
    (description
     "This package provides assorted shell and environment tools for the
py.test testing framework.")
    (license license:expat)))

(define-public python-pytest-fixture-config
  (package
    (name "python-pytest-fixture-config")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-fixture-config" version))
       (sha256
        (base32
         "13i1qpz22w3x4dmw8vih5jdnbqfqvl7jiqs0dg764s0zf8bp98a1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-git" ,python-setuptools-git)))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Fixture configuration utils for py.test")
    (description
     "This package provides fixture configuration utilities for the py.test
testing framework.")
    (license license:expat)))

(define-public python-pytest-virtualenv
  (package
    (name "python-pytest-virtualenv")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-virtualenv" version))
       (sha256
        (base32
         "03w2zz3crblj1p6i8nq17946hbn3zqp9z7cfnifw47hi4a4fww12"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Reference the virtualenv executable directly, to avoid the need
         ;; for PYTHONPATH, which gets cleared when instantiating a new
         ;; virtualenv with pytest-virtualenv.
         (add-after 'unpack 'patch-virtualenv-executable
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((virtualenv (assoc-ref inputs "python-virtualenv"))
                    (virtualenv-bin (string-append virtualenv
                                                   "/bin/virtualenv")))
               (substitute* "pytest_virtualenv.py"
                 (("^DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE.*$")
                  (format #f "DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE = '~a'"
                          virtualenv-bin)))
               #t))))))
    (propagated-inputs
     `(("python-pytest-shutil" ,python-pytest-shutil)
       ("python-pytest-fixture-config" ,python-pytest-fixture-config)))
    (inputs
     `(("python-virtualenv" ,python-virtualenv)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-git" ,python-setuptools-git)))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Virtualenv fixture for py.test")
    (description "This package provides a virtualenv fixture for the py.test
framework.")
    (license license:expat)))

(define-public python-pytest-pycodestyle
  (package
    (name "python-pytest-pycodestyle")
    (version "2.0.0")               ;later versions require python-pytest~=5.4
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-pycodestyle" version))
       (sha256
        (base32
         "02i5gl7pm9cwxk15sn29inz3n8flpj1r3p1l110h43f2na5w8h7z"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pycodestyle" ,python-pycodestyle)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/henry0312/pytest-pycodestyle")
    (synopsis "Pytest plugin to run pycodestyle")
    (description "This package provides a plugin to run @code{pycodestyle}
for the @code{pytest} framework.")
    (license license:expat)))

(define-public python-pytest-benchmark
  (package
    (name "python-pytest-benchmark")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-benchmark" version))
       (sha256
        (base32
         "0a4mpb4j73dsyk47hd1prrjpfk4r458s102cn80rf253jg818hxd"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; XXX: Fail, investigate.
    (propagated-inputs
     `(("python-py-cpuinfo" ,python-py-cpuinfo)))
    (native-inputs
     `(("python-pathlib2" ,python-pathlib2)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/ionelmc/pytest-benchmark")
    (synopsis "Pytest fixture for benchmarking code")
    (description
     "This package provides a pytest fixture that will group the tests into
rounds that are calibrated to the chosen timer.")
    (license license:bsd-2)))

(define-public python-pytest-xvfb
  (package
    (name "python-pytest-xvfb")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xvfb" version))
       (sha256
        (base32 "1kyq5rg27dsnj7dc6x9y7r8vwf8rc88y2ppnnw6r96alw0nn9fn4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-tests
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")

             ;; This test is meant to run on Windows.
             (delete-file "tests/test_xvfb_windows.py")
             #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     `(("python-pyvirtualdisplay"
        ,python-pyvirtualdisplay)))
    (home-page "https://github.com/The-Compiler/pytest-xvfb")
    (synopsis "Pytest plugin to run Xvfb for tests")
    (description
     "This package provides a Pytest plugin to run Xvfb for tests.")
    (license license:expat)))

(define-public python-pytest-services
  (package
    (name "python-pytest-services")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-services" version))
        (sha256
         (base32
          "0b2zfv04w6m3gp2v44ifdhx22vcji069qnn95ry3zcyxib7cjnq3"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests not included in release tarball.
    (propagated-inputs
     `(("python-psutil" ,python-psutil)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/pytest-dev/pytest-services")
    (synopsis "Services plugin for pytest testing framework")
    (description
     "This plugin provides a set of fixtures and utility functions to start
service processes for your tests with pytest.")
    (license license:expat)))

(define-public python-pytest-toolbox
  (package
    (name "python-pytest-toolbox")
    (version "0.4")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/samuelcolvin/pytest-toolbox")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wqkr3g5gmqdxmhzfsxbwy8pm3cadaj6a8cxq58w9bacly4hqbh0"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (substitute* "setup.cfg"
                 ((".*timeout.*") ""))
               ;; Make the installed plugin discoverable by Pytest.
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv")))))))
    (native-inputs
     `(("python-pydantic" ,python-pydantic)
       ("python-pytest" ,python-pytest)
       ("python-pytest-isort" ,python-pytest-isort)))
    (home-page "https://github.com/samuelcolvin/pytest-toolbox")
    (synopsis "Numerous useful plugins for Pytest")
    (description
     "Pytest Toolbox contains many useful plugins for Pytest.  Among them are
new fixtures, new methods and new comparison objects.")
    (license license:expat)))

(define-public python-pytest-aiohttp
  (package
    (name "python-pytest-aiohttp")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-aiohttp" version))
       (sha256
        (base32
         "0kx4mbs9bflycd8x9af0idcjhdgnzri3nw1qb0vpfyb3751qaaf9"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-aiohttp" ,python-aiohttp)))
    (home-page "https://github.com/aio-libs/pytest-aiohttp/")
    (synopsis "Pytest plugin for aiohttp support")
    (description "This package provides a pytest plugin for aiohttp support.")
    (license license:asl2.0)))

(define-public python-nbval
  (package
    (name "python-nbval")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbval" version))
       (sha256
        (base32 "0h3xrnw0mj1srigrx2rfnd73h8s0xjycclmjs0vx7qkfyqpcvvyg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test
           (lambda _
             ;; This test fails because of a mismatch in the output of LaTeX
             ;; equation environments.  Seems OK to skip.
             (delete-file "tests/ipynb-test-samples/test-latex-pass-correctouput.ipynb")
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "-k"
                     (string-append
                     ;; This only works with Pytest < 5.
                      "not nbdime_reporter"
                     ;; https://github.com/computationalmodelling/nbval/pull/148.
                      " and not test_timeouts")))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-sympy" ,python-sympy)))
    (propagated-inputs
     `(("python-ipykernel" ,python-ipykernel)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-nbformat" ,python-nbformat)
       ("python-six" ,python-six)))
    (home-page "https://github.com/computationalmodelling/nbval")
    (synopsis "Pytest plugin to validate Jupyter notebooks")
    (description
     "This plugin adds functionality to Pytest to recognise and collect Jupyter
notebooks.  The intended purpose of the tests is to determine whether execution
of the stored inputs match the stored outputs of the @file{.ipynb} file.  Whilst
also ensuring that the notebooks are running without errors.")
    (license license:bsd-3)))

(define-public python-pytest-flask
  (package
    (name "python-pytest-flask")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flask" version))
       (sha256
        (base32
         "1hln7mwgdzfi5ma0kqfsi768l7p24jhkw8l0imhifwy08nh7hmjd"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-flask" ,python-flask)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-werkzeug" ,python-werkzeug)))
    (home-page "https://github.com/pytest-dev/pytest-flask")
    (synopsis "Pytest fixtures to test Flask applications")
    (description
     "This pytest plugin provides fixtures to simplify Flask app testing.")
    (license license:expat)))

(define-public python-pytest-console-scripts
  (package
    (name "python-pytest-console-scripts")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-console-scripts" version))
       (sha256
        (base32
         "073l2cz11013dl30zjr575ms78j9b2bsbdl1w0gmig37spbkh8aa"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "--verbose"
                       ;; This one test fails because of PATH assumptions
                       "-k" "not test_elsewhere_in_the_path")))))))
    (propagated-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/kvas-it/pytest-console-scripts")
    (synopsis "Pytest plugin for testing console scripts")
    (description
     "This package provides a pytest plugin for testing console scripts.")
    (license license:expat)))

(define-public python-pytest-tornasync
  (package
    (name "python-pytest-tornasync")
    (version "0.6.0.post2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-tornasync" version))
       (sha256
        (base32
         "0pdyddbzppkfqwa7g17sdfl4w2v1hgsky78l8f4c1rx2a7cvd0fp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false ; TODO: fails at "from test import MESSAGE, PAUSE_TIME"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "--verbose")))))))
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-tornado" ,python-tornado)))
    (home-page "https://github.com/eukaryote/pytest-tornasync")
    (synopsis "Pytest plugin for testing Tornado code")
    (description
     "This package provides a simple pytest plugin that provides some helpful
fixtures for testing Tornado (version 5.0 or newer) apps and easy handling of
plain (undecoratored) native coroutine tests.")
    (license license:expat)))

(define-public python-pytest-env
  (package
    (name "python-pytest-env")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-env" version))
       (sha256
        (base32 "1hl0ln0cicdid4qjk7mv90lw9xkb0v71dlj7q7rn89vzxxm9b53y"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/MobileDynasty/pytest-env")
    (synopsis "Pytest plugin that allows you to add environment variables")
    (description
     "This is a @code{py.test} plugin that enables you to set environment
variables in the @file{pytest.ini} file.")
    (license license:expat)))

(define-public python-pyux
  (package
    (name "python-pyux")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyux" version))
       (sha256
        (base32
         "1i17xh4dy238ibrjdgh8vn78fk5q6dj37mcznpvdfzidj57js7ca"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                  ;the mini test suite fails
    (home-page "https://github.com/farizrahman4u/pyux")
    (synopsis "Utility to check API integrity in Python libraries")
    (description "The pyux utility detects API changes in Python
libraries.")
    (license license:expat)))

(define-public python-pytest-qt
  (package
    (name "python-pytest-qt")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-qt" version))
       (sha256
        (base32 "09c9psfn3zigpaw1l1cmynpa3csxa49wc2ih5lzl24skdkw0njvi"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-qpa
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     `(("python-pyqt" ,python-pyqt)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/pytest-dev/pytest-qt")
    (synopsis "Pytest support for PyQt and PySide applications")
    (description
     "@code{pytest-qt} is a Pytest plugin that allows programmers to write
tests for PyQt5 and PySide2 applications.

The main usage is to use the @code{qtbot} fixture, responsible for handling
@code{qApp} creation as needed and provides methods to simulate user
interaction, like key presses and mouse clicks.")
    (license license:expat)))

(define-public python-codacy-coverage
  (package
    (name "python-codacy-coverage")
    (version "1.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codacy-coverage" version))
        (sha256
         (base32
          "1g0c0w56xdkmqb8slacyw5qhzrkp814ng3ddh2lkiij58y9m2imr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     `(("python-check-manifest" ,python-check-manifest)
       ("python-requests" ,python-requests)))
    (home-page "https://github.com/codacy/python-codacy-coverage")
    (synopsis "Codacy coverage reporter for Python")
    (description "This package analyses Python test suites and reports how much
of the code is covered by them.  This tool is part of the Codacy suite for
analysing code quality.")
    (license license:expat)))

(define-public python-httmock
  (package
    (name "python-httmock")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "httmock" version))
        (sha256
         (base32
          "1zj1fcm0n6f0wr9mr0hmlqz9430fnr5cdwd5jkcvq9j44bnsrfz0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page "https://github.com/patrys/httmock")
    (synopsis "Mocking library for requests.")
    (description "This package provides a library for replying fake data to
Python software under test, when they make an HTTP query.")
    (license license:asl2.0)))

(define-public python-atpublic
  (package
    (name "python-atpublic")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "atpublic" version))
        (sha256
         (base32
          "0i3sbxkdlbb4560rrlmwwd5y4ps7k73lp4d8wnmd7ag9k426gjkx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'enable-c-implementation
           (lambda _
             (setenv "ATPUBLIC_BUILD_EXTENSION" "yes")
             #t))
         (replace 'check
           (lambda _
             (invoke "python" "-m" "nose2" "-v"))))))
    (native-inputs
     `(("python-nose2" ,python-nose2)))
    (home-page "https://public.readthedocs.io/")
    (synopsis "@code{@@public} decorator for populating @code{__all__}")
    (description
     "This Python module adds a @code{@@public} decorator and function which
populates a module's @code{__all__} and optionally the module globals.  With
it, the declaration of a name's public export semantics are not separated from
the implementation of that name.")
    (license (list license:asl2.0
                   license:lgpl3))))    ; only for setup_helpers.py

(define-public python-mockito
  (package
    (name "python-mockito")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/kaste/mockito-python")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fg8jflcf4c929gd4zbcrk73d08waaqjfjmdjrgnv54mzl35pjxl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (native-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/kaste/mockito-python")
    (synopsis "Mocking library for Python")
    (description "This package provides a Python implementation of the Java
library of the same name.  It eases monkey patching, for example to stub out
side effects when unit testing.")
    (license license:expat)))

(define-public python-mypy-extensions
  (package
    (name "python-mypy-extensions")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mypy_extensions" version))
        (sha256
         (base32
          "1a04qsk8hd1lqns8w1j7cr0vmvbhg450di5k1i16kqxkbf7q30id"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)); no tests
    (home-page "https://github.com/python/mypy_extensions")
    (synopsis "Experimental extensions for MyPy")
    (description "The @code{python-mypy-extensions} module defines
experimental extensions to the standard @code{typing} module that are
supported by the MyPy typechecker.")
    (license license:expat)))

(define-public python-mypy
  (package
    (name "python-mypy")
    (version "0.910")
    (source
     (origin
       ;; Because of https://github.com/python/mypy/issues/9584, the
       ;; mypyc/analysis directory is missing in the PyPI archive, leading to
       ;; test failures.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/python/mypy")
             (commit (string-append "v" version))
             ;; Fetch git submodules otherwise typeshed is not fetched.
             ;; Typeshed is a collection of Python sources type annotation
             ;; (data) files.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16ryn9d48ilcs3yrkrm9ynx36qnv0gkdkc4sbafpagcqgr2f0mrg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "mypyc")))))))
    (native-inputs
     `(("python-attrs" ,python-attrs)
       ("python-flake8" ,python-flake8)
       ("python-flake8-bugbear" ,python-flake8-bugbear)
       ("python-flake8-pyi" ,python-flake8-pyi)
       ("python-importlib-metadata" ,python-importlib-metadata)
       ("python-lxml" ,python-lxml)
       ("python-psutil" ,python-psutil)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-forked" ,python-pytest-forked)
       ("python-pytest-xdist" ,python-pytest-xdist)
       ("python-virtualenv" ,python-virtualenv)))
    (propagated-inputs
     `(("python-mypy-extensions" ,python-mypy-extensions)
       ("python-toml" ,python-toml)
       ("python-typing-extensions" ,python-typing-extensions)
       ("python-typed-ast" ,python-typed-ast)))
    (home-page "http://www.mypy-lang.org/")
    (synopsis "Static type checker for Python")
    (description "Mypy is an optional static type checker for Python that aims
to combine the benefits of dynamic (or 'duck') typing and static typing.  Mypy combines
the expressive power and convenience of Python with a powerful type system and
compile-time type checking.  Mypy type checks standard Python programs; run them using
any Python VM with basically no runtime overhead.")
    ;; Most of the code is under MIT license; Some files are under Python Software
    ;; Foundation License version 2: stdlib-samples/*, mypyc/lib-rt/pythonsupport.h and
    ;; mypyc/lib-rt/getargs.c
    (license (list license:expat license:psfl))))

;;; This variant exists to break a cycle between python-pylama and python-isort.
(define-public python-mypy-minimal
  (hidden-package
   (package
     (inherit python-mypy)
     (name "python-mypy-minimal")
     (arguments
      `(#:tests? #f
        #:phases (modify-phases %standard-phases
                   ;; XXX: Fails with: "In procedure utime: No such file or
                   ;; directory".
                   (delete 'ensure-no-mtimes-pre-1980))))
     (native-inputs '()))))

(define-public python-pylama
  (package
    (name "python-pylama")
    (version "7.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pylama" version))
       (sha256
        (base32
         "13vx7daqz2918y9s8q3v2i3xaq3ah43a9p58srqi6hqskkpm7blv"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-failing-tests
                    (lambda _
                      ;; Fails with: "ImportError: cannot import name
                      ;; 'commented_out_code_line_numbers' from 'eradicate'".
                      (delete-file "pylama/lint/pylama_eradicate.py")
                      ;; Requires python-astroid, which fails to build on
                      ;; Python 3.9+ (see:
                      ;; https://github.com/PyCQA/astroid/issues/881).
                      (delete-file "pylama/lint/pylama_pylint.py"))))))
    (native-inputs
     `(("python-py" ,python-py)
       ("python-pytest" ,python-pytest)
       ("python-radon" ,python-radon)
       ("git" ,git)))
    (propagated-inputs
     `(("python-mccabe" ,python-mccabe)
       ("python-mypy", python-mypy-minimal)
       ("python-pycodestyle" ,python-pycodestyle)
       ("python-pydocstyle" ,python-pydocstyle)
       ("python-pyflakes" ,python-pyflakes)))
    (home-page "https://github.com/klen/pylama")
    (synopsis "Code audit tool for python")
    (description "Pylama is a code audit tool for Python and JavaScript to check
for style, syntax and other code health metrics.  It is essentially a
convenient wrapper above tools such as Pyflakes, pydocstyle, pycodestyle and
McCabe, among others.")
    (license license:lgpl3+)))

(define-public python-pyannotate
  (package
    (name "python-pyannotate")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyannotate" version))
       (sha256
        (base32
         "16bm0mf7wxvy0lgmcs1p8n1ji8pnvj1jvj8zk3am70dkp825iv84"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-mypy-extensions" ,python-mypy-extensions)
       ("python-six" ,python-six)))
    (home-page
     "https://github.com/dropbox/pyannotate")
    (synopsis "Auto-generate PEP-484 annotations")
    (description "This package, PyAnnotate, is used to auto-generate PEP-484
annotations.")
    (license license:asl2.0)))

(define-public python-eradicate
  (package
    (name "python-eradicate")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eradicate" version))
       (sha256
        (base32
         "1j30g9jfmbfki383qxwrfds8b23yiwywj40lng4lqcf5yab4ahr7"))))
    (build-system python-build-system)
    (home-page "https://github.com/myint/eradicate")
    (synopsis "Remove commented-out code from Python sources")
    (description "The @command{eradicate} command removes commented-out code
from Python files.  It does this by detecting block comments that contain
valid Python syntax that are likely to be commented out code.")
    (license license:expat)))

(define-public python-robber
  (package
    (name "python-robber")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "robber" version))
              (sha256
               (base32
                "0xp5csgv2g9q38hscml6bc5i1nm4xy5lzqqiimm2drxsf0hw2nq5"))))
    (build-system python-build-system)
    ;; There are no tests in the tarball downloaded from PyPI.
    ;; The last version tagged in Github (0.1.0) is older than the one on PyPI.
    ;; Reported upstream: <https://github.com/vesln/robber.py/issues/20>.
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-mock" ,python-mock)
       ("python-termcolor" ,python-termcolor)))
    ;; URL of the fork used to generate the package available on PyPI.
    (home-page "https://github.com/EastAgile/robber.py")
    (synopsis "Test-driven development (TDD) assertion library for Python")
    (description "Robber is a Python assertion library for test-driven and
behavior-driven development (TDD and BDD).")
    (license license:expat)))

(define-public python-stestr
  (package
    (name "python-stestr")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stestr" version))
       (sha256
        (base32
         "0adhqp9c9338wlvlq776k57k04lyxp38bv591afdm9gjsn2qn1zm"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;to avoid circular dependencies
    (native-inputs
     `(("python-pbr" ,python-pbr)))
    (propagated-inputs
     `(("python-cliff" ,python-cliff)
       ("python-fixtures" ,python-fixtures)
       ("python-future" ,python-future)
       ("python-pyyaml" ,python-pyyaml)
       ("python-subunit" ,python-subunit)
       ("python-testtools" ,python-testtools)
       ("python-voluptuous" ,python-voluptuous)))
    (home-page "https://stestr.readthedocs.io/en/latest/")
    (synopsis "Parallel Python test runner")
    (description "This package provides the @command{stestr} command, a
parallel Python test runner built around @code{subunit}.  It is designed to
execute @code{unittest} test suites using multiple processes to split up
execution of a test suite.  It will also store a history of all test runs to
help in debugging failures and optimizing the scheduler to improve speed.")
    (license license:asl2.0)))

;; This is only used by python-sanic
(define-public python-pytest-sanic
  (package
    (name "python-pytest-sanic")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-sanic" version))
              (sha256
                (base32
                  "0hm7im77dgqfk8k34qbbfhimg8hifl4zwpa2s3mgbknrjvyw5qpx"))))
    (build-system python-build-system)
    (arguments
     ;; Tests depend on python-sanic.
     `(#:tests? #f))
    (propagated-inputs
      `(("python-httpx" ,python-httpx)
        ("python-async-generator"
         ,python-async-generator)
        ("python-pytest" ,python-pytest)
        ("python-websockets" ,python-websockets)))
    (home-page
      "https://github.com/yunstanford/pytest-sanic")
    (synopsis "Pytest plugin for Sanic")
    (description "A pytest plugin for Sanic.  It helps you to test your
code asynchronously.")
    (license license:expat)))

(define-public python-allpairspy
  (package
    (name "python-allpairspy")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "allpairspy" version))
       (sha256
        (base32 "1c987h13dly9919d15w3h747rgn50ilnv7dginhlprxbj564hn4k"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/thombashi/allpairspy")
    (synopsis "Pairwise test combinations generator")
    (description
     "This is a Python library for test combinations generator.  The generator
allows one to create a set of tests using @emph{pairwise combinations} method,
reducing a number of combinations of variables into a lesser set that covers
most situations.")
    (license license:expat)))

(define-public python-pytest-mp
  (package
    (name "python-pytest-mp")
    (version "0.0.4p2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ansible/pytest-mp")
             (commit "49a8ff2ca9ef62d8c86854ab31d6b5d5d6cf3f28")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01v98b6n3yvkfmxf2v38xk5ijqlk6ika0yljwkhl5bh6qhq23498"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-psutil" ,python-psutil)))
    (arguments
     ;; tests require setuptools-markdown, which is deprecated and not in guix
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-useless-requirements
           (lambda _
             (substitute* "setup.py"
               ((" setup_requires=") " #")))))))
    (home-page "https://github.com/ansible/pytest-mp")
    (synopsis  "Segregate tests into several processes")
    (description "pytest-mp is a minimalist approach to distribute and
segregate pytest tests across processes using python's multiprocessing library
and is heavily inspired by pytest-concurrent and pytest-xdist.  As a very
early beta, it doesn't pledge or intend to support the majority of platforms
or use cases.  Design is based on supporting slow, io-bound testing with often
tedious system under test configuration that can benefit from running several
tests at one time.")
    (license license:expat)))

(define-public python-aioresponses
  (package
    (name "python-aioresponses")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aioresponses" version))
       (sha256
        (base32 "16p8mdyfirddrsay62ji7rwcrqmmzxzf2isdbfm9cj5p338rbr42"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke
                "pytest" "-vv" "tests" "-k"
                (string-append
                 ;; These tests require network access.
                 "not test_address_as_instance_of_url_combined_with_pass_through "
                 "and not test_pass_through_with_origin_params"))))))))
    (native-inputs
     `(("python-pbr" ,python-pbr)
       ("python-ddt" ,python-ddt)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-aiohttp" ,python-aiohttp)))
    (home-page "https://github.com/pnuckowski/aioresponses")
    (synopsis "Mock out requests made by ClientSession from aiohttp package")
    (description
     "Aioresponses is a helper to mock/fake web requests in python aiohttp
package.  For requests module there are a lot of packages that help us with
testing (eg. httpretty, responses, requests-mock).  When it comes to testing
asynchronous HTTP requests it is a bit harder (at least at the beginning).
The purpose of this package is to provide an easy way to test asynchronous
HTTP requests.")
    (license license:expat)))

(define-public python-pytest-expect
  (package
    (name "python-pytest-expect")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-expect" version))
        (sha256
          (base32
            "0iyq3zd1g5ffaz2wv6mskjfn84sfbyh0j209glcrh1s50hkldd1n"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no tests via pypi
    (propagated-inputs
      `(("python-pytest" ,python-pytest) ; package declares this dependency
        ("python-u-msgpack" ,python-u-msgpack)))
    (home-page
      "https://github.com/gsnedders/pytest-expect")
    (synopsis
      "Py.test plugin to store test expectations and mark tests based on them")
    (description
      "A py.test plugin that stores test expectations by saving the set of
failing tests, allowing them to be marked as xfail when running them in future.
The tests expectations are stored such that they can be distributed alongside
the tests.")
    (license license:expat)))

