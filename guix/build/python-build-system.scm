;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019, 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
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

(define-module (guix build python-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:export (%standard-phases
            add-installed-pythonpath
            site-packages
            python-version
            python-build))

;; Commentary:
;;
;; Builder-side code of the standard Python package build procedure.
;;
;;
;; Backgound about the Python installation methods
;;
;; In Python there are different ways to install packages: distutils,
;; setuptools, easy_install and pip.  All of these are sharing the file
;; setup.py, introduced with distutils in Python 2.0. The setup.py file can be
;; considered as a kind of Makefile accepting targets (or commands) like
;; "build" and "install".  As of autumn 2016 the recommended way to install
;; Python packages is using pip.
;;
;; For both distutils and setuptools, running "python setup.py install" is the
;; way to install Python packages.  With distutils the "install" command
;; basically copies all packages into <prefix>/lib/pythonX.Y/site-packages.
;;
;; Some time later "setuptools" was established to enhance distutils.  To use
;; setuptools, the developer imports setuptools in setup.py.  When importing
;; setuptools, the original "install" command gets overwritten by setuptools'
;; "install" command.
;;
;; The command-line tools easy_install and pip are both capable of finding and
;; downloading the package source from PyPI (the Python Package Index).  Both
;; of them import setuptools and execute the "setup.py" file under their
;; control.  Thus the "setup.py" behaves as if the developer had imported
;; setuptools within setup.py - even is still using only distutils.
;;
;; Setuptools' "install" command (to be more precise: the "easy_install"
;; command which is called by "install") will put the path of the currently
;; installed version of each package and it's dependencies (as declared in
;; setup.py) into an "easy-install.pth" file.  In Guix each packages gets its
;; own "site-packages" directory and thus an "easy-install.pth" of its own.
;; To avoid conflicts, the python build system renames the file to
;; <packagename>.pth in the phase rename-pth-file.  To ensure that Python will
;; process the .pth file, easy_install also creates a basic "site.py" in each
;; "site-packages" directory.  The file is the same for all packages, thus
;; there is no need to rename it.  For more information about .pth files and
;; the site module, please refere to
;; https://docs.python.org/3/library/site.html.
;;
;; The .pth files contain the file-system paths (pointing to the store) of all
;; dependencies.  So the dependency is hidden in the .pth file but is not
;; visible in the file-system.  Now if packages A and B both required packages
;; P, but in different versions, Guix will not detect this when installing
;; both A and B to a profile. (For details and example see
;; https://lists.gnu.org/archive/html/guix-devel/2016-10/msg01233.html.)
;;
;; Pip behaves a bit different then easy_install: it always executes
;; "setup.py" with the option "--single-version-externally-managed" set.  This
;; makes setuptools' "install" command run the original "install" command
;; instead of the "easy_install" command, so no .pth file (and no site.py)
;; will be created.  The "site-packages" directory only contains the package
;; and the related .egg-info directory.
;;
;; This is exactly what we need for Guix and this is what we mimic in the
;; install phase below.
;;
;; As a draw back, the magic of the .pth file of linking to the other required
;; packages is gone and these packages have now to be declared as
;; "propagated-inputs".
;;
;; Note: Importing setuptools also adds two sub-commands: "install_egg_info"
;; and "install_scripts".  These sub-commands are executed even if
;; "--single-version-externally-managed" is set, thus the .egg-info directory
;; and the scripts defined in entry-points will always be created.

;; Base error type.
(define-condition-type &python-build-error &error
  python-build-error?)

;; Raised when 'check cannot find a valid test system in the inputs.
(define-condition-type &test-system-not-found &python-build-error
  test-system-not-found?)

;; Raised when multiple wheels are created by 'build.
(define-condition-type &cannot-extract-multiple-wheels &python-build-error
  cannot-extract-multiple-wheels?)

(define* (sanity-check #:key tests? inputs outputs #:allow-other-keys)
  "Ensure packages depending on this package via setuptools work properly,
their advertised endpoints work and their top level modules are importable
without errors."
  (let ((sanity-check.py (assoc-ref inputs "sanity-check.py")))
    ;; Make sure the working directory is empty (i.e. no Python modules in it)
    (with-directory-excursion "/tmp"
      (invoke "python" sanity-check.py (site-packages inputs outputs)))))

(define* (build #:key outputs #:allow-other-keys)
  "Build a given Python package."

  (define pyproject-build (which "pyproject-build"))

  (define (build-pep517)
    ;; XXX: should probably use a different path, outside of source directory,
    ;; maybe secondary output “wheel”?
    (mkdir-p "dist")
    (invoke pyproject-build "--outdir" "dist" "--no-isolation" "--wheel" "."))

      ;; XXX Would be nice, if we could use bdist_wheel here to remove extra
      ;; code path in 'install, but that depends on python-wheel.
  (define (build-setuptools)
    (invoke "python" "setup.py" "build"))

  (if pyproject-build
    (build-pep517)
    (build-setuptools))
  #t)

(define* (check #:key inputs outputs tests? #:allow-other-keys)
  "Run the test suite of a given Python package."
  (if tests?
    ;; Unfortunately with PEP 517 there is no common method to specify test
    ;; systems. Guess test system based on inputs instead.
    (let ((pytest (which "pytest"))
            (have-setup-py (file-exists? "setup.py")))
        ;; Prefer pytest
        ;; XXX: support nose
        (cond
          (pytest
            (begin
              (format #t "using pytest~%")
              (invoke pytest "-vv"))) ; XXX: support skipping tests based on name/extra arguments?
          ;; But fall back to setup.py, which should work for most
          ;; packages. XXX: would be nice not to depend on setup.py here? fails
          ;; more often than not to find any tests at all. Maybe we can run
          ;; `python -m unittest`?
          (have-setup-py
            (begin
              (format #t "using setup.py~%")
                (invoke "python" "setup.py" "test" "-v")))
          ;; The developer should explicitly disable tests in this case.
          (#t (raise (condition (&test-system-not-found))))))
      (format #t "test suite not run~%"))
  #t)

(define (python-version python)
  (let* ((version     (last (string-split python #\-)))
         (components  (string-split version #\.))
         (major+minor (take components 2)))
    (string-join major+minor ".")))

(define (python-output outputs)
  "Return the path of the python output, if there is one, or fall-back to out."
  (or (assoc-ref outputs "python")
      (assoc-ref outputs "out")))

(define (site-packages inputs outputs)
  "Return the path of the current output's Python site-package."
  (let* ((out (python-output outputs))
         (python (assoc-ref inputs "python")))
    (string-append out "/lib/python" (python-version python) "/site-packages")))

(define (add-installed-pythonpath inputs outputs)
  "Prepend the site-package of OUTPUT to GUIX_PYTHONPATH.  This is useful when
running checks after installing the package."
  (setenv "GUIX_PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                           (getenv "GUIX_PYTHONPATH"))))

(define* (add-install-to-pythonpath #:key inputs outputs #:allow-other-keys)
  "A phase that just wraps the 'add-installed-pythonpath' procedure."
  (add-installed-pythonpath inputs outputs))

(define* (add-install-to-path #:key outputs #:allow-other-keys)
  "Adding Python scripts to PATH is also often useful in tests."
  (setenv "PATH" (string-append (assoc-ref outputs "out")
                                "/bin:"
                                (getenv "PATH"))))

(define* (install #:key inputs outputs (configure-flags '()) #:allow-other-keys)
  "Install a wheel file according to PEP 427"
  ;; See https://www.python.org/dev/peps/pep-0427/#installing-a-wheel-distribution-1-0-py32-none-any-whl
  (let* ((site-dir (site-packages inputs outputs))
         (out (assoc-ref outputs "out")))
    (define (extract file)
      "Extract wheel (ZIP file) into site-packages directory"
      ;; Use Python’s zipfile to avoid extra dependency
      (invoke "python" "-m" "zipfile" "-e" file site-dir))

    (define python-hashbang
      (string-append "#!" (assoc-ref inputs "python") "/bin/python"))

    (define (move-data source destination)
      (mkdir-p (dirname destination))
      (rename-file source destination))

    (define (move-script source destination)
      "Move executable script file from .data/scripts to out/bin and replace
temporary hashbang"
	  (move-data source destination)
      ;; ZIP does not save/restore permissions, make executable
      ;; XXX: might not be a file, but directory with subdirectories
      (chmod destination #o755)
      (substitute* destination (("#!python") python-hashbang)))

    ;; Python’s distutils.command.install defines this mapping from source to
    ;; destination mapping.
    (define install-schemes
      `(("scripts" "bin" ,move-script)
        ;; XXX: Why does Python not use share/ here?
        ("data" "share" ,move-data)))

    (define (expand-data-directory directory)
      "Move files from all .data subdirectories to their respective
destinations."
      (for-each
        (match-lambda ((source destination function)
          (let ((source-path (string-append directory "/" source))
                (destination-path (string-append out "/" destination)))
            (when (file-exists? source-path)
              (begin
                ;; This assumes only files exist in the scripts/ directory.
                (for-each
                  (lambda (file)
                    (apply
                      function
                      (list
                        (string-append source-path "/" file)
                        (string-append destination-path "/" file))))
                  (scandir source-path (negate (cut member <> '("." "..")))))
                (rmdir source-path))))))
        install-schemes))
    
  (define pyproject-build (which "pyproject-build"))

  (define (list-directories base predicate)
    ;; Cannot use find-files here, because it’s recursive.
    (scandir
      base
      (lambda (name)
        (let ((stat (lstat (string-append base "/" name))))
        (and
          (not (member name '("." "..")))
          (eq? (stat:type stat) 'directory)
          (predicate name stat))))))

  (define (install-pep517)
    "Install a wheel generated by a PEP 517-compatible builder."
    (let ((wheels (find-files "dist" "\\.whl$"))) ; XXX: do not recurse
      (when (> (length wheels) 1) ; This code does not support multiple wheels
                                  ; yet, because their outputs would have to be
                                  ; merged properly.
        (raise (condition (&cannot-extract-multiple-wheels))))
      (for-each extract wheels))
    (let ((datadirs (map
					  (cut string-append site-dir "/" <>)
					  (list-directories site-dir (file-name-predicate "\\.data$")))))
      (for-each (lambda (directory)
                  (expand-data-directory directory)
                  (rmdir directory))
                datadirs)))

    (define (install-setuptools)
      "Install using setuptools."
      (let ((out (assoc-ref outputs "out")))
        (invoke "python" "setup.py"
				"install"
				"--prefix" out
				"--single-version-externally-managed"
				"--root=/")))

    (if pyproject-build
      (install-pep517)
      (install-setuptools))
    #t))

(define* (compile-bytecode #:key inputs outputs (configure-flags '()) #:allow-other-keys)
  "Compile installed byte-code in site-packages."
  (let ((site-dir (site-packages inputs outputs)))
    (invoke "python" "-m" "compileall" site-dir)
    ;; XXX: We could compile with -O and -OO too here, at the cost of more space.
    #t))

(define* (wrap #:key inputs outputs #:allow-other-keys)
  (define (list-of-files dir)
    (find-files dir (lambda (file stat)
                      (and (eq? 'regular (stat:type stat))
                           (not (wrapped-program? file))))))

  (define bindirs
    (append-map (match-lambda
                  ((_ . dir)
                   (list (string-append dir "/bin")
                         (string-append dir "/sbin"))))
                outputs))

  (let* ((var `("GUIX_PYTHONPATH" prefix
                ,(search-path-as-string->list
                  (or (getenv "GUIX_PYTHONPATH") "")))))
    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each (cut wrap-program <> var)
                            files)))
              bindirs)))

(define* (set-SOURCE-DATE-EPOCH #:rest _)
  "Set the 'SOURCE_DATE_EPOCH' environment variable.  This is used by tools
that incorporate timestamps as a way to tell them to use a fixed timestamp.
See https://reproducible-builds.org/specs/source-date-epoch/."
  (setenv "SOURCE_DATE_EPOCH" "315619200") ;; python-wheel respects this variable and sets pre-1980 times on files in zip files, which is unsupported
  #t)

(define* (enable-bytecode-determinism #:rest _)
  "Improve determinism of pyc files."
  ;; Use deterministic hashes for strings, bytes, and datetime objects.
  (setenv "PYTHONHASHSEED" "0")
  ;; Prevent Python from creating .pyc files when loading modules (such as
  ;; when running a test suite).
  (setenv "PYTHONDONTWRITEBYTECODE" "1"))

(define* (ensure-no-cythonized-files #:rest _)
  "Check the source code for @code{.c} files which may have been pre-generated
by Cython."
  (for-each
    (lambda (file)
      (let ((generated-file
              (string-append (string-drop-right file 3) "c")))
        (when (file-exists? generated-file)
          (format #t "Possible Cythonized file found: ~a~%" generated-file))))
    (find-files "." "\\.pyx$")))

(define %standard-phases
  ;; The build phase only builds C extensions and copies the Python sources,
  ;; while the install phase copies then byte-compiles the sources to the
  ;; prefix directory.  The check phase is moved after the installation phase
  ;; to ease testing the built package.
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'enable-bytecode-determinism
      enable-bytecode-determinism)
    (add-after 'enable-bytecode-determinism 'ensure-no-cythonized-files
      ensure-no-cythonized-files)
    (replace 'set-SOURCE-DATE-EPOCH set-SOURCE-DATE-EPOCH)
    (delete 'bootstrap)
    (delete 'configure)                 ;not needed
    (replace 'build build)
    (delete 'check)                     ;moved after the install phase
    (replace 'install install)
    (add-after 'install 'add-install-to-pythonpath add-install-to-pythonpath)
    (add-after 'add-install-to-pythonpath 'add-install-to-path
      add-install-to-path)
    (add-after 'add-install-to-path 'wrap wrap)
    (add-after 'wrap 'check check)
    (add-after 'check 'sanity-check sanity-check)
    (add-before 'check 'compile-bytecode compile-bytecode)))

(define* (python-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Python package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; python-build-system.scm ends here
