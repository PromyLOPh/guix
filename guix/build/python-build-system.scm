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
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
  #:use-module (guix build json)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (%standard-phases
            add-installed-pythonpath
            site-packages
            python-version
            python-build))

;; Commentary:
;;
;; PEP 517-compatible build system for Python packages.
;;
;; PEP 517 mandates the use of a TOML file called pyproject.toml at the
;; project root, describing build and runtime dependencies, as well as the
;; build system, which can be different from setuptools. This module uses
;; that file to extract the build system used and call its wheel-building
;; entry point build_wheel (see 'build). setuptools’ wheel builder is
;; used as a fallback if either no pyproject.toml exists or it does not
;; declare a build-system. It supports config_settings through the
;; standard #:configure-flags argument.
;;
;; This wheel, which is just a ZIP file with a file structure defined
;; by PEP 427 (https://www.python.org/dev/peps/pep-0427/), is then unpacked
;; and its contents are moved to the appropriate locations in 'install.
;;
;; Then entry points, as defined by the PyPa Entry Point Specification
;; (https://packaging.python.org/specifications/entry-points/) are read
;; from a file called entry_points.txt in the package’s site-packages
;; subdirectory and scripts are written to bin/. These are not part of a
;; wheel and expected to be created by the installing utility.
;;
;; Caveats:
;; - There is no support for in-tree build backends.

;; Base error type.
(define-condition-type &python-build-error &error
  python-build-error?)

;; Raised when 'check cannot find a valid test system in the inputs.
(define-condition-type &test-system-not-found &python-build-error
  test-system-not-found?)

;; Raised when multiple wheels are created by 'build.
(define-condition-type &cannot-extract-multiple-wheels &python-build-error
  cannot-extract-multiple-wheels?)

;; Raised, when no wheel has been built by the build system.
(define-condition-type &no-wheels-built &python-build-error
  no-wheels-built?)

(define* (sanity-check #:key tests? inputs outputs #:allow-other-keys)
  "Ensure packages depending on this package via setuptools work properly,
their advertised endpoints work and their top level modules are importable
without errors."
  (let ((sanity-check.py (assoc-ref inputs "sanity-check.py")))
    ;; Make sure the working directory is empty (i.e. no Python modules in it)
    (with-directory-excursion "/tmp"
      (invoke "python" sanity-check.py (site-packages inputs outputs)))))

(define* (build #:key outputs build-backend configure-flags #:allow-other-keys)
  "Build a given Python package."

  (define (pyproject.toml->build-backend file)
    "Look up the build backend in a pyproject.toml file."
    (call-with-input-file file
      (lambda (in)
        (let loop ((line (read-line in 'concat)))
          (if (eof-object? line)
              #f
              (let ((m (string-match "build-backend = [\"'](.+)[\"']" line)))
                (if m (match:substring m 1)
                    (loop (read-line in 'concat)))))))))

  (let* ((wheel-output (assoc-ref outputs "wheel"))
         (wheel-dir (if wheel-output wheel-output "dist"))
         ;; There is no easy way to get data from Guile into Python via
         ;; s-expressions, but we have JSON serialization already, which Python
         ;; also supports out-of-the-box.
         (config-settings (call-with-output-string (cut write-json configure-flags <>)))
         ;; python-setuptools’ default backend supports setup.py *and*
         ;; pyproject.toml. Allow overriding this automatic detection via
         ;; build-backend.
         (auto-build-backend (if (file-exists? "pyproject.toml")
                               (pyproject.toml->build-backend "pyproject.toml")
                               #f))
         ;; Use build system detection here and not in importer, because a) we
         ;; have alot of legacy packages and b) the importer cannot update arbitrary
         ;; fields in case a package switches its build system.
         (use-build-backend (or
                              build-backend
                              auto-build-backend
                              "setuptools.build_meta")))
    (format #t "Using '~a' to build wheels, auto-detected '~a', override '~a'.~%"
               use-build-backend auto-build-backend build-backend)
    (mkdir-p wheel-dir)
    ;; Call the PEP 517 build function, which drops a .whl into wheel-dir.
    (invoke "python" "-c" "import sys, importlib, json
config_settings = json.loads (sys.argv[3])
builder = importlib.import_module(sys.argv[1])
builder.build_wheel(sys.argv[2], config_settings=config_settings)"
            use-build-backend wheel-dir config-settings))
  #t)

(define* (check #:key inputs outputs tests? test-backend test-flags #:allow-other-keys)
  "Run the test suite of a given Python package."
  (if tests?
    ;; Unfortunately with PEP 517 there is no common method to specify test
    ;; systems. Guess test system based on inputs instead.
    (let* ((pytest (which "pytest"))
           (nosetests (which "nosetests"))
           (nose2 (which "nose2"))
           (have-setup-py (file-exists? "setup.py"))
           (use-test-backend
            (or
              test-backend
              ;; Prefer pytest
              (if pytest 'pytest #f)
              (if nosetests 'nose #f)
              (if nose2 'nose2 #f)
              ;; But fall back to setup.py, which should work for most
              ;; packages. XXX: would be nice not to depend on setup.py here? fails
              ;; more often than not to find any tests at all. Maybe we can run
              ;; `python -m unittest`?
              (if have-setup-py 'setup.py #f))))
        (format #t "Using ~a~%" use-test-backend)
        (match use-test-backend
          ('pytest
           (apply invoke (cons pytest (or test-flags '("-vv")))))
          ('nose
           (apply invoke (cons nosetests (or test-flags '("-v")))))
          ('nose2
           (apply invoke (cons nose2 (or test-flags '("-v" "--pretty-assert")))))
          ('setup.py
           (apply invoke (append '("python" "setup.py") (or test-flags '("test" "-v")))))
          ;; The developer should explicitly disable tests in this case.
          (else (raise (condition (&test-system-not-found))))))
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
         (python (assoc-ref inputs "python"))
         (out (assoc-ref outputs "out")))
    (define (extract file)
      "Extract wheel (ZIP file) into site-packages directory"
      ;; Use Python’s zipfile to avoid extra dependency
      (invoke "python" "-m" "zipfile" "-e" file site-dir))

    (define python-hashbang
      (string-append "#!" python "/bin/python"))

    (define* (merge-directories source destination #:optional (post-move #f))
      "Move all files in SOURCE into DESTINATION, merging the two directories."
      (format #t "Merging directory ~a into ~a~%" source destination)
      (for-each
        (lambda (file)
          (format #t "~a/~a -> ~a/~a~%" source file destination file)
          (mkdir-p destination)
          (rename-file
              (string-append source "/" file)
              (string-append destination "/" file))
          (when post-move
            (post-move file)))
        (scandir source (negate (cut member <> '("." "..")))))
      (rmdir source))

    (define (expand-data-directory directory)
      "Move files from all .data subdirectories to their respective
destinations."
      ;; Python’s distutils.command.install defines this mapping from source to
      ;; destination mapping.
      (let ((source (string-append directory "/scripts"))
            (destination (string-append out "/bin")))
        (when (file-exists? source)
          (merge-directories
           source
           destination
           (lambda (f)
             (let ((dest-path (string-append destination "/" f)))
               (chmod dest-path #o755)
               (substitute* dest-path (("#!python") python-hashbang)))))))
      ;; XXX: Why does Python not use share/ here?
      (let ((source (string-append directory "/data"))
            (destination (string-append out "/share")))
        (when (file-exists? source)
          (merge-directories source destination)))
      (let* ((distribution (car (string-split (basename directory) #\-)))
            (source (string-append directory "/headers"))
            (destination (string-append out "/include/python" (python-version python) "/" distribution)))
        (when (file-exists? source)
          (merge-directories source destination))))
    
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

  (let* ((wheel-output (assoc-ref outputs "wheel"))
         (wheel-dir (if wheel-output wheel-output "dist"))
         (wheels (find-files wheel-dir "\\.whl$"))) ; XXX: do not recurse
    (cond
    ((> (length wheels) 1) ; This code does not support multiple wheels
                                ; yet, because their outputs would have to be
                                ; merged properly.
      (raise (condition (&cannot-extract-multiple-wheels))))
      ((= (length wheels) 0)
       (raise (condition (&no-wheels-built)))))
    (for-each extract wheels))
  (let ((datadirs (map
                    (cut string-append site-dir "/" <>)
                    (list-directories site-dir (file-name-predicate "\\.data$")))))
    (for-each (lambda (directory)
                (expand-data-directory directory)
                (rmdir directory))
              datadirs))
  #t))

(define* (compile-bytecode #:key inputs outputs (configure-flags '()) #:allow-other-keys)
  "Compile installed byte-code in site-packages."
  (let ((site-dir (site-packages inputs outputs)))
    (invoke "python" "-m" "compileall" site-dir)
    ;; XXX: We could compile with -O and -OO too here, at the cost of more space.
    #t))

(define* (create-entrypoints #:key inputs outputs (configure-flags '()) #:allow-other-keys)
  "Implement Entry Points Specification
(https://packaging.python.org/specifications/entry-points/) by PyPa,
which creates runnable scripts in bin/ from entry point specification
file entry_points.txt. This is necessary, because wheels do not contain
these binaries and installers are expected to create them."

  (define (entry-points.txt->entry-points file)
    "Specialized parser for Python configfile-like files, in particular
entry_points.txt. Returns a list of console_script and gui_scripts
entry points."
    (call-with-input-file file
      (lambda (in)
        (let loop ((line (read-line in))
                   (inside #f)
                   (result '()))
          (if (eof-object? line)
            result
            (let* ((group-match (string-match "^\\[(.+)\\]$" line))
                  (group-name (if group-match (match:substring group-match 1) #f))
                  (next-inside
                   (if (not group-name)
                     inside
                     (or
                       (string=? group-name "console_scripts")
                       (string=? group-name "gui_scripts"))))
                  (item-match (string-match "^([^ =]+)\\s*=\\s*([^:]+):(.+)$" line)))
              (if (and inside item-match)
                (loop (read-line in) next-inside (cons (list
                                                        (match:substring item-match 1)
                                                        (match:substring item-match 2)
                                                        (match:substring item-match 3))
                                                         result))
                (loop (read-line in) next-inside result))))))))

  (define (create-script path name module function)
    "Create a Python script from an entry point’s NAME, MODULE and
  FUNCTION and return write it to PATH/NAME."
    (let ((interpreter (which "python3"))
          (file-path (string-append path "/" name)))
      (format #t "Creating entry point for '~a.~a' at '~a'.~%" module function
                 file-path)
      (call-with-output-file file-path
        (lambda (port)
          ;; Technically the script could also include search-paths,
          ;; but having a generic 'wrap phases also handles manually
          ;; written entry point scripts.
          (format port "#!~a
# Auto-generated entry point script.
import sys
import ~a as mod
sys.exit (mod.~a ())~%" interpreter module function)))
        (chmod file-path #o755)))

  (let* ((site-dir (site-packages inputs outputs))
         (out (assoc-ref outputs "out"))
         (bin-dir (string-append out "/bin"))
         (entry-point-files (find-files site-dir "^entry_points.txt$")))
    (mkdir-p bin-dir)
    (for-each
      (lambda (f)
        (for-each
          (lambda (ep) (apply create-script (cons bin-dir ep)))
          (entry-points.txt->entry-points f)))
      entry-point-files)))

(define* (wrap #:key inputs outputs search-paths #:allow-other-keys)
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

  ;; Do not require "bash" to be present in the package inputs
  ;; even when there is nothing to wrap.
  ;; Also, calculate (sh) only once to prevent some I/O.
  (define %sh (delay (search-input-file inputs "bin/bash")))
  (define (sh) (force %sh))

    (define input-directories
    ;; The "source" input can be a directory, but we don't want it for search
    ;; paths.  See <https://issues.guix.gnu.org/44924>.
    (match (alist-delete "source" inputs)
      (((_ . dir) ...)
       dir)))

    (for-each (match-lambda
             ((env-var (files ...) separator type pattern)
              (display (search-path-as-list files input-directories
                                     #:type type
                                     #:pattern pattern))))
            search-paths)

  (let* ((var `("GUIX_PYTHONPATH" prefix
                ,(search-path-as-string->list
                  (or (getenv "GUIX_PYTHONPATH") "")))))
    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each (cut wrap-program <> #:sh (sh) var)
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
    ;; must be before tests, so they can use installed packages’ entry points.
    (add-before 'wrap 'create-entrypoints create-entrypoints)
    (add-after 'wrap 'check check)
    (add-after 'check 'sanity-check sanity-check)
    (add-before 'check 'compile-bytecode compile-bytecode)))

(define* (python-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Python package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; python-build-system.scm ends here
