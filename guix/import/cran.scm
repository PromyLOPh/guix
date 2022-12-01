;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix import cran)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module ((ice-9 rdelim) #:select (read-string read-line))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 receive)
  #:use-module (web uri)
  #:use-module (guix memoization)
  #:use-module (guix http-client)
  #:use-module (guix diagnostics)
  #:use-module (guix hash)
  #:use-module (guix i18n)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module ((guix build utils)
                #:select (find-files
                          delete-file-recursively
                          with-directory-excursion))
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module ((guix build-system r) #:select (cran-uri bioconductor-uri))
  #:use-module (guix ui)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:export (%input-style

            cran->guix-package
            bioconductor->guix-package
            cran-recursive-import
            %cran-updater
            %bioconductor-updater
            %bioconductor-version

            cran-package?
            bioconductor-package?
            bioconductor-data-package?
            bioconductor-experiment-package?

            description->alist
            description->package))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of an R
;;; package on CRAN, using the DESCRIPTION file downloaded from
;;; cran.r-project.org.
;;;
;;; Code:

(define %input-style
  (make-parameter 'variable)) ; or 'specification

(define (string->licenses license-string license-prefix)
  (let ((licenses
         (map string-trim-both
              (string-tokenize license-string
                               (char-set-complement (char-set #\|))))))
    (string->license licenses license-prefix)))

(define (string->license license-string license-prefix)
  (let ((prefix license-prefix))
    (match license-string
      ("AGPL-3" (prefix 'agpl3))
      ("AGPL (>= 3)" (prefix 'agpl3+))
      ("Artistic-2.0" (prefix 'artistic2.0))
      ((or "Apache License 2.0"
           "Apache License (== 2.0)")
       (prefix 'asl2.0))
      ("BSD_2_clause" (prefix 'bsd-2))
      ("BSD_2_clause + file LICENSE" (prefix 'bsd-2))
      ("BSD_3_clause" (prefix 'bsd-3))
      ("BSD_3_clause + file LICENSE" (prefix 'bsd-3))
      ("CC0" (prefix 'cc0))
      ("CC BY-SA 4.0" (prefix 'cc-by-sa4.0))
      ("CeCILL" (prefix 'cecill))
      ((or "GPL"
           "GNU General Public License")
       `(list ,(prefix 'gpl2+) ,(prefix 'gpl3+)))
      ((or "GPL (>= 2)"
           "GPL (>= 2.0)")
       (prefix 'gpl2+))
      ((or "GPL (> 2)"
           "GPL (>= 3)"
           "GPL (>= 3.0)"
           "GNU General Public License (>= 3)")
       (prefix 'gpl3+))
      ((or "GPL-2"
           "GNU General Public License version 2")
       (prefix 'gpl2))
      ((or "GPL-3"
           "GNU General Public License version 3")
       (prefix 'gpl3))
      ((or "GNU Lesser General Public License"
           "LGPL")
       (prefix 'lgpl2.0+))
      ("LGPL-2" (prefix 'lgpl2.0))
      ("LGPL-2.1" (prefix 'lgpl2.1))
      ("LGPL-3" (prefix 'lgpl3))
      ((or "LGPL (>= 2)"
           "LGPL (>= 2.0)")
       (prefix 'lgpl2.0+))
      ("LGPL (>= 2.1)" (prefix 'lgpl2.1+))
      ("LGPL (>= 3)" (prefix 'lgpl3+))
      ("MIT" (prefix 'expat))
      ("MIT + file LICENSE" (prefix 'expat))
      ("file LICENSE"
       `(,(prefix 'fsdg-compatible) "file://LICENSE"))
      ((x) (string->license x license-prefix))
      ((lst ...) `(list ,@(map (cut string->license <> license-prefix) lst)))
      (unknown `(,(prefix 'fsdg-compatible) ,unknown)))))

(define (description->alist description)
  "Convert a DESCRIPTION string into an alist."
  (let ((lines (string-split description #\newline))
        (parse (lambda (line acc)
                 (if (string-null? line) acc
                     ;; Keys usually start with a capital letter and end with
                     ;; ":".  There are some exceptions, unfortunately (such
                     ;; as "biocViews").  There are no blanks in a key.
                     (if (string-match "^[A-Za-z][^ :]+:( |\n|$)" line)
                         ;; New key/value pair
                         (let* ((pos   (string-index line #\:))
                                (key   (string-take line pos))
                                (value (string-drop line (+ 1 pos))))
                           (cons (cons key
                                       (string-trim-both value))
                                 acc))
                         ;; This is a continuation of the previous pair
                         (match-let ((((key . value) . rest) acc))
                           (cons (cons key (string-join
                                            (list value
                                                  (string-trim-both line))))
                                 rest)))))))
    (fold parse '() lines)))

(define (format-inputs names)
  "Generate a sorted list of package inputs from a list of package NAMES."
  (map (lambda (name)
         (case (%input-style)
           ((specification)
            `(specification->package ,name))
           (else
            (string->symbol name))))
       (sort names string-ci<?)))

(define* (maybe-inputs package-inputs #:optional (type 'inputs))
  "Given a list of PACKAGE-INPUTS, tries to generate the TYPE field of a
package definition."
  (match package-inputs
    (()
     '())
    ((package-inputs ...)
     `((,type (list ,@(format-inputs package-inputs)))))))

(define %cran-url "https://cran.r-project.org/web/packages/")
(define %cran-canonical-url "https://cran.r-project.org/package=")
(define %bioconductor-url "https://bioconductor.org/packages/")

;; The latest Bioconductor release is 3.16.  Bioconductor packages should be
;; updated together.
(define %bioconductor-version "3.16")

(define* (bioconductor-packages-list-url #:optional type)
  (string-append "https://bioconductor.org/packages/"
                 %bioconductor-version
                 (match type
                   ('annotation "/data/annotation")
                   ('experiment "/data/experiment")
                   (_ "/bioc"))
                 "/src/contrib/PACKAGES"))

(define* (bioconductor-packages-list #:optional type)
  "Return the latest version of package NAME for the current bioconductor
release."
  (let ((url (string->uri (bioconductor-packages-list-url type))))
    (guard (c ((http-get-error? c)
               (warning (G_ "failed to retrieve list of packages \
from ~a: ~a (~a)~%")
                        (uri->string (http-get-error-uri c))
                        (http-get-error-code c)
                        (http-get-error-reason c))
               #f))
      ;; Split the big list on empty lines, then turn each chunk into an
      ;; alist of attributes.
      (map (lambda (chunk)
             (description->alist (string-join chunk "\n")))
           (let* ((port  (http-fetch/cached url))
                  (lines (read-lines port)))
             (close-port port)
             (chunk-lines lines))))))

(define* (latest-bioconductor-package-version name #:optional type)
  "Return the version string corresponding to the latest release of the
bioconductor package NAME, or #F if the package is unknown."
  (and=> (find (lambda (meta)
                 (string=? (assoc-ref meta "Package") name))
               (bioconductor-packages-list type))
         (cut assoc-ref <> "Version")))

;; Little helper to download URLs only once.
(define download
  (memoize
   (lambda* (url #:key method (ref '()))
     (with-store store
       (cond
        ((eq? method 'git)
         (latest-repository-commit store url #:ref ref))
        ((eq? method 'hg)
         (call-with-temporary-directory
          (lambda (dir)
            (unless (zero? (system* "hg" "clone" url dir))
              (leave (G_ "~A: hg download failed~%") url))
            (with-directory-excursion dir
              (let* ((port (open-pipe* OPEN_READ "hg" "id" "--id"))
                     (changeset (string-trim-right (read-string port))))
                (close-pipe port)
                (for-each delete-file-recursively
                          (find-files dir "^\\.hg$" #:directories? #t))
                (let ((store-directory
                       (add-to-store store (basename url) #t "sha256" dir)))
                  (values store-directory changeset)))))))
        (else
         (match url
           ((? string?)
            (download-to-store store url))
           ((urls ...)
            ;; Try all the URLs.  A use case where this is useful is when one
            ;; of the URLs is the /Archive CRAN URL.
            (any (cut download-to-store store <>) urls)))))))))

(define (fetch-description-from-tarball url)
  "Fetch the tarball at URL, extra its 'DESCRIPTION' file, parse it, and
return the resulting alist."
  (match (download url)
    (#f #f)
    (tarball
     (call-with-temporary-directory
      (lambda (dir)
        (parameterize ((current-error-port (%make-void-port "rw+"))
                       (current-output-port (%make-void-port "rw+")))
          (and (zero? (system* "tar" "--wildcards" "-x"
                               "--strip-components=1"
                               "-C" dir
                               "-f" tarball "*/DESCRIPTION"))
               (description->alist
                (call-with-input-file (string-append dir "/DESCRIPTION")
                  read-string)))))))))

(define* (fetch-description repository name #:optional version)
  "Return an alist of the contents of the DESCRIPTION file for the R package
NAME at VERSION in the given REPOSITORY, or #f in case of failure.  NAME is
case-sensitive."
  (case repository
    ((cran)
     (guard (c ((http-get-error? c)
                (warning (G_ "failed to retrieve package information \
from ~a: ~a (~a)~%")
                         (uri->string (http-get-error-uri c))
                         (http-get-error-code c)
                         (http-get-error-reason c))
                #f))
       ;; When VERSION is true, we have to download the tarball to get at its
       ;; 'DESCRIPTION' file; only the latest one is directly accessible over
       ;; HTTP.
       (if version
           (let ((urls (list (string-append "mirror://cran/src/contrib/"
                                            name "_" version ".tar.gz")
                             (string-append "mirror://cran/src/contrib/Archive/"
                                            name "/"
                                            name "_" version ".tar.gz"))))
             (fetch-description-from-tarball urls))
           (let* ((url    (string-append %cran-url name "/DESCRIPTION"))
                  (port   (http-fetch url))
                  (result (description->alist (read-string port))))
             (close-port port)
             result))))
    ((bioconductor)
     ;; Currently, the bioconductor project does not offer a way to access a
     ;; package's DESCRIPTION file over HTTP, so we determine the version,
     ;; download the source tarball, and then extract the DESCRIPTION file.
     (and-let* ((type    (or
                          (and (latest-bioconductor-package-version name) #t)
                          (and (latest-bioconductor-package-version name 'annotation) 'annotation)
                          (and (latest-bioconductor-package-version name 'experiment) 'experiment)))
                ;; TODO: Honor VERSION.
                (version (latest-bioconductor-package-version name type))
                (url     (car (bioconductor-uri name version type)))
                (meta    (fetch-description-from-tarball url)))
       (if (boolean? type)
           meta
           (cons `(bioconductor-type . ,type) meta))))
    ((git)
     (and (string-prefix? "http" name)
          ;; Download the git repository at "NAME"
          (call-with-values
              (lambda () (download name #:method 'git))
            (lambda (dir commit)
              (and=> (description->alist (with-input-from-file
                                             (string-append dir "/DESCRIPTION") read-string))
                     (lambda (meta)
                       (cons* `(git . ,name)
                              `(git-commit . ,commit)
                              meta)))))))
    ((hg)
     (and (string-prefix? "http" name)
          ;; Download the mercurial repository at "NAME"
          (call-with-values
              (lambda () (download name #:method 'hg))
            (lambda (dir changeset)
              (and=> (description->alist (with-input-from-file
                                             (string-append dir "/DESCRIPTION") read-string))
                     (lambda (meta)
                       (cons* `(hg . ,name)
                              `(hg-changeset . ,changeset)
                              meta)))))))))

(define (listify meta field)
  "Look up FIELD in the alist META.  If FIELD contains a comma-separated
string, turn it into a list and strip off parenthetic expressions.  Return the
empty list when the FIELD cannot be found."
  (let ((value (assoc-ref meta field)))
    (if (not value)
        '()
        ;; Strip off parentheses
        (let ((items (string-split (regexp-substitute/global
                                    #f "( *\\([^\\)]+\\)) *"
                                    value 'pre 'post)
                                   #\,)))
          (remove (lambda (item)
                    (or (string-null? item)
                        ;; When there is whitespace inside of items it is
                        ;; probably because this was not an actual list to
                        ;; begin with.
                        (string-any char-set:whitespace item)))
                  (map string-trim-both items))))))

;; Trick Guile 3 so that it keeps the 'listify' binding accessible *and*
;; private even though this module is declarative.
(set! listify listify)

(define default-r-packages
  (list "base"
        "compiler"
        "datasets"
        "grDevices"
        "graphics"
        "grid"
        "methods"
        "parallel"
        "splines"
        "stats"
        "stats4"
        "tcltk"
        "tools"
        "translations"
        "utils"))

;; The field for system dependencies is often abused to specify non-package
;; dependencies (such as c++11).  This list is used to ignore them.
(define invalid-packages
  (list "c++"
        "c++11"
        "c++14"
        "c++17"
        "getopt::long"
        "posix.1-2001"
        "linux"
        "none"
        "windows"
        "xcode"
        "xquartz"))

(define (transform-sysname sysname)
  "Return a Guix package name for the common package name SYSNAME."
  (match sysname
    ("java" "openjdk")
    ("fftw3" "fftw")
    ("tcl/tk" "tcl")
    ("booktabs" "texlive-booktabs")
    ("freetype2" "freetype")
    ("sqlite3" "sqlite")
    (_ sysname)))

(define cran-guix-name (cut guix-name "r-" <>))

(define (tarball-needs-fortran? tarball)
  "Check if the TARBALL contains Fortran source files."
  (define (check pattern)
    (parameterize ((current-error-port (%make-void-port "rw+"))
                   (current-output-port (%make-void-port "rw+")))
      (zero? (system* "tar" "--wildcards" "--list" pattern "-f" tarball))))
  (or (check "*.f90")
      (check "*.f95")
      (check "*.f")))

(define (directory-needs-fortran? dir)
  "Check if the directory DIR contains Fortran source files."
  (match (find-files dir "\\.f(90|95)$")
    (() #f)
    (_ #t)))

(define (needs-fortran? thing tarball?)
  "Check if the THING contains Fortran source files."
  (if tarball?
      (tarball-needs-fortran? thing)
      (directory-needs-fortran? thing)))

(define (files-match-pattern? directory regexp . file-patterns)
  "Return #T if any of the files matching FILE-PATTERNS in the DIRECTORY match
the given REGEXP."
  (let ((pattern (make-regexp regexp)))
    (any (lambda (file)
           (call-with-input-file file
             (lambda (port)
               (let loop ()
                 (let ((line (read-line port)))
                   (cond
                    ((eof-object? line) #f)
                    ((regexp-exec pattern line) #t)
                    (else (loop))))))))
         (apply find-files directory file-patterns))))

(define (tarball-files-match-pattern? tarball regexp . file-patterns)
  "Return #T if any of the files represented by FILE-PATTERNS in the TARBALL
match the given REGEXP."
  (call-with-temporary-directory
   (lambda (dir)
     (parameterize ((current-error-port (%make-void-port "rw+")))
       (apply system* "tar"
              "xf" tarball "-C" dir
              `("--wildcards" ,@file-patterns)))
     (files-match-pattern? dir regexp))))

(define (directory-needs-zlib? dir)
  "Return #T if any of the Makevars files in the src directory DIR contain a
zlib linker flag."
  (files-match-pattern? dir "-lz" "(Makevars.*|configure.*)"))

(define (tarball-needs-zlib? tarball)
  "Return #T if any of the Makevars files in the src directory of the TARBALL
contain a zlib linker flag."
  (tarball-files-match-pattern?
   tarball "-lz"
   "*/src/Makevars*" "*/src/configure*" "*/configure*"))

(define (needs-zlib? thing tarball?)
  "Check if the THING contains files indicating a dependency on zlib."
  (if tarball?
      (tarball-needs-zlib? thing)
      (directory-needs-zlib? thing)))

(define (directory-needs-pkg-config? dir)
  "Return #T if any of the Makevars files in the src directory DIR reference
the pkg-config tool."
  (files-match-pattern? dir "pkg-config"
                        "(Makevars.*|configure.*)"))

(define (tarball-needs-pkg-config? tarball)
  "Return #T if any of the Makevars files in the src directory of the TARBALL
reference the pkg-config tool."
  (tarball-files-match-pattern?
   tarball "pkg-config"
   "*/src/Makevars*" "*/src/configure*" "*/configure*"))

(define (needs-pkg-config? thing tarball?)
  "Check if the THING contains files indicating a dependency on pkg-config."
  (if tarball?
      (tarball-needs-pkg-config? thing)
      (directory-needs-pkg-config? thing)))

(define (needs-knitr? meta)
  (member "knitr" (listify meta "VignetteBuilder")))

(define* (description->package repository meta #:key (license-prefix identity))
  "Return the `package' s-expression for an R package published on REPOSITORY
from the alist META, which was derived from the R package's DESCRIPTION file."
  (let* ((base-url   (case repository
                       ((cran)         %cran-url)
                       ((bioconductor) %bioconductor-url)
                       ((git)          #f)
                       ((hg)           #f)))
         (canonical-url-base (case repository
                               ((cran)         %cran-canonical-url)
                               ((bioconductor) %bioconductor-url)
                               ((git)          #f)))
         (uri-helper (case repository
                       ((cran)         cran-uri)
                       ((bioconductor) bioconductor-uri)
                       ((git)          #f)
                       ((hg)           #f)))
         (name       (assoc-ref meta "Package"))
         (synopsis   (assoc-ref meta "Title"))
         (version    (assoc-ref meta "Version"))
         (license    (string->licenses (assoc-ref meta "License") license-prefix))
         ;; Some packages have multiple home pages.  Some have none.
         (home-page  (case repository
                       ((git) (assoc-ref meta 'git))
                       ((hg)  (assoc-ref meta 'hg))
                       (else (match (listify meta "URL")
                               ((url rest ...) url)
                               (_ (string-append canonical-url-base name))))))
         (source-url (case repository
                       ((git) (assoc-ref meta 'git))
                       ((hg)  (assoc-ref meta 'hg))
                       (else
                        (match (apply uri-helper name version
                                      (case repository
                                        ((bioconductor)
                                         (list (assoc-ref meta 'bioconductor-type)))
                                        (else '())))
                          ((urls ...) urls)
                          ((? string? url) url)
                          (_ #f)))))
         (git?       (if (assoc-ref meta 'git) #true #false))
         (hg?        (if (assoc-ref meta 'hg) #true #false))
         (source     (download source-url #:method (cond
                                                    (git? 'git)
                                                    (hg? 'hg)
                                                    (else #f))))
         (sysdepends (append
                      (if (needs-zlib? source (not (or git? hg?))) '("zlib") '())
                      (filter (lambda (name)
                                (not (member name invalid-packages)))
                              (map string-downcase (listify meta "SystemRequirements")))))
         (propagate  (filter (lambda (name)
                               (not (member name (append default-r-packages
                                                         invalid-packages))))
                             (lset-union equal?
                                         (listify meta "Imports")
                                         (listify meta "LinkingTo")
                                         (delete "R"
                                                 (listify meta "Depends")))))
         (package
           `(package
              (name ,(cran-guix-name name))
              (version ,(cond
                         (git?
                          `(git-version ,version revision commit))
                         (hg?
                          `(string-append ,version "-" revision "." changeset))
                         (else version)))
              (source (origin
                        (method ,(cond
                                  (git? 'git-fetch)
                                  (hg?  'hg-fetch)
                                  (else 'url-fetch)))
                        (uri ,(cond
                               (git?
                                `(git-reference
                                  (url ,(assoc-ref meta 'git))
                                  (commit commit)))
                               (hg?
                                `(hg-reference
                                  (url ,(assoc-ref meta 'hg))
                                  (changeset changeset)))
                               (else
                                `(,(procedure-name uri-helper) ,name version
                                  ,@(or (and=> (assoc-ref meta 'bioconductor-type)
                                               (lambda (type)
                                                 (list (list 'quote type))))
                                        '())))))
                        ,@(cond
                           (git?
                            '((file-name (git-file-name name version))))
                           (hg?
                            '((file-name (string-append name "-" version "-checkout"))))
                           (else '()))
                        (sha256
                         (base32
                          ,(bytevector->nix-base32-string
                            (file-hash* source #:recursive? (or git? hg?)))))))
              ,@(if (not (and git? hg?
                              (equal? (string-append "r-" name)
                                      (cran-guix-name name))))
                    `((properties ,`(,'quasiquote ((,'upstream-name . ,name)))))
                    '())
              (build-system r-build-system)
              ,@(maybe-inputs (map transform-sysname sysdepends))
              ,@(maybe-inputs (map cran-guix-name propagate) 'propagated-inputs)
              ,@(maybe-inputs
                 `(,@(if (needs-fortran? source (not (or git? hg?)))
                         '("gfortran") '())
                   ,@(if (needs-pkg-config? source (not (or git? hg?)))
                         '("pkg-config") '())
                   ,@(if (needs-knitr? meta)
                         '("r-knitr") '()))
                 'native-inputs)
              (home-page ,(if (string-null? home-page)
                              (string-append base-url name)
                              home-page))
              (synopsis ,(beautify-synopsis synopsis))
              (description ,(beautify-description (or (assoc-ref meta "Description")
                                                      "")))
              (license ,license))))
    (values
     (cond
      (git?
       `(let ((commit ,(assoc-ref meta 'git-commit))
              (revision "1"))
          ,package))
      (hg?
       `(let ((changeset ,(assoc-ref meta 'hg-changeset))
              (revision "1"))
          ,package))
      (else package))
     propagate)))

(define cran->guix-package
  (memoize
   (lambda* (package-name #:key (repo 'cran) version (license-prefix identity)
                          (fetch-description fetch-description)
                          #:allow-other-keys)
     "Fetch the metadata for PACKAGE-NAME from REPO and return the `package'
s-expression corresponding to that package, or #f on failure."
     (let ((description (fetch-description repo package-name version)))
       (if description
           (description->package repo description
                                 #:license-prefix license-prefix)
           (case repo
             ((git)
              ;; Retry import from Bioconductor
              (cran->guix-package package-name #:repo 'bioconductor
                                  #:license-prefix license-prefix))
             ((hg)
              ;; Retry import from Bioconductor
              (cran->guix-package package-name #:repo 'bioconductor
                                  #:license-prefix license-prefix))
             ((bioconductor)
              ;; Retry import from CRAN
              (cran->guix-package package-name #:repo 'cran
                                  #:license-prefix license-prefix))
             (else
              (values #f '()))))))))

(define* (cran-recursive-import package-name #:key (repo 'cran) version
                                (license-prefix identity))
  (recursive-import package-name
                    #:version version
                    #:repo repo
                    #:repo->guix-package cran->guix-package
                    #:guix-name cran-guix-name
                    #:license-prefix license-prefix))


;;;
;;; Updater.
;;;

(define (package->upstream-name package)
  "Return the upstream name of the PACKAGE."
  (let* ((properties (package-properties package))
         (upstream-name (and=> properties
                               (cut assoc-ref <> 'upstream-name))))
    (if upstream-name
        upstream-name
        (match (package-source package)
          ((? origin? origin)
           (match (origin-uri origin)
             ((or (? string? url) (url _ ...))
              (let ((end   (string-rindex url #\_))
                    (start (string-rindex url #\/)))
                ;; The URL ends on
                ;; (string-append "/" name "_" version ".tar.gz")
                (and start end (substring url (+ start 1) end))))
             (_ #f)))
          (_ #f)))))

(define (latest-cran-release pkg)
  "Return an <upstream-source> for the latest release of the package PKG."

  (define upstream-name
    (package->upstream-name pkg))

  (define meta
    (fetch-description 'cran upstream-name))

  (and meta
       (let ((version (assoc-ref meta "Version")))
         ;; CRAN does not provide signatures.
         (upstream-source
          (package (package-name pkg))
          (version version)
          (urls (cran-uri upstream-name version))
          (input-changes
           (changed-inputs pkg
                           (description->package 'cran meta)))))))

(define (latest-bioconductor-release pkg)
  "Return an <upstream-source> for the latest release of the package PKG."

  (define upstream-name
    (package->upstream-name pkg))

  (define version
    (latest-bioconductor-package-version upstream-name))

  (and version
       ;; Bioconductor does not provide signatures.
       (upstream-source
        (package (package-name pkg))
        (version version)
        (urls (bioconductor-uri upstream-name version))
        (input-changes
         (changed-inputs
          pkg
          (cran->guix-package upstream-name #:repo 'bioconductor))))))

(define (cran-package? package)
  "Return true if PACKAGE is an R package from CRAN."
  (and (string-prefix? "r-" (package-name package))
       ;; Check if the upstream name can be extracted from package uri.
       (package->upstream-name package)
       ;; Check if package uri(s) are prefixed by "mirror://cran".
       ((url-predicate (cut string-prefix? "mirror://cran" <>)) package)))

(define (bioconductor-package? package)
  "Return true if PACKAGE is an R package from Bioconductor."
  (let ((predicate (lambda (uri)
                     (and (string-prefix? "https://bioconductor.org" uri)
                          ;; Data packages are neither listed in SVN nor on
                          ;; the Github mirror, so we have to exclude them
                          ;; from the set of bioconductor packages that can be
                          ;; updated automatically.
                          (not (string-contains uri "/data/annotation/"))
                          ;; Experiment packages are in a separate repository.
                          (not (string-contains uri "/data/experiment/"))))))
    (and (string-prefix? "r-" (package-name package))
         ((url-predicate predicate) package))))

(define (bioconductor-data-package? package)
  "Return true if PACKAGE is an R data package from Bioconductor."
  (let ((predicate (lambda (uri)
                     (and (string-prefix? "https://bioconductor.org" uri)
                          (string-contains uri "/data/annotation/")))))
    (and (string-prefix? "r-" (package-name package))
         ((url-predicate predicate) package))))

(define (bioconductor-experiment-package? package)
  "Return true if PACKAGE is an R experiment package from Bioconductor."
  (let ((predicate (lambda (uri)
                     (and (string-prefix? "https://bioconductor.org" uri)
                          (string-contains uri "/data/experiment/")))))
    (and (string-prefix? "r-" (package-name package))
         ((url-predicate predicate) package))))

(define %cran-updater
  (upstream-updater
   (name 'cran)
   (description "Updater for CRAN packages")
   (pred cran-package?)
   (latest latest-cran-release)))

(define %bioconductor-updater
  (upstream-updater
   (name 'bioconductor)
   (description "Updater for Bioconductor packages")
   (pred bioconductor-package?)
   (latest latest-bioconductor-release)))

;;; cran.scm ends here
