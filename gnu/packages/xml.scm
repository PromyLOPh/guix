;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
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

(define-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public libxmlb
  (package
    (name "libxmlb")
    (version "0.1.15")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/hughsie/libxmlb")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mb73pnfwqc4mm0lm16yfn0lj495h8hcciprb2v6wgy3ifnnjxib"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("appstream-glib" ,appstream-glib)
       ("glib" ,glib)))
    (synopsis "Library to help create and query binary XML blobs")
    (description "Libxmlb library takes XML source, and converts it to a
structured binary representation with a deduplicated string table; where the
strings have the NULs included.  This allows an application to mmap the binary
XML file, do an XPath query and return some strings without actually parsing
the entire document.")
    (home-page "https://github.com/hughsie/libxmlb")
    (license license:lgpl2.1+)))

(define-public expat
  (package
    (name "expat")
    (version "2.4.1")
    (source (let ((dot->underscore (lambda (c) (if (char=? #\. c) #\_ c))))
              (origin
                (method url-fetch)
                (uri (list (string-append "mirror://sourceforge/expat/expat/"
                                          version "/expat-" version ".tar.xz")
                           (string-append
                            "https://github.com/libexpat/libexpat/releases/download/R_"
                            (string-map dot->underscore version)
                            "/expat-" version ".tar.xz")))
                (sha256
                 (base32
                  "0spvyb9d3hijs4ys3x64cfmilsynl8kv6clfahv8d4lvp86js0yg")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'install 'move-static-library
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out    (assoc-ref outputs "out"))
                            (static (assoc-ref outputs "static")))
                        (mkdir-p (string-append static "/lib"))
                        (link (string-append out "/lib/libexpat.a")
                              (string-append static "/lib/libexpat.a"))
                        (delete-file (string-append out "/lib/libexpat.a"))
                        (substitute* (string-append out "/lib/libexpat.la")
                          (("old_library=.*")
                           "old_library=''"))))))))
    (outputs '("out" "static"))
    (home-page "https://libexpat.github.io/")
    (synopsis "Stream-oriented XML parser library written in C")
    (description
     "Expat is an XML parser library written in C.  It is a
stream-oriented parser in which an application registers handlers for
things the parser might find in the XML document (like start tags).")
    (license license:expat)))

(define-public libebml
  (package
    (name "libebml")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.matroska.org/downloads/libebml/"
                           "libebml-" version ".tar.xz"))
       (sha256
        (base32 "1wmri5c94b02q2z32bqlpfs4vbw0n0c602321wigna2qw1y27is1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=YES")
       #:tests? #f))                    ; no test suite
    (home-page "https://matroska-org.github.io/libebml/")
    (synopsis "C++ library to parse EBML files")
    (description "libebml is a C++ library to read and write @dfn{EBML}
(Extensible Binary Meta Language) files.  EBML was designed to be a simplified
binary extension of XML for the purpose of storing and manipulating data in a
hierarchical form with variable field lengths.")
    (license license:lgpl2.1)))

(define-public libxml2
  (package
    (name "libxml2")
    (version "2.9.12")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxml2/libxml2-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "14hxwzmf5xqppx77z7i0ni9lpzg1a84dqpf8j8l1fvy570g6imn8"))
             (patches (search-patches "libxml2-parent-pointers.patch"
                                      "libxml2-terminating-newline.patch"
                                      "libxml2-xpath-recursion-limit.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "static" "doc"))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'use-other-outputs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((src (assoc-ref outputs "out"))
                            (doc (string-append (assoc-ref outputs "doc") "/share"))
                            (dst (string-append (assoc-ref outputs "static")
                                                "/lib")))
                        (mkdir-p doc)
                        (mkdir-p dst)
                        (for-each (lambda (dir)
                                    (rename-file (string-append src "/share/" dir)
                                                 (string-append doc "/" dir)))
                                  '("doc" "gtk-doc"))
                        (for-each (lambda (ar)
                                    (rename-file ar (string-append dst "/"
                                                                   (basename ar))))
                                  (find-files (string-append src "/lib") "\\.a$"))

                        ;; Remove reference to the static library from the .la
                        ;; file such that Libtool does the right thing when both
                        ;; the shared and static variants are available.
                        (substitute* (string-append src "/lib/libxml2.la")
                          (("^old_library='libxml2.a'") "old_library=''"))))))))
    (home-page "http://www.xmlsoft.org/")
    (synopsis "C parser for XML")
    (inputs `(("xz" ,xz)))
    (propagated-inputs `(("zlib" ,zlib))) ; libxml2.la says '-lz'.
    (native-inputs `(("perl" ,perl)))
    ;; $XML_CATALOG_FILES lists 'catalog.xml' files found in under the 'xml'
    ;; sub-directory of any given package.
    (native-search-paths (list (search-path-specification
                                (variable "XML_CATALOG_FILES")
                                (separator " ")
                                (files '("xml"))
                                (file-pattern "^catalog\\.xml$")
                                (file-type 'regular))))
    (search-paths native-search-paths)
    (description
     "Libxml2 is the XML C parser and toolkit developed for the Gnome
project (but it is usable outside of the Gnome platform).")
    (license license:x11)))

(define-public libxml2-xpath0
  (package/inherit libxml2
    (name "libxml2-xpath0")
    (source (origin
              (inherit (package-source libxml2))
              (patches (append (search-patches
                                "libxml2-xpath0-Add-option-xpath0.patch")
                               (origin-patches (package-source libxml2))))))
    (description
     "Libxml2-xpath0 is like libxml2 but with a patch applied that
provides an @code{--xpath0} option to @command{xmllint} that enables it
to output XPath results with a null delimiter.")))

(define-public libxlsxwriter
  (package
    (name "libxlsxwriter")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://github.com/jmcnamara/libxlsxwriter")
         (commit (string-append "RELEASE_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14c5rgx87nhzasr0j7mcfr1w7ifz0gmdiqy2xq59di5xvcdrpxpv"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled minizip source
        '(begin
           (delete-file-recursively "third_party/minizip")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "USE_STANDARD_TMPFILE=1"
             "USE_SYSTEM_MINIZIP=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (inputs
     `(("minizip" ,minizip)))
    (home-page "https://github.com/jmcnamara/libxlsxwriter")
    (synopsis "C library for creating Excel XLSX files")
    (description
     "Libxlsxwriter is a C library that can be used to write text, numbers,
formulas and hyperlinks to multiple worksheets in an Excel 2007+ XLSX file.")
    (license (list license:bsd-2
                   license:public-domain)))) ; third_party/md5

(define-public python-libxml2
  (package/inherit libxml2
    (name "python-libxml2")
    (source (origin
              (inherit (package-source libxml2))
              (patches
                (append (search-patches "python-libxml2-utf8.patch")
                        (origin-patches (package-source libxml2))))))
    (build-system python-build-system)
    (outputs '("out"))
    (arguments
     `(;; XXX: Tests are specified in 'Makefile.am', but not in 'setup.py'.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'build 'configure
          (lambda* (#:key inputs #:allow-other-keys)
            (chdir "python")
            (let ((glibc   (assoc-ref inputs ,(if (%current-target-system)
                                                  "cross-libc" "libc")))
                  (libxml2 (assoc-ref inputs "libxml2")))
              (substitute* "setup.py"
                ;; For 'libxml2/libxml/tree.h'.
                (("ROOT = r'/usr'")
                 (format #f "ROOT = r'~a'" libxml2))
                (("/opt/include")
                 (string-append glibc "/include"))
                (("distutils\\.core") "setuptools")
                ;; python-pypa-build does not like it if setup.py exits.
                (("sys\\.exit\\(0\\)") "")))
            #t)))))
    (inputs `(("libxml2" ,libxml2)))
    (synopsis "Python bindings for the libxml2 library")))

(define-public python2-libxml2
  (package-with-python2 python-libxml2))

(define-public libxslt
  (package
    (name "libxslt")
    (version "1.1.34")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxslt/libxslt-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0zrzz6kjdyavspzik6fbkpvfpbd25r2qg6py5nnjaabrsr3bvccq"))
             (patches (search-patches "libxslt-generated-ids.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-fuzz-tests
                    (lambda _
                      ;; Disable libFuzzer tests, because they require
                      ;; instrumentation builds of libxml2 and libxslt.
                      (substitute* "tests/Makefile"
                        (("exslt plugins fuzz")
                         "exslt plugins"))
                      #t)))))
    (home-page "http://xmlsoft.org/XSLT/index.html")
    (synopsis "C library for applying XSLT stylesheets to XML documents")
    (inputs `(("libgcrypt" ,libgcrypt)
              ("libxml2" ,libxml2)
              ("python" ,python-minimal-wrapper)
              ("zlib" ,zlib)
              ("xz" ,xz)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (description
     "Libxslt is an XSLT C library developed for the GNOME project.  It is
based on libxml for XML parsing, tree manipulation and XPath support.")
    (license license:x11)))

(define-public openjade
  (package
    (name "openjade")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/openjade/openjade/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l92sfvx1f0wmkbvzv1385y1gb3hh010xksi1iyviyclrjb7jb8x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--enable-spincludedir="
                            (assoc-ref %build-inputs "opensp")
                            "/include/OpenSP")
             (string-append "--enable-splibdir="
                            (assoc-ref %build-inputs "opensp") "/lib")
             ;; Workaround segfaults in OpenJade (see:
             ;; https://bugs.launchpad.net/ubuntu/+source/openjade/+bug/1869734).
             "CXXFLAGS=-O0")
       #:parallel-build? #f             ;build fails otherwise
       ;; The test suite fails with diff errors between the actual and
       ;; expected results, like: (char<? #\a #\A) returning #t rather than
       ;; #f (see: https://sourceforge.net/p/openjade/bugs/150/).
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-deprecated-getopt
           ;; See: https://sourceforge.net/p/openjade/bugs/140/.
           (lambda _
             (substitute* "msggen.pl"
               (("use POSIX;") "use POSIX;\nuse Getopt::Std;")
               (("do 'getopts.pl';") "")
               (("&Getopts") "getopts"))
             #t))
         (add-after 'replace-deprecated-getopt 'fix-locale-lookup
           ;; See: https://sourceforge.net/p/openjade/bugs/149/.
           (lambda _
             (substitute* "testsuite/expr-lang.dsl"
               (("\\(language \"EN\" \"US\"\\)")
                "(language \"EN\" \"US.UTF-8\")"))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             ;; TODO: Generate the manpage from source, with
             ;; openjade-bootstrap and jadetex.  See the file docsrc/Makefile.
             (let* ((out (assoc-ref outputs "out"))
                    (man1 (string-append out "/share/man/man1")))
               (install-file "docsrc/openjade.1" man1)
               #t)))
         (add-after 'install-doc 'install-dtds
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (dtd (string-append out "/sgml/dtd")))
              (mkdir-p dtd)
              (copy-recursively "dsssl" dtd)
              #t)))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? out #:allow-other-keys)
             (if tests?
                 (with-directory-excursion "testsuite"
                   (invoke "make"))
                 (format #t "test suite not run~%"))
             #t)))))
    (inputs
     `(("opensp" ,opensp)))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://openjade.sourceforge.net/")
    (synopsis "ISO/IEC 10179:1996 standard DSSSL language implementation")
    (description "OpenJade is an implementation of Document Style Semantics
and Specification Language (DSSSL), a style language to format SGML or XML
documents.  It contains backends for various formats such as RTF, HTML, TeX,
MIF, SGML2SGML, and FOT.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public perl-graph-readwrite
  (package
    (name "perl-graph-readwrite")
    (version "2.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/N/NE/NEILB/Graph-ReadWrite-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jlsg64pmy6ka5q5gy851nnyfgjzvhyxc576bhns3vi2x5ng07mh"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-graph" ,perl-graph)
       ("perl-parse-yapp" ,perl-parse-yapp)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-writer" ,perl-xml-writer)))
    (home-page "https://metacpan.org/release/Graph-ReadWrite")
    (synopsis "Modules for reading and writing directed graphs")
    (description "This is a collection of perl classes for reading and writing
directed graphs in a variety of file formats.  The graphs are represented in
Perl using Jarkko Hietaniemi's @code{Graph} classes.

There are two base classes. @code{Graph::Reader} is the base class for classes
which read a graph file and create an instance of the Graph class.
@code{Graph::Writer} is the base class for classes which take an instance of
the @code{Graph} class and write it out in a specific file format.")
    (license license:perl-license)))

(define-public perl-xml-atom
  (package
    (name "perl-xml-atom")
    (version "0.43")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                                  "XML-Atom-" version ".tar.gz"))
              (sha256
               (base32
                "0b8bpdnvz9sqwjhjkydbzy4karb7nn6i15b8g4mczrznlsb3hnaf"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-perl-search-path
           (lambda _
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t)))))
    (native-inputs
     ;; TODO package: perl-datetime-format-atom
     `(("perl-html-tagset" ,perl-html-tagset)
       ("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-class-data-inheritable" ,perl-class-data-inheritable)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-digest-sha1" ,perl-digest-sha1)
       ("perl-libwww" ,perl-libwww)
       ("perl-uri" ,perl-uri)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-xpath" ,perl-xml-xpath)))
    (home-page "https://metacpan.org/release/XML-Atom")
    (synopsis "Atom feed and API implementation")
    (description
     "Atom is a syndication, API, and archiving format for weblogs and other data.
@code{XML::Atom} implements the feed format as well as a client for the API.")
    (license license:perl-license)))

(define-public perl-xml-descent
  (package
    (name "perl-xml-descent")
    (version "1.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AN/ANDYA/"
                                  "XML-Descent-" version ".tar.gz"))
              (sha256
               (base32
                "0l5xmw2hd95ypppz3lyvp4sn02ccsikzjwacli3ydxfdz1bbh4d7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-test-differences" ,perl-test-differences)
       ("perl-xml-tokeparser" ,perl-xml-tokeparser)))
    (home-page "https://metacpan.org/release/XML-Descent")
    (synopsis "Recursive descent XML parsing")
    (description
     "The conventional models for parsing XML are either @dfn{DOM}
(a data structure representing the entire document tree is created) or
@dfn{SAX} (callbacks are issued for each element in the XML).

XML grammar is recursive - so it's nice to be able to write recursive
parsers for it.  @code{XML::Descent} allows such parsers to be created.")
    (license license:perl-license)))

(define-public perl-xml-parser
  (package
    (name "perl-xml-parser")
    (version "2.46")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TO/TODDR/XML-Parser-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0pai3ik47q7rgnix9644c673fwydz52gqkxr9kxwq765j4j36cfk"))))
    (build-system perl-build-system)
    (arguments `(#:make-maker-flags
                 (let ((expat (assoc-ref %build-inputs "expat")))
                   (list (string-append "EXPATLIBPATH=" expat "/lib")
                         (string-append "EXPATINCPATH=" expat "/include")))))
    (inputs `(("expat" ,expat)))
    (license license:perl-license)
    (synopsis "Perl bindings to the Expat XML parsing library")
    (description
     "This module provides ways to parse XML documents.  It is built on top of
XML::Parser::Expat, which is a lower level interface to James Clark's expat
library.  Each call to one of the parsing methods creates a new instance of
XML::Parser::Expat which is then used to parse the document.  Expat options
may be provided when the XML::Parser object is created.  These options are
then passed on to the Expat object on each parse call.  They can also be given
as extra arguments to the parse methods, in which case they override options
given at XML::Parser creation time.")
    (home-page "https://metacpan.org/release/XML-Parser")))

(define-public perl-xml-tokeparser
  (package
    (name "perl-xml-tokeparser")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/P/PO/PODMASTER/"
                                  "XML-TokeParser-" version ".tar.gz"))
              (sha256
               (base32
                "1hnpwb3lh6cbgwvjjgqzcp6jm4mp612qn6ili38adc9nhkwv8fc5"))))
    (build-system perl-build-system)
    (propagated-inputs `(("perl-xml-parser" ,perl-xml-parser)))
    (home-page "https://metacpan.org/release/XML-TokeParser")
    (synopsis "Simplified interface to XML::Parser")
    (description
     "@code{XML::TokeParser} provides a procedural (\"pull mode\") interface
to @code{XML::Parser} in much the same way that Gisle Aas'
@code{HTML::TokeParser} provides a procedural interface to @code{HTML::Parser}.
@code{XML::TokeParser} splits its XML input up into \"tokens\", each
corresponding to an @code{XML::Parser} event.")
    (license license:perl-license)))

(define-public perl-libxml
  (package
    (name "perl-libxml")
    (version "0.08")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/K/KM/KMACLEOD/libxml-perl-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1jy9af0ljyzj7wakqli0437zb2vrbplqj4xhab7bfj2xgfdhawa5"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-parser" ,perl-xml-parser)))
    (license license:perl-license)
    (synopsis "Perl modules for working with XML")
    (description
     "libxml-perl is a collection of smaller Perl modules, scripts, and
documents for working with XML in Perl.  libxml-perl software works in
combination with @code{XML::Parser}, PerlSAX, @code{XML::DOM},
@code{XML::Grove}, and others.")
    (home-page "https://metacpan.org/release/libxml-perl")))

(define-public perl-xml-libxml
  (package
    (name "perl-xml-libxml")
    (version "2.0134")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXML-" version ".tar.gz"))
       (sha256
        (base32
         "1ks69xymv6zkj7hvaymjvb78ch81abri7kg4zrwxhdfsqb8a9g7h"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
       ("perl-xml-sax" ,perl-xml-sax)))
    (inputs
     `(("libxml2" ,libxml2)))
    (home-page "https://metacpan.org/release/XML-LibXML")
    (synopsis "Perl interface to libxml2")
    (description "This module implements a Perl interface to the libxml2
library which provides interfaces for parsing and manipulating XML files.  This
module allows Perl programmers to make use of the highly capable validating
XML parser and the high performance DOM implementation.")
    (license license:perl-license)))

(define-public perl-xml-libxml-simple
  (package
    (name "perl-xml-libxml-simple")
    (version "1.01")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-LibXML-Simple-" version ".tar.gz"))
              (sha256
               (base32
                "19k50d80i9dipsl6ln0f4awv9wmdg0xm3d16z8mngmvh9c8ci66d"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-slurp-tiny" ,perl-file-slurp-tiny)
       ("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "https://metacpan.org/release/XML-LibXML-Simple")
    (synopsis "XML::LibXML based XML::Simple clone")
    (description
     "This package provides the same API as @code{XML::Simple} but is based on
@code{XML::LibXML}.")
    (license license:perl-license)))

(define-public perl-xml-libxslt
  (package
    (name "perl-xml-libxslt")
    (version "1.96")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXSLT-" version ".tar.gz"))
       (sha256
        (base32
         "0wyl8klgr65j8y8fzgwz9jlvfjwvxazna8j3dg9gksd2v973fpia"))))
    (build-system perl-build-system)
    (inputs
     `(("libxslt" ,libxslt)))
    (propagated-inputs
     `(("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "https://metacpan.org/release/XML-LibXSLT")
    (synopsis "Perl bindings to GNOME libxslt library")
    (description "This Perl module is an interface to the GNOME project's
libxslt library.")
    (license license:perl-license)))

(define-public perl-xml-namespacesupport
  (package
    (name "perl-xml-namespacesupport")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PERIGRIN/"
                           "XML-NamespaceSupport-" version ".tar.gz"))
       (sha256
        (base32
         "1vz5pbi4lm5fhq2slrs2hlp6bnk29863abgjlcx43l4dky2rbsa7"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-NamespaceSupport")
    (synopsis "XML namespace support class")
    (description "This module offers a simple to process namespaced XML
names (unames) from within any application that may need them.  It also helps
maintain a prefix to namespace URI map, and provides a number of basic
checks.")
    (license license:perl-license)))

(define-public perl-xml-rss
  (package
    (name "perl-xml-rss")
    (version "1.62")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                                  "XML-RSS-" version ".tar.gz"))
              (sha256
               (base32
                "0klb8ghd405pdkmn25lp3i4j2lfydz8w581sk51p3zy788s0c9yk"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-manifest" ,perl-test-manifest)
       ("perl-test-differences" ,perl-test-differences)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    ;; XXX: The test which uses this modules does not run, even when it is included
    ;; it is ignored. ("perl-test-trailingspace" ,perl-test-trailingspace)
    (inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-mail" ,perl-datetime-format-mail)
       ("perl-datetime-format-w3cdtf" ,perl-datetime-format-w3cdtf)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-xml-parser" ,perl-xml-parser)))
    (home-page "https://metacpan.org/release/XML-RSS")
    (synopsis "Creates and updates RSS files")
    (description
     "This module provides a basic framework for creating and maintaining
RDF Site Summary (RSS) files.  This distribution also contains many examples
that allow you to generate HTML from an RSS, convert between 0.9, 0.91, and
1.0 version, and more.")
    (license license:perl-license)))

(define-public perl-xml-sax
  (package
    (name "perl-xml-sax")
    (version "1.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-" version ".tar.gz"))
       (sha256
        (base32 "0am13vnv8qsjafr5ljakwnkhlwpk15sga02z8mxsg9is0j3w61j5"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
       ("perl-xml-sax-base" ,perl-xml-sax-base)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'install 'augment-path
                   ;; The install target tries to load the newly-installed
                   ;; XML::SAX module, but can't find it, so we need to tell
                   ;; perl where to look.
                   (lambda* (#:key outputs #:allow-other-keys)
                     (setenv "PERL5LIB"
                             (string-append (getenv "PERL5LIB") ":"
                                            (assoc-ref outputs "out")
                                            "/lib/perl5/site_perl"))
                     #t)))))
    (home-page "https://metacpan.org/release/XML-SAX")
    (synopsis "Perl API for XML")
    (description "XML::SAX consists of several framework classes for using and
building Perl SAX2 XML parsers, filters, and drivers.")
    (license license:perl-license)))

(define-public perl-xml-sax-base
  (package
    (name "perl-xml-sax-base")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-Base-" version ".tar.gz"))
       (sha256
        (base32
         "1l1ai9g1z11ja7mvnfl5mj346r13jyckbg9qlw6c2izglidkbjv6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-SAX-Base")
    (synopsis "Base class for SAX Drivers and Filters")
    (description "This module has a very simple task - to be a base class for
PerlSAX drivers and filters.  Its default behaviour is to pass the input
directly to the output unchanged.  It can be useful to use this module as a
base class so you don't have to, for example, implement the characters()
callback.")
    (license license:perl-license)))

(define-public perl-xml-simple
  (package
    (name "perl-xml-simple")
    (version "2.25")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GR/GRANTM/XML-Simple-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1y6vh328zrh085d40852v4ij2l4g0amxykswxd1nfhd2pspds7sk"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-sax" ,perl-xml-sax)))
    (license license:perl-license)
    (synopsis "Perl module for easy reading/writing of XML files")
    (description
     "The XML::Simple module provides a simple API layer on top of an
underlying XML parsing module (either XML::Parser or one of the SAX2
parser modules).")
    (home-page "https://metacpan.org/release/XML-Simple")))

(define-public perl-xml-regexp
  (package
    (name "perl-xml-regexp")
    (version "0.04")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TJ/TJMATHER/XML-RegExp-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0m7wj00a2kik7wj0azhs1zagwazqh3hlz4255n75q21nc04r06fz"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-xml-parser" ,perl-xml-parser)))
    (license license:perl-license)
    (synopsis "Perl regular expressions for XML tokens")
    (description
     "XML::RegExp contains regular expressions for the following XML tokens:
BaseChar, Ideographic, Letter, Digit, Extender, CombiningChar, NameChar,
EntityRef, CharRef, Reference, Name, NmToken, and AttValue.")
    (home-page "https://metacpan.org/release/XML-RegExp")))

(define-public perl-xml-dom
  (package
    (name "perl-xml-dom")
    (version "1.46")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TJ/TJMATHER/XML-DOM-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0phpkc4li43m2g44hdcvyxzy9pymqwlqhh5hwp2xc0cv8l5lp8lb"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libwww" ,perl-libwww)
       ("perl-libxml" ,perl-libxml)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-regexp" ,perl-xml-regexp)))
    (license license:perl-license)
    (synopsis
     "Perl module for building DOM Level 1 compliant document structures")
    (description
     "This module extends the XML::Parser module by Clark Cooper.  The
XML::Parser module is built on top of XML::Parser::Expat, which is a lower
level interface to James Clark's expat library.  XML::DOM::Parser is derived
from XML::Parser.  It parses XML strings or files and builds a data structure
that conforms to the API of the Document Object Model.")
    (home-page "https://metacpan.org/release/XML-DOM")))

(define-public perl-xml-compile-tester
  (package
    (name "perl-xml-compile-tester")
    (version "0.91")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Tester-" version ".tar.gz"))
              (sha256
               (base32
                "1drzwziwi96rfkh48qpw4l225mcbk8ppl2157nj92cslcpwwdk75"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-test-deep" ,perl-test-deep)))
    (home-page "https://metacpan.org/release/XML-Compile-Tester")
    (synopsis "XML::Compile related regression testing")
    (description
     "The @code{XML::Compile} module suite has extensive regression testing.
This module provide functions which simplify writing tests for
@code{XML::Compile} related distributions.")
    (license license:perl-license)))

(define-public perl-xml-compile
  (package
    (name "perl-xml-compile")
    (version "1.63")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-" version ".tar.gz"))
              (sha256
               (base32
                "0psr5pwsk2biz2bfkigmx04v2rfhs6ybwcfmcrrg7gvh9bpp222b"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-scalar-list-utils" ,perl-scalar-list-utils)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-types-serialiser" ,perl-types-serialiser)))
    (home-page "https://metacpan.org/release/XML-Compile")
    (synopsis "Compilation-based XML processing")
    (description
     "@code{XML::Compile} can be used to translate a Perl data-structure into
XML or XML into a Perl data-structure, both directions under rigid control by
a schema.")
    (license license:perl-license)))

(define-public perl-xml-compile-cache
  (package
    (name "perl-xml-compile-cache")
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Cache-" version ".tar.gz"))
              (sha256
               (base32
                "181qf1s7ymgi7saph3cf9p6dbxkxyh1ja23na4dchhi8v5mi66sr"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)
       ("perl-xml-libxml-simple" ,perl-xml-libxml-simple)))
    (home-page "https://metacpan.org/release/XML-Compile-Cache")
    (synopsis "Cache compiled XML translators")
    (description
     "This package provides methods to cache compiled XML translators.")
    (license license:perl-license)))

(define-public perl-xml-compile-soap
  (package
    (name "perl-xml-compile-soap")
    (version "3.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-SOAP-" version ".tar.gz"))
              (sha256
               (base32
                "0pkcph562l2ij7rlwlvm58v6y062qsbydfpaz2qnph2ixqy0xfd1"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-slurp-tiny" ,perl-file-slurp-tiny)
       ("perl-libwww" ,perl-libwww)
       ("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-cache" ,perl-xml-compile-cache)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)))
    (home-page "https://metacpan.org/release/XML-Compile-SOAP")
    (synopsis "Base-class for SOAP implementations")
    (description
     "This module provides a class to handle the SOAP protocol.  The first
implementation is @url{SOAP1.1,
http://www.w3.org/TR/2000/NOTE-SOAP-20000508/}, which is still most often
used.")
    (license license:perl-license)))

(define-public perl-xml-compile-wsdl11
  (package
    (name "perl-xml-compile-wsdl11")
    (version "3.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-WSDL11-" version ".tar.gz"))
              (sha256
               (base32
                "09ayl442hzvn97q4ghn5rz4r82dm9w3l69hixhb29h9xq9ysi7ba"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-cache" ,perl-xml-compile-cache)
       ("perl-xml-compile-soap" ,perl-xml-compile-soap)))
    (home-page "https://metacpan.org/release/XML-Compile-WSDL11")
    (synopsis "Create SOAP messages defined by WSDL 1.1")
    (description
     "This module understands WSDL version 1.1.  A WSDL file defines a set of
messages to be send and received over SOAP connections.  This involves
encoding of the message to be send into XML, sending the message to the
server, collect the answer, and finally decoding the XML to Perl.")
    (license license:perl-license)))

(define-public perl-xml-feed
  (package
    (name "perl-xml-feed")
    (version "0.63")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DA/DAVECROSS/"
                                  "XML-Feed-" version ".tar.gz"))
              (sha256
               (base32
                "04frqhikmyq0i9ldraisbvppyjhqg6gz83l2rqpmp4f2h9n9k2lw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-uri" ,perl-uri)
       ("perl-class-data-inheritable" ,perl-class-data-inheritable)))
    (propagated-inputs
     `(("perl-class-errorhandler" ,perl-class-errorhandler)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-flexible" ,perl-datetime-format-flexible)
       ("perl-datetime-format-iso8601" ,perl-datetime-format-iso8601)
       ("perl-datetime-format-mail" ,perl-datetime-format-mail)
       ("perl-datetime-format-natural" ,perl-datetime-format-natural)
       ("perl-datetime-format-w3cdtf" ,perl-datetime-format-w3cdtf)
       ("perl-feed-find" ,perl-feed-find)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-libwww-perl" ,perl-libwww)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-uri-fetch" ,perl-uri-fetch)
       ("perl-xml-atom" ,perl-xml-atom)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-rss" ,perl-xml-rss)))
    (home-page "https://metacpan.org/release/XML-Feed")
    (synopsis "XML Syndication Feed Support")
    (description "@code{XML::Feed} is a syndication feed parser for both RSS and
Atom feeds.  It also implements feed auto-discovery for finding feeds, given a URI.
@code{XML::Feed} supports the following syndication feed formats:
RSS 0.91, RSS 1.0, RSS 2.0, Atom")
    (license license:perl-license)))

(define-public perl-xml-xpath
  (package
    (name "perl-xml-xpath")
    (version "1.44")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MANWAR/"
                                  "XML-XPath-" version ".tar.gz"))
              (sha256
               (base32
                "03yxj7w5a43ibbpiqsvb3lswj2b71dydsx4rs2fw0p8n0l3i3j8w"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-path-tiny" ,perl-path-tiny)))
    (propagated-inputs
     `(("perl-xml-parser" ,perl-xml-parser)))
    (home-page "https://metacpan.org/release/XML-XPath")
    (synopsis "Parse and evaluate XPath statements")
    (description
     "This module aims to comply exactly to the @url{XPath specification,
https://www.w3.org/TR/xpath} and yet allow extensions to be added in
the form of functions.")
    (license license:perl-license)))

(define-public pugixml
  (package
    (name "pugixml")
    (version "1.11")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/zeux/pugixml/releases/download/v"
                          version "/pugixml-" version ".tar.gz"))
      (sha256
       (base32 "0b5apqiisq8yk51x0cwks4h2m0zd2zgjdy0w80qp9h5rccz3v496"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))                    ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://pugixml.org")
    (synopsis "Light-weight, simple and fast XML parser for C++ with XPath support")
    (description
     "pugixml is a C++ XML processing library, which consists of a DOM-like
interface with rich traversal/modification capabilities, a fast XML parser
which constructs the DOM tree from an XML file/buffer, and an XPath 1.0
implementation for complex data-driven tree queries.  Full Unicode support is
also available, with Unicode interface variants and conversions between
different Unicode encodings which happen automatically during
parsing/saving.")
    (license license:expat)))

(define-public python-pyxb
  (package
    (name "python-pyxb")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyXB" version))
              (sha256
               (base32
                "1d17pyixbfvjyi2lb0cfp0ch8wwdf44mmg3r5pwqhyyqs66z601a"))))
    (build-system python-build-system)
    (home-page "http://pyxb.sourceforge.net/")
    (synopsis "Python XML Schema Bindings")
    (description
     "PyXB (\"pixbee\") is a pure Python package that generates Python source
code for classes that correspond to data structures defined by XMLSchema.")
    (license (list license:asl2.0    ; Most files.
                   license:expat     ; pyxb/utils/six.py
                   license:gpl2      ; bundled jquery in doc is dual MIT/GPL2
                   license:psfl))))  ; pyxb/utils/activestate.py

(define-public python2-pyxb
  (package-with-python2 python-pyxb))

(define-public xmlto
  (package
    (name "xmlto")
    (version "0.0.28")
    (source
     (origin
      (method url-fetch)
      ;; The old source on fedorahosted.org is offline permanently:
      ;; <https://bugs.gnu.org/25989>
      (uri (string-append "mirror://debian/pool/main/x/xmlto/"
                          "xmlto_" version ".orig.tar.bz2"))
      (file-name (string-append name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0xhj8b2pwp4vhl9y16v3dpxpsakkflfamr191mprzsspg4xdyc0i"))))
    (build-system gnu-build-system)
    (arguments
     ;; Make sure the reference to util-linux's 'getopt' is kept in 'xmlto'.
     '(#:configure-flags (list (string-append "GETOPT="
                                              (assoc-ref %build-inputs
                                                         "util-linux")
                                              "/bin/getopt"))))
    (native-inputs
     `(("util-linux" ,util-linux)))
    (inputs
     `(("util-linux" ,util-linux)                 ; for 'getopt'
       ("libxml2" ,libxml2)                       ; for 'xmllint'
       ("libxslt" ,libxslt)))                     ; for 'xsltproc'
    (home-page "http://cyberelk.net/tim/software/xmlto/")
    (synopsis "Front-end to an XSL toolchain")
    (description
     "Xmlto is a front-end to an XSL toolchain.  It chooses an appropriate
stylesheet for the conversion you want and applies it using an external
XSL-T processor.  It also performs any necessary post-processing.")
    (license license:gpl2+)))

(define-public xmlsec
  (package
    (name "xmlsec")
    (version "1.2.32")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.aleksey.com/xmlsec/download/"
                                  "xmlsec1-" version ".tar.gz"))
              (sha256
               (base32
                "0hy0nwz57n9r5wwab9xa66gzwlwvzs54nhlfn3jh8q13acl710z3"))))
    (build-system gnu-build-system)
    (propagated-inputs                  ; according to xmlsec1.pc
     `(("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (inputs
     `(("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libltdl" ,libltdl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.aleksey.com/xmlsec/")
    (synopsis "XML Security Library")
    (description
     "The XML Security Library is a C library based on Libxml2.  It
supports XML security standards such as XML Signature, XML Encryption,
Canonical XML (part of Libxml2) and Exclusive Canonical XML (part of
Libxml2).")
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public xmlsec-nss
  (package/inherit xmlsec
    (name "xmlsec-nss")
    (native-inputs
     ;; For tests.
     `(("nss:bin" ,nss "bin")           ; for certutil
       ,@(package-native-inputs xmlsec)))
    (inputs
     `(("nss" ,nss)
       ("libltdl" ,libltdl)))
    (arguments
     ;; NSS no longer supports MD5 since 3.59, don't attempt to use it.
     '(#:configure-flags '("--disable-md5")))
    (synopsis "XML Security Library (using NSS instead of GnuTLS)")))

(define-public xmlsec-openssl
  (package/inherit xmlsec
    (name "xmlsec-openssl")
    (inputs
     `(("openssl" ,openssl)
       ("libltdl" ,libltdl)))
    (synopsis "XML Security Library (using OpenSSL instead of GnuTLS)")))

(define-public minixml
  (package
    (name "minixml")
    (version "3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/michaelrsweet/mxml/"
                                  "releases/download/v" version
                                  "/mxml-" version ".tar.gz"))
              (sha256
               (base32
                "1n1xzvhnsjsgsqaq1rg9zilwf0b2rydsadbxzy64z3lydwv7dybw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:tests? #f))                    ; tests are run during build
    (home-page "https://www.msweet.org/mxml/")
    (synopsis "Small XML parsing library")
    (description
     "Mini-XML is a small C library to read and write XML files and strings in
UTF-8 and UTF-16 encoding.")
    ;; LGPL 2.0+ with additional exceptions for static linking
    (license license:lgpl2.0+)))

;; TinyXML is an unmaintained piece of software, so the patches and build
;; system massaging have no upstream potential.
(define-public tinyxml
  (package
    (name "tinyxml")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/tinyxml/tinyxml/"
                                  version "/tinyxml_"
                                  (string-join (string-split version #\.) "_")
                                  ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14smciid19lvkxqznfig77jxn5s4iq3jpb47vh5a6zcaqp7gvg8m"))
              (patches (search-patches "tinyxml-use-stl.patch"))))
    (build-system gnu-build-system)
    ;; This library is missing *a lot* of the steps to make it usable, so we
    ;; have to add them here, like every other distro must do.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'build-shared-library
           (lambda _
             (invoke "g++" "-Wall" "-O2" "-shared" "-fpic"
                     "tinyxml.cpp" "tinyxmlerror.cpp"
                     "tinyxmlparser.cpp" "tinystr.cpp"
                     "-o" "libtinyxml.so")))
         (replace 'check
           (lambda _ (invoke "./xmltest")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (lib (string-append out "/lib"))
                    (pkgconfig (string-append out "/lib/pkgconfig"))
                    (doc (string-append out "/share/doc")))
               ;; Install libs and headers.
               (install-file "libtinyxml.so" lib)
               (install-file "tinystr.h" include)
               (install-file "tinyxml.h" include)
               ;; Generate and install pkg-config file.
               (mkdir-p pkgconfig)
               ;; Software such as Kodi expect this file to be present, but
               ;; it's not provided in the source code.
               (call-with-output-file (string-append pkgconfig "/tinyxml.pc")
                 (lambda (port)
                   (format port "prefix=~a
exec_prefix=${prefix}
libdir=${exec_prefix}/lib
includedir=${prefix}/include

Name: TinyXML
Description: A simple, small, C++ XML parser
Version: ~a
Libs: -L${libdir} -ltinyxml
Cflags: -I${includedir}
"
                           out ,version)))
               ;; Install docs.
               (mkdir-p doc)
               (copy-recursively "docs" (string-append doc "tinyxml"))
               #t))))))
    (synopsis "Small XML parser for C++")
    (description "TinyXML is a small and simple XML parsing library for the
C++ programming language.")
    (home-page "http://www.grinninglizard.com/tinyxml/index.html")
    (license license:zlib)))

(define-public tinyxml2
  (package
    (name "tinyxml2")
    (version "8.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leethomason/tinyxml2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0raa8r2hsagk7gjlqjwax95ib8d47ba79n91r4aws2zg8y6ssv1d"))))
    (build-system cmake-build-system)
    (synopsis "Small XML parser for C++")
    (description "TinyXML2 is a small and simple XML parsing library for the
C++ programming language.")
    (home-page "http://www.grinninglizard.com/tinyxml2/")
    (license license:zlib)))

(define-public xmlstarlet
 (package
   (name "xmlstarlet")
   (version "1.6.1")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/xmlstar/xmlstarlet/"
                          version "/xmlstarlet-" version ".tar.gz"))
      (sha256
       (base32
        "1jp737nvfcf6wyb54fla868yrr39kcbijijmjpyk4lrpyg23in0m"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
     (modify-phases %standard-phases
       (add-before 'check 'drop-failing-tests
         (lambda _
           ;; FIXME: Why are these tests failing.
           (substitute* "Makefile"
             (("^examples/schema1\\\\") "\\")
             (("^examples/valid1\\\\") "\\"))
           #t))
       (add-after 'install 'symlink-xmlstarlet
         (lambda* (#:key outputs #:allow-other-keys)
           ;; Other distros usually either rename or symlink the `xml' binary
           ;; as `xmlstarlet', let's do it as well for compatibility.
           (let* ((out (assoc-ref outputs "out"))
                  (bin (string-append out "/bin")))
             (symlink "xml" (string-append bin "/xmlstarlet"))
             #t))))))
   (inputs
    `(("libxslt" ,libxslt)
      ("libxml2" ,libxml2)))
   (home-page "http://xmlstar.sourceforge.net/")
   (synopsis "Command line XML toolkit")
   (description "XMLStarlet is a set of command line utilities which can be
used to transform, query, validate, and edit XML documents.  XPath is used to
match and extract data, and elements can be added, deleted or modified using
XSLT and EXSLT.")
   (license license:x11)))

(define-public html-xml-utils
 (package
   (name "html-xml-utils")
   (version "7.9")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://www.w3.org/Tools/HTML-XML-utils/html-xml-utils-"
            version ".tar.gz"))
      (sha256
       (base32 "0gs3xvdbzhk5k12i95p5d4fgkkaldnlv45sch7pnncb0lrpcjsnq"))))
   (build-system gnu-build-system)
   (home-page "https://www.w3.org/Tools/HTML-XML-utils/")
   (synopsis "Command line utilities to manipulate HTML and XML files")
   (description "HTML-XML-utils provides a number of simple utilities for
manipulating and converting HTML and XML files in various ways.  The suite
consists of the following tools:

@itemize
 @item @command{asc2xml} convert from @code{UTF-8} to @code{&#nnn;} entities
 @item @command{xml2asc} convert from @code{&#nnn;} entities to @code{UTF-8}
 @item @command{hxaddid} add IDs to selected elements
 @item @command{hxcite} replace bibliographic references by hyperlinks
 @item @command{hxcite} mkbib - expand references and create bibliography
 @item @command{hxclean} apply heuristics to correct an HTML file
 @item @command{hxcopy} copy an HTML file while preserving relative links
 @item @command{hxcount} count elements and attributes in HTML or XML files
 @item @command{hxextract} extract selected elements
 @item @command{hxincl} expand included HTML or XML files
 @item @command{hxindex} create an alphabetically sorted index
 @item @command{hxmkbib} create bibliography from a template
 @item @command{hxmultitoc} create a table of contents for a set of HTML files
 @item @command{hxname2id} move some @code{ID=} or @code{NAME=} from A
elements to their parents
 @item @command{hxnormalize} pretty-print an HTML file
 @item @command{hxnsxml} convert output of hxxmlns back to normal XML
 @item @command{hxnum} number section headings in an HTML file
 @item @command{hxpipe} convert XML to a format easier to parse with Perl or AWK
 @item @command{hxprintlinks} number links and add table of URLs at end of an HTML file
 @item @command{hxprune} remove marked elements from an HTML file
 @item @command{hxref} generate cross-references
 @item @command{hxselect} extract elements that match a (CSS) selector
 @item @command{hxtoc} insert a table of contents in an HTML file
 @item @command{hxuncdata} replace CDATA sections by character entities
 @item @command{hxunent} replace HTML predefined character entities to @code{UTF-8}
 @item @command{hxunpipe} convert output of pipe back to XML format
 @item @command{hxunxmlns} replace \"global names\" by XML Namespace prefixes
 @item @command{hxwls} list links in an HTML file
 @item @command{hxxmlns} replace XML Namespace prefixes by \"global names\"
@end itemize
")
   (license license:expat)))

(define-public xlsx2csv
  (package
    (name "xlsx2csv")
    (version "0.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dilshod/xlsx2csv")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "168dm6p7w6pvgd87yb9hcxv9y0liv6mxgril202nfva68cp8y939"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2               ; use python-2 for the test script
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (substitute* "test/run"
               ;; Run tests with `python' only.
               (("^(PYTHON_VERSIONS = ).*" all m) (string-append m "['']")))
             (invoke "test/run"))))))
    (home-page "https://github.com/dilshod/xlsx2csv")
    (synopsis "XLSX to CSV converter")
    (description
     "Xlsx2csv is a program to convert Microsoft Excel 2007 XML (XLSX and
XLSM) format spreadsheets into plaintext @dfn{comma separated values} (CSV)
files.  It is designed to be fast and to handle large input files.")
    (license license:gpl2+)))

(define-public python-defusedxml
  (package
    (name "python-defusedxml")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "defusedxml" version))
       (sha256
        (base32
         "1xbp8fivl3wlbyg2jrvs4lalaqv1xp9a9f29p75wdx2s2d6h717n"))))
    (build-system python-build-system)
    (home-page "https://bitbucket.org/tiran/defusedxml")
    (synopsis "XML bomb protection for Python stdlib modules")
    (description
     "Defusedxml provides XML bomb protection for Python stdlib modules.")
    (license license:psfl)))

(define-public python2-defusedxml
  (package-with-python2 python-defusedxml))

(define-public freexl
  (package
    (name "freexl")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.gaia-gis.it/gaia-sins/"
                                  "freexl-" version ".tar.gz"))
              (sha256
               (base32
                "08pwj17l0lgp6zms9nmpawdxpvhzrslklbd53s4b430k7mxbbs1x"))))
    (build-system gnu-build-system)
    (home-page "https://www.gaia-gis.it/fossil/freexl/index")
    (synopsis "Read Excel files")
    (description
     "FreeXL is a C library to extract valid data from within an Excel (.xls)
spreadsheet.")
    ;; Any of these licenses may be picked.
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1))))

(define-public xerces-c
  (package
    (name "xerces-c")
    (version "3.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xerces/c/3/sources/"
                                  "xerces-c-" version ".tar.xz"))
              (sha256
               (base32
                "0jf1khvlssg31vkxbc25dxjxcxm56xb8nywj1sypj6hxzjlrkz0j"))))
    (build-system gnu-build-system)
    (arguments
     (let ((system (or (%current-target-system)
                       (%current-system))))
       (if (string-prefix? "x86_64" system)
           '()
           '(#:configure-flags '("--disable-sse2")))))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://xerces.apache.org/xerces-c/")
    (synopsis "Validating XML parser library for C++")
    (description "Xerces-C++ is a validating XML parser written in a portable
subset of C++.  Xerces-C++ makes it easy to give your application the ability
to read and write XML data.  A shared library is provided for parsing,
generating, manipulating, and validating XML documents using the DOM, SAX, and
SAX2 APIs.")
    (license license:asl2.0)))

(define-public xlsxio
  (package
    (name "xlsxio")
    (version "0.2.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brechtsanders/xlsxio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jr6ggzhd8aakdvppcl8scy9j9jafg82zbzr4ih996sz8lrj90fn"))))
    (native-inputs
     `(("expat" ,expat)
       ("make" ,gnu-make)
       ("minizip" ,minizip)
       ("which" ,which)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "install"
                     (string-append
                      "PREFIX=" (assoc-ref outputs "out"))))))))
    (synopsis "C library for reading and writing .xlsx files")
    (description "XLSX I/O aims to provide a C library for reading and writing
.xlsx files.  The .xlsx file format is the native format used by Microsoft(R)
Excel(TM) since version 2007.")
    (home-page "https://github.com/brechtsanders/xlsxio")
    (license license:expat)))

(define-public java-simple-xml
  (package
    (name "java-simple-xml")
    (version "2.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/simple/simple-xml-"
                                  version ".zip"))
              (sha256
               (base32
                "0w19k1awslmihpwsxwjbg89hv0vjhk4k3i0vrfchy3mqknd988y5"))
              (patches (search-patches "java-simple-xml-fix-tests.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "build"
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "jar")))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://simple.sourceforge.net/")
    (synopsis "XML serialization framework for Java")
    (description "Simple is a high performance XML serialization and
configuration framework for Java.  Its goal is to provide an XML framework
that enables rapid development of XML configuration and communication systems.
This framework aids the development of XML systems with minimal effort and
reduced errors.  It offers full object serialization and deserialization,
maintaining each reference encountered.")
    (license license:asl2.0)))

(define-public perl-xml-xpathengine
  (package
    (name "perl-xml-xpathengine")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "XML-XPathEngine-" version ".tar.gz"))
              (sha256
               (base32
                "0r72na14bmsxfd16s9nlza155amqww0k8wsa9x2a3sqbpp5ppznj"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-XPathEngine")
    (synopsis "Re-usable XPath engine for DOM-like trees")
    (description
     "This module provides an XPath engine, that can be re-used by other
modules/classes that implement trees.

In order to use the XPath engine, nodes in the user module need to mimic DOM
nodes.  The degree of similitude between the user tree and a DOM dictates how
much of the XPath features can be used.  A module implementing all of the DOM
should be able to use this module very easily (you might need to add the
@code{cmp} method on nodes in order to get ordered result sets).")
    (license license:perl-license)))

(define-public perl-tree-xpathengine
  (package
    (name "perl-tree-xpathengine")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "Tree-XPathEngine-" version ".tar.gz"))
              (sha256
               (base32
                "1vbbw8wxm79r3xbra8narw1dqvm34510q67wbmg2zmj6zd1k06r9"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Tree-XPathEngine")
    (synopsis "Re-usable XPath engine")
    (description
     "This module provides an XPath engine, that can be re-used by other
module/classes that implement trees.  It is designed to be compatible with
@code{Class::XPath}, ie it passes its tests if you replace @code{Class::XPath}
by @code{Tree::XPathEngine}.")
    (license license:perl-license)))

(define-public perl-xml-filter-buffertext
  (package
    (name "perl-xml-filter-buffertext")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RB/RBERJON/"
                           "XML-Filter-BufferText-" version ".tar.gz"))
       (sha256
        (base32
         "0p5785c1dsk6kdp505vapb5h54k8krrz8699hpgm9igf7dni5llg"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-sax-base" ,perl-xml-sax-base)))
    (home-page "https://metacpan.org/release/XML-Filter-BufferText")
    (synopsis "Filter to put all characters() in one event")
    (description "This is a very simple filter.  One common cause of
grief (and programmer error) is that XML parsers aren't required to provide
character events in one chunk.  They can, but are not forced to, and most
don't.  This filter does the trivial but oft-repeated task of putting all
characters into a single event.")
    (license license:perl-license)))

(define-public perl-xml-sax-writer
  (package
    (name "perl-xml-sax-writer")
    (version "0.57")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/P/PE/PERIGRIN/"
                    "XML-SAX-Writer-" version ".tar.gz"))
              (sha256
               (base32
                "1w1cd1ybxdvhmnxdlkywi3x5ka3g4md42kyynksjc09vyizd0q9x"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libxml" ,perl-libxml)
       ("perl-xml-filter-buffertext" ,perl-xml-filter-buffertext)
       ("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
       ("perl-xml-sax-base" ,perl-xml-sax-base)))
    (home-page "https://metacpan.org/release/XML-SAX-Writer")
    (synopsis "SAX2 XML Writer")
    (description
     "This is an XML writer that understands SAX2.  It is based on
@code{XML::Handler::YAWriter}.")
    (license license:perl-license)))

(define-public perl-xml-handler-yawriter
  (package
    (name "perl-xml-handler-yawriter")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KR/KRAEHE/"
                           "XML-Handler-YAWriter-" version ".tar.gz"))
       (sha256
        (base32
         "11d45a1sz862va9rry3p2m77pwvq3kpsvgwhc5ramh9mbszbnk77"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libxml" ,perl-libxml)))
    (home-page "https://metacpan.org/release/XML-Handler-YAWriter")
    (synopsis "Yet another Perl SAX XML Writer")
    (description "YAWriter implements Yet Another @code{XML::Handler::Writer}.
It provides a flexible escaping technique and pretty printing.")
    (license license:perl-license)))

(define-public perl-xml-twig
  (package
    (name "perl-xml-twig")
    (version "3.52")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "XML-Twig-" version ".tar.gz"))
              (sha256
               (base32
                "1bc0hrz4jp6199hi29sdxmb9gyy45whla9hd19yqfasgq8k5ixzy"))))
    (build-system perl-build-system)
    (inputs
     `(("expat" ,expat)))
    (propagated-inputs
     `(("perl-html-tidy" ,perl-html-tidy)
       ("perl-html-tree" ,perl-html-tree)
       ("perl-io-captureoutput" ,perl-io-captureoutput)
       ("perl-io-string" ,perl-io-string)
       ("perl-io-stringy" ,perl-io-stringy)
       ("perl-libxml" ,perl-libxml)
       ("perl-xml-filter-buffertext" ,perl-xml-filter-buffertext)
       ("perl-xml-handler-yawriter" ,perl-xml-handler-yawriter)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-sax-writer" ,perl-xml-sax-writer)
       ("perl-xml-simple" ,perl-xml-simple)
       ("perl-xml-xpathengine" ,perl-xml-xpathengine)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-tree-xpathengine" ,perl-tree-xpathengine)))
    (home-page "https://metacpan.org/release/XML-Twig")
    (synopsis "Perl module for processing huge XML documents in tree mode")
    (description "@code{XML::Twig} is an XML transformation module.  Its
strong points: can be used to process huge documents while still being in tree
mode; not bound by DOM or SAX, so it is very perlish and offers a very
comprehensive set of methods; simple to use; DWIMs as much as possible.

What it doesn't offer: full SAX support (it can export SAX, but only reads
XML), full XPath support (unless you use @code{XML::Twig::XPath}), nor DOM
support.")
    (license license:perl-license)))

;; TODO: Debian builds several jars out of this: jaxp-1.4.jar,
;; xml-apis.jar and xml-apis-1.4.01.jar.
(define-public java-jaxp
  (package
    (name "java-jaxp")
    (version "1.4.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/source/"
                           "xml-commons-external-" version "-src.tar.gz"))
       (sha256
        (base32 "0rhq32a7dl9yik7zx9h0naz2iz068qgcdiayak91wp4wr26xhjyk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxp.jar"
       #:jdk ,icedtea-8
       #:source-dir ".."
       #:tests? #f)); no tests
    (home-page "http://xerces.apache.org/xml-commons/")
    (synopsis "Java XML parser and transformer APIs (DOM, SAX, JAXP, TrAX)")
    (description "Jaxp from the Apache XML Commons project is used by
the Xerces-J XML parser and Xalan-J XSLT processor and specifies these APIs:

@itemize
@item Document Object Model (DOM)
@item Simple API for XML (SAX)
@item Java APIs for XML Processing (JAXP)
@item Transformation API for XML (TrAX)
@item Document Object Model (DOM) Load and Save
@item JSR 206 Java API for XML Processing
@end itemize")
    (license (list license:asl2.0
                   license:w3c ;; Files under org.w3c
                   license:public-domain)))) ;; org.xml.sax

(define-public java-apache-xml-commons-resolver
  (package
    (name "java-apache-xml-commons-resolver")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/"
                           "xml-commons-resolver-" version ".tar.gz"))
       (sha256
        (base32 "1zhy4anc3fg9f8y348bj88vmab15aavrg6nf419ifb25asyygnsm"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file (find-files "." ".*\\.(jar|zip)"))
           #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append "xml-resolver.jar")
       #:tests? #f)); no tests
    (inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://xerces.apache.org/xml-commons/")
    (synopsis "Catalog-based entity and URI resolution")
    (description "The resolver class implements the full semantics of OASIS Technical
Resolution 9401:1997 (Amendment 2 to TR 9401) catalogs and the 06 Aug
2001 Committee Specification of OASIS XML Catalogs.

It also includes a framework of classes designed to read catalog files
in a number of formats:

@itemize
@item The plain-text flavor described by TR9401.
@item The XCatalog XML format defined by John Cowan
@item The XML Catalog format defined by the OASIS Entity Resolution
      Technical Committee.
@end itemize")
    (license license:asl2.0)))

;; Jaxen requires java-dom4j and java-xom that in turn require jaxen.
;; This package is a bootstrap version without dependencies on dom4j and xom.
(define java-jaxen-bootstrap
  (package
    (name "java-jaxen-bootstrap")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              ;; No release on github
              (uri (string-append "https://repo1.maven.org/maven2/jaxen/jaxen/"
                                  version "/jaxen-" version "-sources.jar"))
              (sha256
               (base32
                "18pa8mks3gfhazmkyil8wsp6j1g1x7rggqxfv4k2mnixkrj5x1kx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxen.jar"
       #:source-dir "src"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-dom4j
           (lambda _
             (delete-file-recursively "src/org/jaxen/dom4j")
             (delete-file-recursively "src/org/jaxen/xom")
             #t)))))
    (inputs
     `(("java-jdom" ,java-jdom)))
    (home-page "https://github.com/jaxen-xpath/jaxen")
    (synopsis "XPath library")
    (description "Jaxen is an XPath library written in Java.  It is adaptable
to many different object models, including DOM, XOM, dom4j, and JDOM.  It is
also possible to write adapters that treat non-XML trees such as compiled
Java byte code or Java beans as XML, thus enabling you to query these trees
with XPath too.")
    (license license:bsd-3)))

(define-public java-jaxen
  (package
    (inherit java-jaxen-bootstrap)
    (name "java-jaxen")
    (inputs
     `(("java-jdom" ,java-jdom)
       ("java-xom" ,java-xom)
       ("java-dom4j" ,java-dom4j)))))

(define-public java-xom
  (package
    (name "java-xom")
    (version "127")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elharo/xom")
                    (commit (string-append "XOM_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jh6y03g5zzdhsb5jm6ms1xnamr460qmn96y3w6aw0ikfwqlg0bq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xom.jar"
       #:jdk ,icedtea-8
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'configure 'fix-tagsoup-dep
           (lambda _
             ;; FIXME: Where is tagsoup source?
             (delete-file "src/nu/xom/tools/XHTMLJavaDoc.java")
             #t)))))
    (inputs
     `(("java-jdom" ,java-jdom)
       ("java-junit" ,java-junit)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-jaxen-bootstrap" ,java-jaxen-bootstrap)
       ("java-xerces" ,java-xerces)))
    (home-page "https://xom.nu/")
    (synopsis "XML Object Model")
    (description "XOM is a new XML Object Model for processing XML with Java 
that strives for correctness and simplicity.")
    ;; 2.1 only
    (license license:lgpl2.1)))

(define-public java-xsdlib
  (package
    (name "java-xsdlib")
    (version "2013.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/com/sun/msv/"
                                  "datatype/xsd/xsdlib/" version "/xsdlib-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "185i48p1xp09wbq03i9zgfl701qa262rq46yf4cajzmk3336kqim"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:jar-name "xsdlib.jar"
       #:jdk ,icedtea-8))
    (inputs
     `(("java-xerces" ,java-xerces)))
    (home-page (string-append "https://web.archive.org/web/20161127144537/"
                              "https://msv.java.net//"))
    (synopsis "Sun Multi-Schema Validator")
    (description "Xsdlib contains an implementation of sun.com.msv, an XML
validator.")
    (license license:bsd-2)))

(define-public java-xpp3
  (package
    (name "java-xpp3")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.extreme.indiana.edu/dist/"
                                  "java-repository/xpp3/distributions/xpp3-"
                                  version "_src.tgz"))
              (sha256
               (base32
                "1b99zrhyij5qwyhilyjdl1ykxvhk902vsvflh6gx4fir8hfvdl5p"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "build")))))
    (home-page "http://www.extreme.indiana.edu/xgws/xsoap/xpp/")
    (synopsis "Streaming pull XML parser")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors). This
package is a stable XmlPull parsing engine that is based on ideas from XPP
and in particular XPP2 but completely revised and rewritten to take the best
advantage of JIT JVMs.")
    (license (license:non-copyleft "file://LICENSE.txt"))))

(define-public java-xmlpull2
  (package
    (name "java-xmlpull2")
    (version "2.1.10")
    (source (origin
              (method url-fetch)
              ;; Unfortunately, archive.org does not have a copy of this file.
              (uri (string-append "https://ftp.fau.de/gentoo/distfiles/"
                                  "PullParser" version ".tgz"))
              (sha256
               (base32
                "1kw9nhyqb7bzhn2zjbwlpi5vp5rzj89amzi3hadw2acyh2dmd0md"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "impl"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "build/lib")))))
    (home-page (string-append "https://web.archive.org/web/20210225153105/"
                              "https://www.extreme.indiana.edu/"))
    (synopsis "Streaming pull XML parser")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors).  This
package is in maintenance mode.")
    (license (license:non-copyleft "file:///LICENSE.txt"))))

(define-public java-xmlpull-api-v1
  (package
    (name "java-xmlpull-api-v1")
    (version "1.1.3.4b")
    (source (origin
              ;; The package is originally from Extreme! Lab, but the website
              ;; is now gone.  This repositories contains the sources of the
              ;; latest version.
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aslom/xmlpull-api-v1")
                     ;; No releases, this is the latest commit
                     (commit "abeaa4aa87b2625af70c32f658f44e11355fe568")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15bdqxfncnakskna4m9gsh4f9iczxy83qxn2anqiqd15z406a5ih"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  (delete-file-recursively "lib")
                  (mkdir-p "lib")
                  ;; prevents a failure in "dist_lite"
                  (substitute* "build.xml"
                    (("README.html") "README.md"))))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "junit"
       #:build-target "dist"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (java (string-append out "/share/java"))
                    (project (string-append
                               "xmlpull_"
                               ,(string-join (string-split version #\.) "_"))))
               (mkdir-p doc)
               (copy-recursively (string-append "build/dist/" project "/doc/")
                                 doc)
               (mkdir-p java)
               (copy-file (string-append "build/dist/" project "/" project ".jar")
                          (string-append java "/" project ".jar")))
             )))))
    (home-page "https://github.com/aslom/xmlpull-api-v1")
    (synopsis "XML pull parsing API")
    (description "XmlPull v1 API is a simple to use XML pull parsing API.  XML
pull parsing allows incremental (sometimes called streaming) parsing of XML
where application is in control - the parsing can be interrupted at any given
moment and resumed when application is ready to consume more input.")
    (license license:public-domain)))

(define-public java-dom4j
  (package
    (name "java-dom4j")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dom4j/dom4j")
                    (commit (string-append "version-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q907srj9v4hwicpcrn4slyld5npf2jv7hzchsgrg29q2xmbwkdl"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "dom4j.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       ;; FIXME: Requires xalan, but xalan depends on java-cup which has a
       ;; dependency on itself through jflex.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-jaxen-sources
           ;; java-jaxen-bootstrap is not enough. These files have a circular
           ;; dependency and there is no subset of dom4j that would allow
           ;; breaking the circle.
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "jaxen-sources")
             (with-directory-excursion "jaxen-sources"
               (system* "jar" "xf" (assoc-ref inputs "java-jaxen-sources")))
             (mkdir-p "src/main/java/org/jaxen/dom4j")
             (copy-file "jaxen-sources/org/jaxen/dom4j/DocumentNavigator.java"
                        "src/main/java/org/jaxen/dom4j/DocumentNavigator.java")
             (copy-file "jaxen-sources/org/jaxen/dom4j/Dom4jXPath.java"
                        "src/main/java/org/jaxen/dom4j/Dom4jXPath.java")
             #t))
         (add-before 'build 'fix-old-xpp2
           (lambda _
             ;; This package normally depends on xpp2 2.0, but version 2.1.10
             ;; is the only version whose source code is published.
             (substitute* "src/main/java/org/dom4j/xpp/ProxyXmlStartTag.java"
               (("public void resetStartTag")
                "public boolean removeAttributeByRawName(String name) {\n
  return false;\n
}\n
public boolean removeAttributeByName(String name, String name2) {\n
  return false;\n
}\n\npublic void resetStartTag")
               (("Atttribute") "Attribute"))
             #t)))))
    (inputs
     `(("java-jaxen-bootstrap" ,java-jaxen-bootstrap)
       ("java-jaxen-sources" ,(package-source java-jaxen-bootstrap))
       ("java-xmlpull2" ,java-xmlpull2)
       ("java-xpp3" ,java-xpp3)
       ("java-xsdlib" ,java-xsdlib)))
    (native-inputs
     `(("java-testng" ,java-testng)
       ("java-xerces" ,java-xerces)))
    (home-page "https://dom4j.github.io/")
    (synopsis "Flexible XML framework for Java")
    (description "Dom4j is a flexible XML framework for Java.  DOM4J works
with DOM, SAX, XPath, and XSLT.  It can parse large XML documents with very
low memory footprint.")
    ;; some BSD-like 5-clause license
    (license (license:non-copyleft "file://LICENSE"))))

(define-public java-kxml2
  (package
    (name "java-kxml2")
    (version "2.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stefanhaustein/kxml2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g6d8c9r9sh3x04sf4wdpgwvhkqvk11k3kq9skx91i60h4vn01hg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "kxml2.jar"
       #:source-dir "src/main/java"
       #:test-include (list "TestWb.java")
       ;; Test failure: it was expected to get an XML entity but got the
       ;; equivalent Unicode character instead.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-xpp3" ,java-xpp3)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://kxml.org")
    (synopsis "XML pull parser")
    (description "kXML is a small XML pull parser, specially designed for
constrained environments such as Applets, Personal Java or devices compliant
with the Mobile Information Device Profile (MIDP).")
    (license license:expat)))

(define-public java-stax
  (package
    (name "java-stax")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/stax/stax/"
                                  version "/stax-" version "-sources.jar"))
              (sha256
               (base32
                "04ba4qvbrps45j8bldbakxq31k7gjlsay9pppa9yn13fr00q586z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "stax.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-utf8
           (lambda _
             ;; This file is ISO-8859-1 but java expects UTF-8.
             ;; Remove special characters in comments.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/com/wutka/dtd/Scanner.java"
                 (("//.*") "\n")))
             #t)))))
    (home-page "https://repo1.maven.org/maven2/stax/stax/")
    (synopsis "Streaming API for XML")
    (description "This package provides the reference implementation of the
@dfn{Streaming API for XML} (StAX).  It is used for streaming XML data to
and from a Java application.  It provides a standard pull parser interface.")
    (license license:asl2.0)))

(define-public java-jettison
  (package
    (name "java-jettison")
    (version "1.3.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/codehaus/jettison")
                    (commit (string-append "jettison-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15sydmi5chdh4126qc7v8bsrp7fp4ldaya8a05iby4pq2324q0qw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jettison.jar"
       #:source-dir "src/main/java"
       #:test-exclude (list "**/Abstract*.java"
                            ;; Abstract classes
                            "**/DOMTest.java"
                            "**/BadgerFishDOMTest.java"
                            "**/MappedDOMTest.java")))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://github.com/codehaus/jettison")
    (synopsis "StAX implementation for JSON")
    (description "Jettison is a Java library for converting XML to JSON and
vice-versa with the help of the @dfn{Streaming API for XML} (StAX).  It
implements @code{XMLStreamWriter} and @code{XMLStreamReader} and supports
@code{Mapped} and @code{BadgerFish} conventions.")
    (license license:asl2.0)))

(define-public java-jdom2
  (package
    (name "java-jdom")
    (version "2.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hunterhacker/jdom")
                    (commit (string-append "JDOM-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14vv1kxrsdvwi4cz3rx6r48w5y6fvk9cymil8qhvxwp56xxrgxiq"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:tests? #f                ; tests are run as part of the build process
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "build")))))
    (home-page "http://jdom.org/")
    (synopsis "Access, manipulate, and output XML data")
    (description "Jdom is a Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

(define-public java-xstream
  (package
    (name "java-xstream")
    (version "1.4.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/x-stream/xstream")
             (commit (string-append
                      "XSTREAM_"
                      (string-map (lambda (x) (if (eq? x #\.) #\_ x))
                                  version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16k2mc63h2fw7lxv74qmhg4p8q9hfrw114daa6nxwnpv08cnq755"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xstream.jar"
       ;; FIXME: Tests are not in a java subdirectory as assumed by ant-build-system.
       #:tests? #f
       #:jdk ,icedtea-8
       #:source-dir "xstream/src/java"))
    (inputs
     `(("java-jdom" ,java-jdom)
       ("java-jdom2" ,java-jdom2)
       ("java-cglib" ,java-cglib)
       ("java-joda-time" ,java-joda-time)
       ("java-jettison" ,java-jettison)
       ("java-xom" ,java-xom)
       ("java-mxparser" ,java-mxparser)
       ("java-xpp3" ,java-xpp3)
       ("java-dom4j" ,java-dom4j)
       ("java-stax2-api" ,java-stax2-api)
       ("java-woodstox-core" ,java-woodstox-core)
       ("java-kxml2" ,java-kxml2)
       ("java-stax" ,java-stax)))
    (home-page "https://x-stream.github.io")
    (synopsis "XML serialization library")
    (description "XStream is a simple library to serialize Java objects to XML
and back again.")
    (license license:bsd-3)))

(define-public java-mxparser
  (package
    (name "java-mxparser")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/x-stream/mxparser")
              (commit (string-append "v-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i3jrjbz4hgf62fm1ix7nlcmhi4kcv4flqsfvh7a3l2v7nsp5ryb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "mxparser.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (propagated-inputs
     `(("java-xmlpull-api-v1" ,java-xmlpull-api-v1)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://github.com/x-stream/mxparser")
    (synopsis "Streaming pull XML parser forked from @code{java-xpp3}")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors). This
package is a stable XmlPull parsing engine that is based on ideas from XPP
and in particular XPP2 but completely revised and rewritten to take the best
advantage of JIT JVMs.

MXParser is a fork of xpp3_min 1.1.7 containing only the parser with merged
changes of the Plexus fork. It is an implementation of the XMLPULL V1 API
(parser only).")
    (license (license:non-copyleft "file://LICENSE.txt"))))

(define-public xmlrpc-c
  (package
    (name "xmlrpc-c")
    (version "1.43.08")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/xmlrpc-c/Xmlrpc-c%20Super%20Stable/"
                                 version "/xmlrpc-c-" version ".tgz"))
             (sha256
              (base32
               "18zwbj6i2hpcn5riiyp8i6rml0sfv60dd7phw1x8g4r4lj2bbxf9"))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)))
    (native-inputs
     `(;; For tools, if ever needed.
       ("perl" ,perl)))
    (arguments
     `(#:make-flags ; Add $libdir to the RUNPATH of all the executables.
       (list (string-append "LDFLAGS_PERSONAL=-Wl,-rpath=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-/bin/sh-in-tests
           (lambda _
             (substitute* "GNUmakefile"
               (("#! /bin/sh") (which "sh")))
             #t)))))
    (home-page "http://xmlrpc-c.sourceforge.net/")
    (synopsis "Lightweight RPC library based on XML and HTTP")
    (description
     "XML-RPC is a quick-and-easy way to make procedure calls over the Internet.
It converts the procedure call into an XML document, sends it to a remote
server using HTTP, and gets back the response as XML.  This library provides a
modular implementation of XML-RPC for C and C++.")
    (license (list license:psfl license:expat))))

(define-public opensp
  (package
    (name "opensp")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/openjade/opensp/"
                                  version "/OpenSP-" version ".tar.gz"))
              (sha256
               (base32
                "1khpasr6l0a8nfz6kcf3s81vgdab8fm2dj291n5r2s53k228kx2p"))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)             ;for XML_CATALOG_DIR
       ("xmlto" ,xmlto)
       ;; Dependencies to regenerate the 'configure' script.
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)))
    (arguments
     `( ;; Note: we cannot use '--enable-full-doc-build' as this would require
       ;; Openjade, which in turn requires this package.

       ;; Skip the tests that are known to fail (see:
       ;; https://sourceforge.net/p/openjade/mailman/message/6182316/)
       #:make-flags '("TESTS_THAT_FAIL=")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                          "/xml/dtd/docbook"))
                   (xsldoc (string-append (assoc-ref inputs "docbook-xsl")
                                          "/xml/xsl/docbook-xsl-"
                                          ,(package-version docbook-xsl))))
               (substitute* (find-files "docsrc" "\\.xml$")
                 (("/usr/share/sgml/docbook/xml-dtd-4.1.2") xmldoc)
                 (("http://.*/docbookx\\.dtd")
                  (string-append xmldoc "/docbookx.dtd")))
               #t)))
         (add-after 'patch-docbook-paths 'delete-configure
           ;; The configure script in the release was made with an older
           ;; Autoconf and lacks support for the `--docdir' option.
           (lambda _
             (delete-file "configure")
             #t))
         (add-after 'delete-configure 'honor-docdir
           ;; docdir is not honored due to being hardcoded in the various
           ;; Makefile.am (see: https://sourceforge.net/p/openjade/bugs/147/).
           (lambda _
             (substitute* '("Makefile.am" "doc/Makefile.am" "docsrc/Makefile.am")
               (("^docdir = .*") "docdir = @docdir@\n"))
             #t))
         (add-after 'delete-configure 'fix-tests-makefile.am
           ;; Remove the trailing $(SHELL) from the TESTS_ENVIRONMENT variable
           ;; definition. Otherwise, when targets are built using
           ;; "$(am__check_pre) $(LOG_DRIVER) [...]", there would be two
           ;; $(SHELL) expansion which fails the build.
           (lambda _
             (substitute* "tests/Makefile.am"
               (("^\tOSGMLNORM=`echo osgmlnorm\\|sed '\\$\\(transform\\)'`\\\\")
                "\tOSGMLNORM=`echo osgmlnorm|sed '$(transform)'`")
               (("^\t\\$\\(SHELL\\)\n") ""))
             #t)))))
    ;; $SGML_CATALOG_FILES lists 'catalog' or 'CATALOG' or '*.cat' files found
    ;; under the 'sgml' sub-directory of any given package.
    (native-search-paths (list (search-path-specification
                                (variable "SGML_CATALOG_FILES")
                                (separator ":")
                                (files '("sgml"))
                                (file-pattern "^catalog$|^CATALOG$|^.*\\.cat$")
                                (file-type 'regular))))
    (home-page "http://openjade.sourceforge.net/")
    (synopsis "Suite of SGML/XML processing tools")
    (description "OpenSP is an object-oriented toolkit for SGML parsing and
entity management.  It is a fork of James Clark's SP suite.  The tools it
contains can be used to parse, validate, and normalize SGML and XML files.
The central program included in this package is @code{onsgmls}, which replaces
@code{sgmls}, @code{ospam}, @code{ospent}, @code{osgmlnorm}, and @code{osx}.")
    (license
     ;; expat license with added clause regarding advertising
     (license:non-copyleft
      "file://COPYING"
      "See COPYING in the distribution."))))

(define-public python-elementpath
  (package
    (name "python-elementpath")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "elementpath" version))
       (sha256
        (base32
         "1kxx573ywqfh6j6aih2i6hhsya6kz79qq4bgz6yskwk6b18jyr8z"))))
    (build-system python-build-system)
    ;; The test suite is not run, to avoid a dependency cycle with
    ;; python-xmlschema.
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/sissaschool/elementpath")
    (synopsis
     "XPath 1.0/2.0 parsers and selectors for ElementTree and lxml")
    (description
     "The proposal of this package is to provide XPath 1.0 and 2.0 selectors
for Python's ElementTree XML data structures, both for the standard
ElementTree library and for the @uref{http://lxml.de, lxml.etree} library.

For lxml.etree this package can be useful for providing XPath 2.0 selectors,
because lxml.etree already has its own implementation of XPath 1.0.")
    (license license:expat)))

(define-public python-lxml
  (package
    (name "python-lxml")
    (version "4.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lxml" version))
       (sha256
        (base32 "0s14r1w2x9sdlcsw8mxiqgw4rz5zs5lpqpxrfyn4a1mkndqqbdrr"))
       ;; Adapt a test to libxml2 2.9.12, taken from this commit:
       ;; https://github.com/lxml/lxml/commit/852ed1092bd80b6b9a51db24371047e
       (modules '((guix build utils)))
       (snippet
        '(substitute* "src/lxml/tests/test_etree.py"
             (("self\\.assertEqual\\(\\{'hha': None\\}, el\\.nsmap\\)")
              "self.assertEqual({}, el.nsmap)")))))
    (build-system python-build-system)
    (arguments
      `(#:phases (modify-phases %standard-phases
                                ;; Expects setup.py to be called from source root.
         (add-after 'unpack 'patch-get-base-dir
                    (lambda _
                      (substitute* "versioninfo.py"
                                   (("os.path.abspath\\(os.path.dirname\\(sys.argv\\[0\\]\\)\\)") "'.'"))
                      #t))
         (replace 'check
                  (lambda _
                    (invoke "make" "test"))))))
    (inputs
     `(("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (home-page "https://lxml.de/")
    (synopsis "Python XML processing library")
    (description
     "The lxml XML toolkit is a Pythonic binding for the C libraries
libxml2 and libxslt.")
    (license license:bsd-3))) ; and a few more, see LICENSES.txt

(define-public python2-lxml
  (package-with-python2 python-lxml))

(define-public python-xmlschema
  (package
    (name "python-xmlschema")
    (version "1.2.5")
    (source (origin
              ;; Unit tests are not distributed with the PyPI archive.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sissaschool/xmlschema")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rsa75x86gdjalvy4riq7613szb616hff80crx006chyppzdkxmq"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 ;; Disable test_export_remote__issue_187, which is known to
                 ;; fail (see:
                 ;; https://github.com/sissaschool/xmlschema/issues/206).
                 (invoke "python" "-m" "unittest" "-v"
                         "-k" "not test_export_remote__issue_187")
                 (format #t "test suite not run~%")))))))
    (native-inputs
     `(("python-lxml" ,python-lxml)))   ;for tests
    (propagated-inputs
     `(("python-elementpath" ,python-elementpath)))
    (home-page "https://github.com/sissaschool/xmlschema")
    (synopsis "XML Schema validator and data conversion library")
    (description
     "The @code{xmlschema} library is an implementation of
@url{https://www.w3.org/2001/XMLSchema, XML Schema} for Python.  It has
full support for the XSD 1.0 and 1.1 standards, an XPath-based API for
finding schema's elements and attributes; and can encode and decode
XML data to JSON and other formats.")
    (license license:expat)))

(define-public python-xmltodict
  (package
    (name "python-xmltodict")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xmltodict" version))
       (sha256
        (base32
         "08cadlb9vsb4pmzc99lz3a2lx6qcfazyvgk10pcqijvyxlwcdn2h"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/martinblech/xmltodict")
    (synopsis "Work with XML like you are working with JSON")
    (description "This package provides a Python library to convert XML to
@code{OrderedDict}.")
    (license license:expat)))
