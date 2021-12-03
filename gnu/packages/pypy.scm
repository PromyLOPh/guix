;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2017, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Omar Radwan <toxemicsquire4@gmail.com>
;;; Copyright © 2015 Pierre-Antoine Rault <par@rigelk.eu>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016, 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Daniel Pimentel <d4n1@d4n1.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016, 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016–2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017, 2018, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017, 2018 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2017, 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018 Ethan R. Jones <ethanrjones97@gmail.com
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Vijayalakshmi Vedantham <vijimay12@gmail.com>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2016, 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2019, 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Luther Thompson <lutheroto@gmail.com>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
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

(define-module (gnu packages pypy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public pypy3
  (package
    (name "pypy3")
    (version "7.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.python.org/pypy/"
                                  "pypy3.7-v" version "-src.tar.bz2"))
              (sha256
               (base32
                "18lrdmpcczlbk3cfarkgwqdmilrybz56i1dafk8dkjlyk90gw86r"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python-2" ,python-2)
       ("pkg-config" ,pkg-config)
       ("tar" ,tar)                     ; Required for package.py
       ("python2-pycparser" ,python2-pycparser)
       ("python2-hypothesis" ,python2-hypothesis)
       ("nss-certs" ,nss-certs)         ; For ssl tests
       ("gzip" ,gzip)))
    (inputs
     `(("libffi" ,libffi)
       ("zlib" ,zlib)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("expat" ,expat)
       ("bzip2" ,bzip2)
       ("sqlite" ,sqlite)
       ("gdbm" ,gdbm)
       ("tcl" ,tcl)
       ("tk" ,tk)
       ("glibc" ,glibc)
       ("xz" ,xz)))                     ; liblzma
    (arguments
     `(#:tests? #f                     ;FIXME: 43 out of 364 tests are failing
       #:modules ((ice-9 ftw) (ice-9 match)
                  (guix build utils) (guix build gnu-build-system))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-source
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (substitute* '("rpython/rlib/clibffi.py")
                        ;; find_library does not work for libc
                        (("ctypes\\.util\\.find_library\\('c'\\)") "'libc.so'"))
                      (substitute* '("lib_pypy/cffi/_pycparser/ply/cpp.py")
                        ;; Make reproducible (XXX: unused?)
                        (("time\\.localtime\\(\\)") "time.gmtime(0)"))
                      (substitute* '("pypy/module/sys/version.py")
                        ;; Make reproducible
                        (("t\\.gmtime\\(\\)") "t.gmtime(0)"))
                      (substitute* '("lib_pypy/_tkinter/tklib_build.py")
                        ;; Link to versioned libtcl and libtk
                        (("linklibs = \\['tcl', 'tk'\\]")
                         "linklibs = ['tcl8.6', 'tk8.6']")
                        (("incdirs = \\[\\]")
                         (string-append "incdirs = ['"
                                        (assoc-ref inputs "tcl")
                                        "/include', '"
                                        (assoc-ref inputs "tk")
                                        "/include']")))
                      (substitute* '("lib_pypy/_curses_build.py")
                        ;; Find curses
                        (("/usr/local") (assoc-ref inputs "ncurses")))
                      (substitute* '("lib_pypy/_dbm.py")
                        ;; Use gdbm compat library, so we don’t need to pull
                        ;; in bdb.
                        (("ctypes.util.find_library\\('db'\\)")
                         (format #f "'~a/lib/libgdbm_compat.so'"
                                 (assoc-ref inputs "gdbm"))))
                      (substitute* '("lib_pypy/_sqlite3_build.py")
                        ;; Always use search paths
                        (("sys\\.platform\\.startswith\\('freebsd'\\)") "True")
                        ;; Find sqlite3
                        (("/usr/local") (assoc-ref inputs "sqlite"))
                        (("libname = 'sqlite3'")
                         (string-append "libname = '"
                                        (assoc-ref inputs "sqlite")
                                        "/lib/libsqlite3.so.0'")))
                      (substitute* '("lib-python/3/subprocess.py")
                        ;; Fix shell path
                        (("/bin/sh")
                         (search-input-file inputs "/bin/sh")))
                      (substitute* '("lib-python/3/distutils/unixccompiler.py")
                        ;; gcc-toolchain does not provide symlink cc -> gcc
                        (("\"cc\"") "\"gcc\""))))
                  (add-after
                      'unpack 'set-source-file-times-to-1980
                    ;; copied from python package, required by zip testcase
                    (lambda _
                      (let ((circa-1980 (* 10 366 24 60 60)))
                        (ftw "." (lambda (file stat flag)
                                   (utime file circa-1980 circa-1980)
                                   #t)))))
                  (replace 'build
                    (lambda* (#:key inputs #:allow-other-keys)
                      (with-directory-excursion "pypy/goal"
                        ;; Build with jit optimization.
                        (invoke "python2"
                                "../../rpython/bin/rpython"
                                (string-append "--make-jobs="
                                               (number->string (parallel-job-count)))
                                "-Ojit"
                                "targetpypystandalone"
                                "--allworkingmodules"))
                      ;; Build c modules and package everything, so tests work.
                      (with-directory-excursion "pypy/tool/release"
                        (invoke "python2" "package.py"
                                "--archive-name" "pypy-dist"
                                "--builddir" (getcwd)))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            (setenv "HOME" "/tmp") ; test_with_pip tries to
                                        ; access ~/.cache/pip
                            ;; Run library tests only (no interpreter unit
                            ;; tests). This is what Gentoo does.
                            (invoke
                             "python2"
                             "pypy/test_all.py"
                             "--pypy=pypy/tool/release/pypy-dist/bin/pypy3"
                             "lib-python"))
                          (format #t "test suite not run~%"))))
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin-pypy3 (string-append out "/bin/pypy3"))
                             (shebang-match-python "#!.+/bin/python")
                             (shebang-pypy3 (string-append "#!" bin-pypy3))
                             (dist-dir "pypy/tool/release/pypy-dist"))
                        (with-directory-excursion dist-dir
                          ;; Delete test data.
                          (for-each
                           (lambda (x)
                             (delete-file-recursively (string-append
                                                       "lib-python/3/" x)))
                           '("tkinter/test"
                             "test"
                             "sqlite3/test"
                             "lib2to3/tests"
                             "idlelib/idle_test"
                             "distutils/tests"
                             "ctypes/test"
                             "unittest/test"))
                          ;; Patch shebang referencing python2
                          (substitute* '("lib-python/3/cgi.py"
                                         "lib-python/3/encodings/rot_13.py")
                            ((shebang-match-python) shebang-pypy3))
                          (with-fluids ((%default-port-encoding "ISO-8859-1"))
                            (substitute* '("lib_pypy/_md5.py"
                                           "lib_pypy/_sha1.py")
                              ((shebang-match-python) shebang-pypy3))))
                        (copy-recursively dist-dir out)))))))
    (home-page "https://www.pypy.org/")
    (synopsis "Python implementation with just-in-time compilation")
    (description "PyPy is a faster, alternative implementation of the Python
programming language employing a just-in-time compiler.  It supports most
Python code natively, including C extensions.")
    (license (list license:expat     ; pypy itself; _pytest/
                   license:psfl      ; python standard library in lib-python/
                   license:asl2.0    ; dotviewer/font/ and some of lib-python/
                   license:gpl3+ ; ./rpython/rlib/rvmprof/src/shared/libbacktrace/dwarf2.*
                   license:bsd-3 ; lib_pypy/cffi/_pycparser/ply/
                   (license:non-copyleft
                    "http://www.unicode.org/copyright.html")))))

