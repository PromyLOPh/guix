;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages python-commencement)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public python-toolchain
  (package
    (name "python-toolchain")
    (version (package-version python))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (srfi srfi-1)
                                (srfi srfi-26)
                                (guix build union))

                   (let ((out (assoc-ref %outputs "out")))
                     (union-build out (filter-map (match-lambda
                                                ((_ . directory) directory))
                                              %build-inputs))
                     #t))))
    (inputs
     `(("python" ,python-wrapper)
       ("python-setuptools" ,python-setuptools)
       ("python-pip" ,python-pip)
       ("python-virtualenv" ,python-virtualenv)))
    (native-search-paths
     (package-native-search-paths python))
    (search-paths
     (package-search-paths python))
    (license (package-license python)) ; XXX
    (synopsis "Python toolchain")
    (description
     "Python toolchain including Python itself, setuptools and pip.  Use this
package if you need a fully-fledged Python toolchain instead of just the
interpreter.")
    (home-page (package-home-page python))))

;; Python 3 toolchain for python-build-system. We cannot use python-toolchain
;; here, since we’d need to bootstrap python-pip somehow.
(define-public python-toolchain-for-build
  (package
    (inherit python-toolchain)
    (name "python-toolchain-for-build")
    (inputs
      `(("python" ,python-wrapper)
        ("python-setuptools" ,python-setuptools)))))

