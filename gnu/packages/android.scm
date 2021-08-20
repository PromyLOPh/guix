;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Stefan Handschuh <handschuh.stefan@googlemail.com>
;;; Copyright © 2015 Kai-Chung Yan <seamlikok@gmail.com>
;;; Copyright © 2016, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Sergey Trofimov <sarg@sarg.org.ru>
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

(define-module (gnu packages android)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system android-ndk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public android-make-stub
  (package
    (name "android-make-stub")
    (version "0.6.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/daym/android-make-stub")
            (commit (string-append "v" version))))
      (file-name (string-append "android-make-stub-"
                                version "-checkout"))
      (sha256
       (base32
        "0y1b2x96d37n6f1bp6dcx08bn08zac0cylmbfsx6mf2nahc02fhc"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; None exist.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (invoke "make" (string-append "prefix=" out) "install")
                 #t))))))
    (home-page "https://github.com/daym/android-make-stub")
    (synopsis "Stubs for the @command{make} system of the Android platform")
    (description "@code{android-make-stub} provides stubs for the
@command{make} system of the Android platform.  This allows us to
use their packages mostly unmodified in our Android NDK build system.")
    (license license:asl2.0)))

(define-public android-googletest
  (package (inherit googletest)
    (name "android-googletest")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google/googletest")
              (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bjlljmbf8glnd9qjabx73w6pd7ibv43yiyngqvmvgxsabzr8399"))))
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-host-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (symlink "libgtest.so"
                        (string-append lib "/libgtest_host.so"))
               (symlink "libgmock.so"
                        (string-append lib "/libgmock_host.so"))
               #t))))))))

;; The Makefiles that we add are largely based on the Debian
;; packages.  They are licensed under GPL-2 and have copyright:
;; 2012, Stefan Handschuh <handschuh.stefan@googlemail.com>
;; 2015, Kai-Chung Yan <seamlikok@gmail.com>
;; Big thanks to them for laying the groundwork.

;; The version tag is consistent between all repositories.
(define-public (android-platform-version) "7.1.2_r36")

(define-public (android-platform-system-core version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://android.googlesource.com/platform/system/core")
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-system-core-"
                              version "-checkout"))
    (sha256
     (base32
      "1krnc2b9zfkzpdgs1dcbji59nszlx2qr723pg89m52622czc06hg"))
    (patches
     (search-patches "libbase-use-own-logging.patch"
                     "libbase-fix-includes.patch"
                     "libutils-remove-damaging-includes.patch"
                     "libutils-add-includes.patch"
                     "adb-add-libraries.patch"
                     "adb-libssl_11-compatibility.patch"
                     "libziparchive-add-includes.patch"))))

(define (android-platform-system-extras version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://android.googlesource.com/platform/system/extras")
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-system-extras-"
                              version "-checkout"))
    (sha256
     (base32
      "18130c23ybqcpgjc5v6f8kdbv2xn39hyiaj17dzldjb9rlwzcyy9"))))

(define (android-platform-bionic version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://android.googlesource.com/platform/bionic")
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-bionic-"
                              version "-checkout"))
    (sha256
     (base32
      "15r4s20d7vw022f8vrc3jbghmqwdcqzprl7i2bfvdkz8z76wc1ps"))))

(define (android-platform-external version subdirectory checksum)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url
           (string-append "https://android.googlesource.com/platform/external/"
                          subdirectory))
          (commit (string-append "android-" version))))
    (file-name (string-append "android-platform-system-external-" subdirectory "-"
                              version "-checkout"))
    (sha256
     (base32
      checksum))))

(define-public android-liblog
  (package
    (name "android-liblog")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("LDLIBS=-lpthread")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "liblog") #t))
         (add-after 'install 'ldconfig
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (symlink "liblog.so.0" (string-append out "/lib/liblog.so"))
               #t)))
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively
                 "../include/log" (string-append out "/include/log"))
               ;; For android/log.h, the only header in the android directory.
               (copy-recursively
                 "../include/android" (string-append out "/include/android")))
             #t)))))
    (home-page "https://developer.android.com/")
    (synopsis "Logging library from the Android platform.")
    (description "@code{liblog} represents an interface to the volatile Android
Logging system for NDK (Native) applications and libraries and contain
interfaces for either writing or reading logs.  The log buffers are divided up
in Main, System, Radio and Events sub-logs.")
    (license license:asl2.0)))

(define android-libbase
  (package
    (name "android-libbase")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:tests? #f ; Test failure: logging.UNIMPLEMENTED
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "base") #t)))))
    (inputs `(("android-liblog" ,android-liblog)))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform base library")
    (description "@code{libbase} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public android-libcutils
  (package
    (name "android-libcutils")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; TODO.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libcutils") #t))
         (add-after 'enter-source 'create-Makefile
           (lambda _
             ;; No useful makefile is shipped, so we create one.
             (with-output-to-file "Makefile"
               (lambda _
                 (display
                  (string-append
                   "NAME = libcutils\n"
                   "SOURCES = load_file.o socket_local_client_unix.o"
                   " socket_loopback_client_unix.o socket_network_client_unix.o"
                   " socket_loopback_server_unix.o socket_local_server_unix.o"
                   " sockets_unix.o socket_inaddr_any_server_unix.o"
                   " sockets.o\n"
                   "CC = gcc\n"

                   "CFLAGS += -fPIC\n"
                   "CXXFLAGS += -fPIC\n"
                   "CPPFLAGS += -Iinclude -I../include\n"
                   "LDFLAGS += -shared -Wl,-soname,$(NAME).so.0\n"

                   "build: $(SOURCES)\n"
                   "	$(CXX) $^ -o $(NAME).so.0 $(CXXFLAGS) $(CPPFLAGS)"
                   " $(LDFLAGS)\n"))
                 #t))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (install-file "libcutils.so.0" lib)
               (with-directory-excursion lib
                 (symlink "libcutils.so.0" "libcutils.so"))
               (copy-recursively "../include/cutils"
                                 (string-append include "/cutils"))
               #t))))))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform c utils library")
    (description "@code{libcutils} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public android-libsparse
  (package
    (name "android-libsparse")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("CFLAGS=-Wno-error"
                      "CXXFLAGS=-fpermissive -Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libsparse") #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform sparse library")
    (description "@code{android-libsparse} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public android-libziparchive
  (package
    (name "android-libziparchive")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("CFLAGS=-Wno-error"
                      "CXXFLAGS=-fpermissive -Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libziparchive") #t))
         (add-before 'check 'setenv
           (lambda _
             (setenv "ziparchive_tests_host_PARAMS" "--test_data_dir=testdata")
             #t))
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "../include/ziparchive"
                                 (string-append out "/include/ziparchive"))
               #t))))))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("android-libbase" ,android-libbase)
       ("android-libutils" ,android-libutils)
       ("android-liblog" ,android-liblog)))
    (home-page "https://developer.android.com/")
    (synopsis "Android platform ZIP library")
    (description "@code{android-libziparchive} is a library in common use by the
various Android core host applications.")
    (license license:asl2.0)))

(define-public adb
  (package
    (name "adb")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:tests? #f ; Test failure: sysdeps_poll.fd_count
       #:make-flags
       (list "CFLAGS=-Wno-error"
             "CXXFLAGS=-fpermissive -Wno-error -std=gnu++14 -D_Nonnull= -D_Nullable= -I ."
             (string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out") "/lib "
                            "-Wl,-rpath=" (assoc-ref %build-inputs "openssl") "/lib -L ."))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "adb") #t))
         (add-after 'enter-source 'glibc-compat
           (lambda _
             ;; Include sysmacros.h for "major" and "minor" in Glibc 2.28.
             (substitute* "usb_linux.cpp"
               (("#include <sys/types.h>" all)
                (string-append all "\n#include <sys/sysmacros.h>\n")))
             #t))
         (add-after 'enter-source 'make-libs-available
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "Android.mk"
              (("libcrypto_static") "libcrypto"))
             #t))
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (install-file "diagnose_usb.h" (string-append (assoc-ref outputs "out") "/include"))
             #t)))))
    (inputs
     `(("android-libbase" ,android-libbase)
       ("android-libcutils" ,android-libcutils)
       ("android-liblog" ,android-liblog)
       ("openssl" ,openssl)))
    (home-page "https://developer.android.com/studio/command-line/adb.html")
    (synopsis "Android Debug Bridge")
    (description
     "@command{adb} is a versatile command line tool that lets you communicate
with an emulator instance or connected Android device.  It facilitates a variety
of device actions, such as installing and debugging apps, and it provides access
to a Unix shell that can run commands on the connected device or emulator.")
    (license license:asl2.0)))

(define-public mkbootimg
  (package
    (name "mkbootimg")
    (version (android-platform-version))
    (source (origin
              (inherit (android-platform-system-core version))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "mkbootimg") #t))
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (include (string-append out "/include")))
               (install-file "mkbootimg" bin)
               (install-file "bootimg.h" include)
               #t))))))
    (home-page "https://developer.android.com/studio/command-line/adb.html")
    (synopsis "Tool to create Android boot images")
    (description "This package provides a tool to create Android Boot
Images.")
    (license license:asl2.0)))

(define-public android-safe-iop
  (package
    (name "android-safe-iop")
    (version (android-platform-version))
    (source (android-platform-external version "safe-iop"
                                       "1nyyrs463advjhlq8xx1lm37m4g5afv7gy0csxrj7biwwl0v13qw"))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags '("CXXFLAGS=-fpermissive -Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-host
           (lambda _
             ;; TODO: Cross-compile.
             (substitute* "Android.mk"
              (("BUILD_STATIC_LIBRARY") "BUILD_HOST_STATIC_LIBRARY"))
             #t)))))
    (home-page "https://developer.android.com/")
    (synopsis "Safe integers in C")
    (description "@code{android-safe-iop} provides a set of functions for
performing and checking safe integer operations.  Ensure that integer
operations do not result in silent overflow.")
    (license license:bsd-2)))

(define-public android-bionic-uapi
  (package
    (name "android-bionic-uapi")
    (version (android-platform-version))
    (source (android-platform-bionic version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'enter-source
           (lambda _ (chdir "libc") #t))
        (replace 'check
          (const #t))
        (replace 'build
          (const #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (out-sys (string-append out "/include/sys")))
              (mkdir-p out-sys)
              (install-file "include/sys/system_properties.h" out-sys)
              (install-file "include/sys/_system_properties.h" out-sys)
              (copy-recursively "kernel/uapi" (string-append out "/include"))
              #t))))))
    (home-page "https://developer.android.com/")
    (synopsis "Android Linux API that is safe for user space")
    (description "@code{android-bionic-uapi} provides the part of the Linux API
that is safe to use for user space.  It also includes
@code{system_properties.h} and @code{_system_properties.h}.")
    (license license:asl2.0)))

(define-public android-libselinux
  (package
    (name "android-libselinux")
    (version (android-platform-version))
    (source
     (android-platform-external version "libselinux"
                                "13m2q32gzdcs5d0zj1nwasjy1j8vsxsgbjg7m5sa9lfcjaj7nkm7"))
    (build-system android-ndk-build-system)
    (arguments
     ;; See logd/Android.mk for the *_LOG_TAG values.
     `(#:make-flags (list (string-append "CFLAGS=-Wno-error "
                                         "-I core/include "
                                         "-I core/libpackagelistparser/include "
                                         "-DAUDITD_LOG_TAG=1003 "
                                         "-DLOGD_LOG_TAG=1004 -D_GNU_SOURCE")
                          "LDFLAGS=-L . -lpcre")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack-core 'patch-HOST
           (lambda _
             ;; gettid duplicates otherwise.
             (substitute* "src/procattr.c"
              (("#ifdef HOST") "#ifdef XXX"))
             #t)))))
    (inputs
     `(("openssl" ,openssl)))
    (native-inputs
     `(("android-bionic-uapi" ,android-bionic-uapi)
       ;; pcre is inlined by our package.
       ("pcre" ,pcre)))
    (home-page "https://developer.android.com/")
    (synopsis "Android version of the SELinux libraries and utilities")
    (description
     "The libselinux library provides an API for SELinux applications to get
and set process and file security contexts, and to obtain security policy
decisions.  It is required for any applications that use the SELinux API, and
used by all applications that are SELinux-aware.  This package also includes
the core SELinux management utilities.")
    (license license:public-domain)))

(define-public android-ext4-utils
  (package
    (name "android-ext4-utils")
    (version (android-platform-version))
    (source (android-platform-system-extras version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CPPFLAGS="
                            ;"-Wno-error "
                            "-I "
                            (assoc-ref %build-inputs "android-libselinux")
                            "/include "
                            "-I " (assoc-ref %build-inputs "android-libsparse")
                            "/include "
                            "-I " (assoc-ref %build-inputs "android-libcutils")
                            "/include "
                            "-I " (assoc-ref %build-inputs "android-liblog") "/include "
                            "-I ../core/include")
             "CFLAGS=-Wno-error"
             "install-libext4_utils_host.a"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-core
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "core")
             (with-directory-excursion "core"
               (invoke "tar" "axf" (assoc-ref inputs "android-core")
                             "--strip-components=1"))
             #t))
         (add-after 'unpack-core 'enter-source
           (lambda _ (chdir "ext4_utils") #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." (string-append out "/include")))
             #t)))))
    (inputs
     `(("android-libcutils" ,android-libcutils)
       ("android-liblog" ,android-liblog)
       ("android-libselinux" ,android-libselinux)
       ("android-libsparse" ,android-libsparse)
       ("zlib" ,zlib)))
    (native-inputs
     `(("android-core" ,(android-platform-system-core version))))
    (home-page "https://developer.android.com/")
    (synopsis "Android ext4 file system utilities")
    (description "@code{android-ext4-utils} is a library in common use by the
Android core.")
    (license license:asl2.0)))

(define-public android-f2fs-utils
  (package
    (name "android-f2fs-utils")
    (version (android-platform-version))
    (source (android-platform-system-extras version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "f2fs_utils") #t))
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (copy-recursively "." (string-append (assoc-ref outputs "out")
                                                  "/include"))
             #t))
         (add-after 'install 'install-shell-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (patch-shebang "mkf2fsuserimg.sh")
               (substitute* "mkf2fsuserimg.sh"
                (("make_f2fs") (string-append bin "/make_f2fs")))
               (install-file "mkf2fsuserimg.sh" bin)
               #t))))))
    (inputs
     `(("f2fs-tools" ,f2fs-tools-1.7)
       ("android-libselinux" ,android-libselinux)
       ("android-libsparse" ,android-libsparse)
       ("android-libcutils" ,android-libcutils)
       ("zlib" ,zlib)))
    (home-page "https://developer.android.com/")
    (synopsis "Android f2fs utils")
    (description "@code{android-f2fs-utils} is a library in common use by the
Android core.  It allows the user to create images for the @code{f2fs} Flash
file system.")
    (license license:asl2.0)))

(define-public android-libutils
  (package
    (name "android-libutils")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:tests? #f ; TODO
       #:make-flags '("CXXFLAGS=-std=gnu++11 -Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hide the default GCC from CPLUS_INCLUDE_PATH to prevent it from
             ;; shadowing the version of GCC provided in native-inputs.
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH")
                                              #\:))
                        ":"))
               #t)))
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "libutils") #t))
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (copy-recursively "../include/utils" (string-append (assoc-ref outputs "out") "/include/utils")))))))
    (inputs
     `(("android-safe-iop" ,android-safe-iop)
       ("android-libcutils" ,android-libcutils)))
    (native-inputs
     `(("android-bionic-uapi" ,android-bionic-uapi)
       ("android-liblog" ,android-liblog)
       ("gcc@5" ,gcc-5))) ; XXX: fails to build with GCC 7
    (home-page "https://developer.android.com/")
    (synopsis "Android utility library")
    (description "@code{android-libutils} provides utilities for Android NDK developers.")
    (license license:asl2.0)))

(define-public fastboot
  (package
    (name "fastboot")
    (version (android-platform-version))
    (source (android-platform-system-core version))
    (build-system android-ndk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source
           (lambda _
             (chdir "fastboot")
             #t))
         (add-after 'enter-source 'patch-source
           (lambda _
             (substitute* "Android.mk"
              (("libext4_utils_host") "libext4_utils_host libselinux libpcre"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (bin (string-append out "/bin")))
               (install-file "fastboot" bin)
               #t))))))
    (inputs
     `(("adb" ,adb)
       ("android-safe-iop" ,android-safe-iop)
       ("android-ext4-utils" ,android-ext4-utils)
       ("android-f2fs-utils" ,android-f2fs-utils)
       ("android-libbase" ,android-libbase)
       ("android-libcutils" ,android-libcutils)
       ("android-liblog" ,android-liblog)
       ("android-libutils" ,android-libutils)
       ("android-libsparse" ,android-libsparse)
       ("android-libziparchive" ,android-libziparchive)
       ("android-libselinux" ,android-libselinux)
       ("pcre" ,pcre)
       ("mkbootimg" ,mkbootimg)
       ("zlib" ,zlib)))
    (native-inputs
     `(("xz" ,xz)))
    (home-page "https://developer.android.com/studio/command-line/")
    (synopsis "Android image flasher")
    (description
     "This package provides @command{fastboot}, a tool to upload file system images to Android devices.")
    (license license:asl2.0)))

(define-public android-udev-rules
  (package
    (name "android-udev-rules")
    (version "20200613")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/M0Rf30/android-udev-rules")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cf5br8x6iwxc1cifv0i1klw7skgs8hghdx6qlqby68kyqg81bb2"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/51-android.rules")
                         (string-append %output "/lib/udev/rules.d"))
           #t))))
    (home-page "https://github.com/M0Rf30/android-udev-rules")
    (synopsis "udev rules for Android devices")
    (description "Provides a set of udev rules to allow using Android devices
with tools such as @command{adb} and @command{fastboot} without root
privileges.  This package is intended to be added as a rule to the
@code{udev-service-type} in your @code{operating-system} configuration.
Additionally, an @code{adbusers} group must be defined and your user added to
it.

@emph{Simply installing this package will not have any effect.}  It is meant
to be passed to the @code{udev} service.")
    (license license:gpl3+)))

(define-public git-repo
  (package
    (name "git-repo")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gerrit.googlesource.com/git-repo")
             (commit (string-append "v" version))))
       (file-name (string-append "git-repo-" version "-checkout"))
       (sha256
        (base32 "0khg1731927gvin73dcbw1657kbfq4k7agla5rpzqcnwkk5agzg3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-executable-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (git (assoc-ref inputs "git"))
                    (ssh (assoc-ref inputs "ssh")))
               (substitute* '("repo" "git_command.py")
                 (("^GIT = 'git'")
                  (string-append "GIT = '" git "/bin/git'")))
               (substitute* "git_config.py"
                 ((" command_base = \\['ssh',")
                  (string-append " command_base = ['" ssh "/bin/ssh',")))
               #t)))
         (add-before 'build 'do-not-self-update
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Setting the REPO_MAIN variable to an absolute file name is
             ;; enough to have _FindRepo return the store main.py file.  The
             ;; self update mechanism is activated with the call to _Init() in
             ;; main(), so we bypass it.

             ;; Ticket requesting upstream to provide a mean to disable the
             ;; self update mechanism:
             ;; https://bugs.chromium.org/p/gerrit/issues/detail?id=12407.
             (let* ((out (assoc-ref outputs "out"))
                    (repo-main (string-append out "/share/git-repo/main.py")))
               (substitute* "repo"
                 (("^REPO_MAIN = .*")
                  (format #f "REPO_MAIN = ~s~%" repo-main))
                 ((" _Init\\(args, gitc_init=\\(cmd ==.*" all)
                  (string-append "True #" all)))
               ;; Prevent repo from trying to git describe its version from
               ;; the (disabled) self updated copy.
               (substitute* "git_command.py"
                 (("ver = getattr\\(RepoSourceVersion.*")
                  (format #f "ver = ~s~%" ,version)))
               (substitute* "subcmds/version.py"
                 (("rp_ver = .*")
                  (format #f "rp_ver = ~s~%" ,version)))
               ;; Prevent repo from adding its (disabled) self update copy to
               ;; the list of projects to fetch when using 'repo sync'.
               (substitute* "subcmds/sync.py"
                 (("to_fetch\\.extend\\(all_projects\\).*" all)
                  (string-append "#" all))
                 (("self\\._Fetch\\(to_fetch")
                  "self._Fetch(all_projects")
                 (("_PostRepoFetch\\(rp, opt\\.repo_verify).*" all)
                  (string-append "#" all))))))
         (delete 'build) ; nothing to build
         (add-before 'check 'configure-git
           (lambda _
             (setenv "HOME" (getcwd))
             (invoke "git" "config" "--global" "user.email" "you@example.com")
             (invoke "git" "config" "--global" "user.name" "Your Name")))
         (replace 'check
           (lambda _
             (invoke "./run_tests")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir (string-append out "/bin"))
                    (repo-dir (string-append out "/share/" ,name)))
               (mkdir-p bin-dir)
               (mkdir-p repo-dir)
               (copy-recursively "." repo-dir)
               (delete-file-recursively (string-append repo-dir "/tests"))
               (symlink (string-append repo-dir "/repo")
                        (string-append bin-dir "/repo"))
               #t))))))
    (inputs
     ;; TODO: Add git-remote-persistent-https once it is available in guix
     `(("git" ,git)
       ("ssh" ,openssh)))
    (native-inputs
     `(("pytest" ,python-pytest)))
    (home-page "https://code.google.com/p/git-repo/")
    (synopsis "Helps to manage many Git repositories.")
    (description "Repo is a tool built on top of Git.  Repo helps manage many
Git repositories, does the uploads to revision control systems, and automates
parts of the development workflow.  Repo is not meant to replace Git, only to
make it easier to work with Git.  The repo command is an executable Python
script that you can put anywhere in your path.")
    (license license:asl2.0)))

(define-public abootimg
  (package
    (name "abootimg")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://http.debian.net/debian/pool/main/a/abootimg/"
                           "abootimg_" version ".orig.tar.gz"))
       (sha256
        (base32 "0sfc2k011l1ymv97821w89391gnqdh8pp0haz4sdcm5hx0axv2ba"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
        (replace 'configure
          (lambda _
            (setenv "CC" "gcc")
            #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (install-file "abootimg" bin)
              #t))))))
    (inputs
     `(("libblkid" ,util-linux "lib")))
    (home-page "https://ac100.grandou.net/abootimg")
    (synopsis "Tool for manipulating Android Boot Images")
    (description "This package provides a tool for manipulating old Android
Boot Images.  @code{abootimg} can work directly on block devices, or, the
safest way, on a file image.")
    (license license:gpl2+)))

(define-public python-androguard
  (package
    (name "python-androguard")
    (version "3.2.1")
    (source
      (origin
        ;; The pypi release doesn't have the tests, but the tests use
        ;; packaged binaries, so we skip them.
        (method url-fetch)
        (uri (pypi-uri "androguard" version))
        (sha256
         (base32
          "0ndsw00pkyda4i2s3wi5ap8gbk6a9d23xhhxpdbk02padv8sxkfv"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Adapted from .travis.yml
           (lambda _
             (invoke "nosetests" "--with-coverage" "--with-timer"
                     "--timer-top-n" "50"))))))
    (native-inputs
     `(("python-codecov" ,python-codecov)
       ("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-nose-timer" ,python-nose-timer)))
    (propagated-inputs
     `(("python-asn1crypto" ,python-asn1crypto)
       ("python-colorama" ,python-colorama)
       ("python-future" ,python-future)
       ("python-ipython" ,python-ipython)
       ("python-lxml" ,python-lxml)
       ("python-matplotlib" ,python-matplotlib)
       ("python-networkx" ,python-networkx)
       ("python-pygments" ,python-pygments)
       ("python-pyperclip" ,python-pyperclip)))
    (home-page "https://github.com/androguard/androguard")
    (synopsis "Python tool to play with Android files")
    (description
     "Androguard is a full Python tool to manipulate Android files.  It is
useful for reverse engineering, analysis of Android applications and more.")
    (license license:asl2.0)))

(define-public fdroidserver
  (package
    (name "fdroidserver")
    (version "1.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fdroidserver" version))
        (sha256
         (base32
          "0m07f791z45w7r2dzx4yb6s54b3c3wykm3w9hn25p2jcyax082a2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versioning
           (lambda _
             (substitute* "setup.py"
               (("0.2.1") ,(package-version python-pyasn1-modules))
               ;; The dependency on docker has been removed upstream by
               ;; a fairly large patch:
               ;; https://gitlab.com/fdroid/fdroidserver/-/commit/89614851250c79a05db84070feca6dea033af334
               ;; that is not in a release yet. It appears we can compile with
               ;; a newer version.
               (("docker-py >= 1.9, < 2.0") "docker >= 1.9"))
             #t)))))
    (propagated-inputs
     `(("python-androguard" ,python-androguard)
       ("python-apache-libcloud" ,python-apache-libcloud)
       ("python-clint" ,python-clint)
       ("python-defusedxml" ,python-defusedxml)
       ("python-docker" ,python-docker)
       ("python-gitpython" ,python-gitpython)
       ("python-mwclient" ,python-mwclient)
       ("python-paramiko" ,python-paramiko)
       ("python-pillow" ,python-pillow)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pyasn1-modules" ,python-pyasn1-modules)
       ("python-pyyaml" ,python-pyyaml)
       ("python-qrcode" ,python-qrcode)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-requests" ,python-requests)
       ("python-vagrant" ,python-vagrant)))
    (native-inputs
     `(("python-babel" ,python-babel)
       ("python-bcrypt" ,python-bcrypt)
       ("python-docker-pycreds" ,python-docker-pycreds)
       ("python-pynacl" ,python-pynacl)
       ("python-websocket-client" ,python-websocket-client)))
    (home-page "https://f-droid.org")
    (synopsis "F-Droid server tools")
    (description
     "The F-Droid server tools provide various scripts and tools that are used
to maintain F-Droid, the repository of free Android applications.  You can use
these same tools to create your own additional or alternative repository for
publishing, or to assist in creating, testing and submitting metadata to the
main repository.")
    (license license:agpl3+)))

(define-public fdroidcl
  (package
    (name "fdroidcl")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mvdan/fdroidcl")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1rxcdyy2j34z0ql9d62w7ivsch9xihjnpb1z9kgy9q46vl8zhhy0"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "mvdan.cc/fdroidcl"
       #:tests? #f  ; TODO: Inputs missing.
       #:install-source? #f))
    (inputs
     `(("go-github-com-kr-pretty" ,go-github-com-kr-pretty)))
    ;(native-inputs
    ; `(("go-github-com-rogpeppe-go-internal-testscript"
    ;    ,go-github-com-rogpeppe-go-internal-testscript)))
    (synopsis "F-Droid desktop client")
    (description
     "While the Android client integrates with the system with regular update
checks and notifications, this is a simple command line client that talks to
connected devices via ADB.")
    (home-page "https://github.com/mvdan/fdroidcl")
    (license license:bsd-3)))

(define-public enjarify
  (package
    (name "enjarify")
    (version "1.0.3")
    (home-page "https://github.com/Storyyeller/enjarify")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url home-page)
            (commit version)))
      (file-name (git-file-name name version))
      (patches
       (search-patches "enjarify-setup-py.patch"))
      (sha256
       (base32
        "1nam7h1g4f1h6jla4qcjjagnyvd24dv6d5445w04q8hx07nxdapk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enjarify-wrapper-inherit-pythonpath
           ;; enjarify sets PYTHONPATH from a shell script, overwriting
           ;; PYTHONPATH set from guix. Comment out this line.
           (lambda _
             (substitute* "enjarify.sh"
               (("export PYTHONPATH") "# export PYTHONPATH"))
             #t))
         (add-before 'check 'fixup-expected-test-results
           ;; Upstream adjusted this test in commit:
           ;; 3ae884a6485af82d300515813f537685b08dd800
           (lambda _
             (substitute* "tests/test2/expected.txt"
               (("^20") "0"))
             #t))
         (add-before 'check 'drop-java-xss-argument
           ;; Upstream removed this argument in order to support 32-bit
           ;; architectures.  commit: 4be0111d879aa95fdc0d9f24fe529f8c664d4093
           (lambda _
             (substitute* "enjarify/runtests.py"
               (("java -Xss515m") "java "))
             #t))
         (add-after 'install 'install-enjarify-wrapper
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/bin/"))
                 (copy-file "enjarify.sh" (string-append out "/bin/enjarify"))
                 #t))))))
    (native-inputs `(("openjdk" ,openjdk12)))
    (synopsis "Translate Dalvik bytecode to equivalent Java bytecode")
    (description "Android applications are Java programs that run on a
customized virtual machine, which is part of the Android operating system, the
Dalvik VM.  Their bytecode differs from the bytecode of normal Java
applications.  Enjarify can translate the Dalvik bytecode back to equivalent
Java bytecode, which simplifies the analysis of Android applications.")
    (license license:asl2.0)))
