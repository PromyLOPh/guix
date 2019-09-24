;; GNU Guix news, for use by 'guix pull'.
;;
;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;
;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty provided the copyright notice and
;; this notice are preserved.

(channel-news
 (version 0)

 (entry (commit "5f3f70391809f8791c55c05bd1646bc58508fa2c")
        (title (en "GNU C Library upgraded"))
        (body
         (en "The GNU C Library (glibc) has been upgraded to version 2.29.  To
run previously-installed programs linked against glibc 2.28, you need to
install locale data for version 2.28 in addition to locale data for 2.29:

@example
guix install glibc-locales glibc-locales-2.28
@end example

On Guix System, you can adjust the @code{locale-libcs} field of your
@code{operating-system} form.  Run @code{info \"(guix) Locales\"}, for more
info.")
         (de "Die GNU-C-Bibliothek (glibc) wurde auf Version 2.29
aktualisiert. Um zuvor installierte Programme, die an glibc 2.28 gebunden
worden sind, weiter benutzen zu können, müssen Sie Locale-Daten für Version
2.28 zusätzlich zu den Locale-Daten für 2.29 installieren:

@example
guix install glibc-locales glibc-locales-2.28
@end example

Auf Guix System genügt es, das @code{locale-libcs}-Feld Ihrer
@code{operating-system}-Form anzupassen. Führen Sie @code{info \"(guix.de)
Locales\"} aus, um weitere Informationen dazu zu erhalten.")))

 (entry (commit "cdd3bcf03883d129581a79e6d6611b2afd3b277b")
        (title (en "New reduced binary seed bootstrap"))
        (body
         (en "The package graph on x86_64 and i686 is now rooted in a
@dfn{reduced set of binary seeds}.  The initial set of binaries from which
packages are built now weighs in at approximately 130 MiB, half of what it
used to be.  Run @code{info \"(guix) Bootstrapping\"} to learn more, or watch
the talk at @uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (de "Der Paketgraph auf x86_64 und i686 hat jetzt eine @dfn{kleinere
Menge an binären Seeds}. Das heißt, die ursprüngliche Menge an Binärdateien,
aus denen heraus Pakete erstellt werden, machen nun ungefähr 130 MiB aus, halb
so viel wie früher. Führen Sie @code{info \"(guix.de) Bootstrapping\"} aus, um
mehr zu erfahren, oder schauen Sie sich den Vortrag auf
@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/} an.")))

 (entry (commit "dcc90d15581189dbc30e201db2b807273d6484f0")
        (title (en "New channel news mechanism")
               (de "Neuer Mechanismus, um Neuigkeiten über Kanäle anzuzeigen.")
               (fr "Nouveau mécanisme d'information sur les canaux"))
        (body
         (en "You are reading this message through the new channel news
mechanism, congratulations!  This mechanism allows channel authors to provide
@dfn{news entries} that their users can view with @command{guix pull --news}.
Run @command{info \"(guix) Invoking guix pull\"} for more info.")
         (de "Sie lesen diese Meldung mit Hilfe des neuen Mechanismus, um
Neuigkeiten über Kanäle anzuzeigen — Glückwunsch! Mit diesem
Mechanismus können Kanalautoren Ihren Nutzern @dfn{Einträge zu
Neuigkeiten} mitteilen, die diese sich mit @command{guix pull --news}
anzeigen lassen können. Führen Sie @command{info \"(guix.de) Aufruf
von guix pull\"} aus, um weitere Informationen zu erhalten.")
         (fr "Ce message t'arrive à travers le nouveau mécanisme d'information
des canaux, bravo !  Ce mécanisme permet aux auteur·rice·s de canaux de
fournir des informations qu'on peut visualiser avec @command{guix pull
--news}.  Tape @command{info \"(guix.fr) Invoquer guix pull\"} pour plus de
détails."))))
