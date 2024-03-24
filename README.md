Ekmelily
========

Ekmelily is an extension for [LilyPond](http://lilypond.org/) that supports variable
accidentals and key signatures for the notation of microtonal music
in several equal-temperament tunings - 12, 19, 24, 31, 36, 48, 53, 72-EDO -
and in 5-limit JI.
For this purpose, it introduces predefined and user-defined notation styles.
Each style describes a set of symbols for the alterations up to the five-quarters-tone, at most.
Furthermore, Ekmelily defines own note names based on the names for
semi- and quarter-tones given in LilyPond.

Ekmelily requires LilyPond version 2.19.22 or higher.

Installation
------------

The folder *ly* contains the include files.

*   Copy the include file(s) for the desired tuning(s) as well as the
    main include file `ekmel-main.ily` into an appropriate folder,
    e.g. `LILYPOND/usr/share/lilypond/current/ly`, with `LILYPOND`
    meaning the installation folder of LilyPond.

*   Optionally install a font, e.g. [Ekmelos](https://github.com/tr-igem/ekmelos).
