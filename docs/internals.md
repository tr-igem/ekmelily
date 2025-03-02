Internals
=========

Description of the internal tables of [Ekmelily](https://github.com/tr-igem/ekmelily)
which implement the tuning (tone system), the language (note names),
and the notation and padding of accidentals.
Each include file for a specific tuning defines variables with these
tables as described below. They are used by the main include file
`ekmel-main.ily`. For alteration codes used in these tables, see a
description further below.



Tuning
------

The tuning table establishes a specific tuning (tone system).

The variable `ekmTuning` defines an alist of alterations mapped onto
alteration codes. Only upward alterations > 0 (i.e. with even alteration
codes) are necessary.

    ekmTuning = #'(
      (-1 STEP ...)
      (ALTER-CODE . ALTER)
      ...
    )

*   The special key -1 defines the global scale.
    It defaults to the 12-edo scale (0 1 2 5/2 7/2 9/2 11/2).

*   STEP (rational): The scale note step as a 200 cent fraction above
    the tonic, usually in the range 0 to 6.

*   ALTER-CODE (integer): The alteration code.

*   ALTER (rational): The alteration as a 200 cent fraction, usually
    in the range +5/4 to -5/4.



Language
--------

The language table defines the note names (pitch names).

The variable `ekmLanguages` defines an alist with one or more source
language tables and alias definitions.

    ekmLanguages = #'(
      (LANGUAGE . (
        ...
      ))

      (ALIAS-LANGUAGE . LANGUAGE)

      ...
    )

*   LANGUAGE (symbol): The name of a supported language.

*   ALIAS-LANGUAGE (symbol): The alias name of a supported language.

The actual language table (pitchnames table) is created and installed
with `(ekm:set-language lang)`. It ignores note names for alteration
codes that are not defined in the tuning table.

Each source language table has one of the following formats:


### Old format

An alist of notes and alteration codes mapped onto note names.

    (LANGUAGE . (
      (NOTE-NAME NOTE . ALTER-CODE)
      ...
    ))

*   NOTE-NAME (symbol): The note name (pitch name).

*   NOTE (integer): The scale note index, usually in the range 0 to 6.

*   ALTER-CODE (integer): The alteration code.


### New format

A list that defines scale names, alteration names, and extra (alias)
names separately. The actual note names are generated by combinig
each scale name with each alteration name. A generated name can be
extended with one or more alias names, and/or it can be excluded.

    (LANGUAGE . (
      SCALE
      (EXTRA ...)
      ALTER ...
    ))

*   SCALE (vector or integer): A vector with the scale names (symbols)
    or the number of a predefined vector:

        0       c   d   e   f   g   a   b
        1       c   d   e   f   g   a   h
        2       do  re  mi  fa  sol la  si
        3       do  ré  mi  fa  sol la  si

*   EXTRA: An optional definition. A pair defines an alias for a
    generated name. A single name excludes it.

        (NAME . ALIAS-NAME)
        (NAME-PREFIX . ALIAS-NAME-PREFIX)
        NAME
        NAME-PREFIX

*   NAME (symbol): Applies to this name only.

*   NAME-PREFIX (string): Applies to every name with this prefix.

*   ALTER (list): One or more alteration names mapped onto an
    alteration code.

        (ALTER-CODE ALTER-NAME ...)

*   ALTER-CODE (integer): The alteration code.

*   ALTER-NAME (symbol or #f): The alteration name or `#f`.

ALTER = `(0 #f)` for note names without alteration can be omitted,
unless there are composite names, e.g. for "english": `(0 #f -natural)`



Notation
--------

The notation table defines the accidental symbols.

The variable `ekmNotations` defines an alist with one or more source
notation tables. Each of them is an alist of accidental definitions
mapped onto alteration codes.

    ekmNotations = #'(
      (NOTATION . (
        (ALTER-CODE ELEMENT ...)
        ...
      ))
      ...
    )

*   NOTATION (symbol): The name of a supported notation.

*   ALTER-CODE (integer): The alteration code.

*   ELEMENT: A codepoint (integer), character, string, or any markup
    defining the accidental. Two or more ELEMENTs are juxtaposed
    with a padding of 0.12 staff units, but no extra space is inserted
    between the characters of a string.

The actual notation table is created and installed with
`(ekm:set-notation name)`. It is an alist of accidentals defined
as markup and mapped onto alterations. It ignores accidentals for
alteration codes that are not defined in the tuning table.

    (
      (ALTER MARKUP ...)
      ...
      (SPECIAL MARKUP ...)
      ...
    )

*   ALTER (rational): The alteration as a 200 cent fraction.

*   SPECIAL (symbol): The name of a special symbol:

    *   `leftparen`: Left parenthesis for cautionary accidentals.
    *   `rightparen`: Right parenthesis for cautionary accidentals.
    *   `default`: Default accidental for undefined alterations.

*   MARKUP: A markup from the corresponding ELEMENT. This is a string
    if ELEMENT is a codepoint (integer), character, or string.

Each alteration in the language table must also appear in the
notation table. A missing alteration is supplied with an empty markup
as accidental and produce a warning. A missing enharmonically equivalent
alteration is supplied with the accidental of the corresponding
alteration if present, else with an empty markup and produce a warning.



Padding
-------

The padding table defines the extra padding of accidentals. It is used
for normal accidentals without parentheses and for key signatures.

The variable `ekmPadding` defines an alist of paddings mapped onto
accidentals. It is optional and extends the predefined list.

    ekmPadding = #'(
      (ELEMENT . PADDING)
      ...
    )

The predefined list currently includes flat and double-flat:

    (#xE260 . 0.375)
    (#xE264 . 0.65)

*   ELEMENT: A codepoint (integer), character, string, or any markup
    defining the accidental.

*   PADDING (number): Factor of the right edge of an imaginary
    (transparent) box relative to the right edge of the accidental.
    The box is used to change the right horizontal skyline of the
    accidental.



Text align
----------

The text align table defines the vertical alignment of accidentals
for use outside of a staff, e.g. in a NoteNames context, trill spanner,
figured bass, or function theory text.
Is is used by the markup command `\ekmelic-char-text`.

The variable `ekmTextAlign` defines an alist of alignments mapped onto
accidentals. It is optional and extends the predefined list.

    ekmTextAlign = #'(
      (ELEMENT . ALIGN)
      ...
    )

The predefined list currently includes natural, sharp, and double-sharp:

    (#xE261 . 0)
    (#xE262 . 0)
    (#xE263 . 0)

*   ELEMENT: A codepoint (integer), character, string, or any markup
    defining the accidental.

*   ALIGN (number): One of the following:

    *   `DOWN` (-1): Bottom aligned. This is the default for accidentals
        without an entry in the text align table (e.g. flat).

    *   `CENTER` (0): Center aligned to the midline of another glyph
        defined with the variable `ekmTextCenter` or wiggleTrill (U+EAA4).

    *   &gt; 32: Codepoint of a glyph to draw instead of the accidental.

    *   Else: Vertically translated by ALIGN.



Alteration code
---------------

This is a proposal for a universal encoding of alterations (accidentals)
in an arbitrary tuning and independent of a specific notation.
An alteration code represents a distinct tone step, i.e. either an
Equal-Temperament tone fraction, or a Just Intonation primary comma,
relative to a base tone in a given scale.

The encoding is based on the [Sagittal](https://sagittal.org/)
notation system and the corresponding code points in [SMuFL](http://www.smufl.org/).


### Basic concepts

*   All codes are positive integers.

*   0 represents natural.

*   An even code 2n represents an upward alterations.
    An odd code 2n+1 represents the corresponding downward alteration.

*   The codes are grouped into blocks according to the precision classes
    of Sagittal S,A,T,P (see below) and to the corresponding SMuFL ranges.
    The order of codes within a block equals the order of SMuFL code points.

*   The codes are not always contiguous, i.e. there are unused and
    reserved ranges, and not necessarily sorted by alteration.

*   Additional bits are reserved for alternative codes, e.g. for
    enharmonically equivalent accidentals, and possibly for higher
    precisions according to the accidental diacritics in Sagittal.


### Ranges

    Range       Size  Description

    0000-01FF   512   Basic codes (bits 0 - 8)

    0000-000F   16    No alteration
    0000        1     Natural
    0001-000F   15    --

    0010-005F   80    S - Spartan
    0010-001F   16    Single-shaft
    0020-0045   38    Multi-shaft (incl. 2 unused), up to whole tone
    0046-0051   12    up to 5/4 tone
    0052-005F   14    --

    0060-009F   64    A - Athenian (medium precision)
    0060-0087   40    up to whole tone
    0088-0091   10    up to 5/4 tone
    0092-009F   14    --

    00A0-00BF   32    T - Trojan (12-EDO relative)
    00A0-00B7   24    up to whole tone
    00B8-00BD   6     up to 5/4 tone
    00BE-00BF   2     --

    00C0-013F   128   P - Promethean (high precision)
    00C0-00DD   30    Single-shaft
    00DE-00DF   2     --
    00E0-011F   64    Multi-shaft (incl. 2 unused), up to whole tone
    0120-0133   20    up to 5/4 tone
    0134-013F   12    --

    0200-03FF   512   Alternative codes = Basic codes + 0x0200 (bit 9)
