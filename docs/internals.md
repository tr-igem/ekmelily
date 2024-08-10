Internals
=========

Description of the internal tables of [Ekmelily](https://github.com/tr-igem/ekmelily)
which implement the tuning (tone system), the language (note names),
and the notation (accidentals).
Each include file for a specific tuning defines three variables with
these tables as described below. They are required by the main include
file `ekmel-main.ily`.
In the same way, Ekmelily can be extended to support another tuning,
language, or notation.

See "README.md" at [Tables](https://u.pcloud.link/publink/show?code=kZ3UnHZ7OiBK9gH8mYYIi1spPdIM4yK45gy#folder=43567022)
for a description of the alteration codes used in these tables.



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
    the tonic, usually in the range 0 .. 6.

*   ALTER-CODE (integer): The alteration code.

*   ALTER (rational): The alteration as a 200 cent fraction in the
    range +5/4 .. -5/4.



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

Each source language table has one of the following formats:


### Old format

An alist of notes and alteration codes mapped onto note names.

    (LANGUAGE . (
      (NOTE-NAME NOTE . ALTER-CODE)
      ...
    ))


### New format

A list that defines scale names, alteration names, and extra (alias)
names separately. The actual note names are generated when LANGUAGE
is selected by combinig each scale name with each alteration name.
A generated name can be extended with one or more alias names, and
it can be excluded.

    (LANGUAGE . (
      SCALE
      EXTRA
      (ALTER-CODE ALTER-NAME ...)
      ...
    ))

SCALE is either a vector with the scale names (symbols) or a number
for predefined scale names:

    0       c   d   e   f   g   a   b
    1       c   d   e   f   g   a   h
    2       do  re  mi  fa  sol la  si
    3       do  ré  mi  fa  sol la  si

EXTRA is a list whose elements extend or exclude the generated names.
A pair defines an alias for NAME. A single NAME excludes it.
NAME is either a symbol or a string which applies to every name with
prefix NAME (then ALIAS-NAME must also be a string).

    (NAME . ALIAS-NAME)
    NAME

The remainder (cddr) of the source language table is an alist of
alteration names mapped onto alteration codes.
`#f` means no alteration name. The element `(0 #f)` for note names
without alteration can be omitted, unless there are composite names,
e.g. for "english": `(0 #f -natural)`.

The actual language table (pitchnames table) is created and installed
with `(ekm:set-language lang)`. It ignores note names for alteration
codes that are not defined in the tuning table.

*   LANGUAGE (symbol): The name of a supported language.

*   ALIAS-LANGUAGE (symbol): The alias name of a supported language.

*   NOTE-NAME (symbol): The note name (pitch name).

*   NOTE (integer): The scale note index; usually in the range 0 .. 6.

*   ALTER-CODE (integer): The alteration code.

*   ALTER-NAME (symbol): The alteration name.



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

The actual notation table is created and installed with
`(ekm:set-notation name)`.
It is an alist of accidental definitions as markup mapped onto
alterations, but only those whose alteration codes are defined in the
tuning table.

Each alteration in the language table must also appear in the
notation table.
A missing alteration is supplied with an empty string as accidental
and produce a warning.
A missing enharmonically equivalent alteration is supplied with the
accidental of the corresponding alteration if present, else with an
empty string and produce a warning.

    ((ALTER-OR-SPECIAL MARKUP ...) ...)

*   NOTATION (symbol): The name of a supported notation.

*   ALTER-CODE (integer): The alteration code.

*   ELEMENT: A codepoint (integer), a character, a string, or
    an arbitrary markup which defines the accidental.
    Two or more ELEMENTs are juxtaposed with a padding of 0.12
    staff units, but no extra space is inserted between the characters
    of a string.

*   ALTER-OR-SPECIAL (rational or symbol): Either the alteration
    as a 200 cent fraction in the range +5/4 .. -5/4,
    or the name of a special symbol:

    *   'leftparen: Left parenthesis for cautionary accidentals.
    *   'rightparen: Right parenthesis for cautionary accidentals.
    *   'default: Default accidental for undefined alterations.

*   MARKUP: A markup from the corresponding ELEMENT.
    This is a string if ELEMENT is a codepoint, character, or string.
