%%
%% Examples of user-defined notation styles
%%

%%----------------------------------------------------------------------
%% Some notation styles for 72-EDO
%% with textual alternatives using ASCII punctuation marks and letters.
%%----------------------------------------------------------------------

%% HEWM (Helmholtz / Ellis / Wolf / Monzo) notation

\include "ekmel.ily"

\ekmelicUserStyle hewm #'(
    (1 #\x)
    (-1 #\b #\b)
    (1/2 #\#)
    (-1/2 #\b)
    (1/4 #\^)
    (-1/4 #\v)
    (1/6 #\>)
    (-1/6 #\<)
    (1/12 #\+)
    (-1/12 #\-))

%-----------------------------------------------------------------------

%% Sims / Maneri ASCII notation

\include "ekmel.ily"

\ekmelicUserStyle simsascii #'(
    (1 #\x)
    (-1 #\b #\b)
    (1/2 #\#)
    (-1/2 #\b)
    (1/4 #\[)
    (-1/4 #\])
    (1/6 #\>)
    (-1/6 #\<)
    (1/12 #\^)
    (-1/12 #\v))

%-----------------------------------------------------------------------

%% Sagittal Pure-long ASCII representation

\include "ekmel.ily"

\ekmelicUserStyle saglong #'(
    (0 #\| #\/ #\/ #\|)
    (1/12 #\/ #\|)
    (-1/12 #\\ #\!)
    (1/6 #\| #\))
    (-1/6 #\! #\))
    (1/4 #\/ #\| #\\)
    (-1/4 #\\ #\! #\/)
    (1/3 #\| #\| #\))
    (-1/3 #\! #\! #\))
    (5/12 #\| #\| #\\)
    (-5/12 #\! #\! #\/)
    (1/2 #\/ #\| #\| #\\)
    (-1/2 #\\ #\! #\! #\/)
    (7/12 #\/ #\| #\| #\|)
    (-7/12 #\\ #\! #\! #\!)
    (2/3 #\| #\| #\| #\))
    (-2/3 #\! #\! #\! #\))
    (3/4 #\/ #\| #\| #\| #\\)
    (-3/4 #\\ #\! #\! #\! #\/)
    (5/6 #\X #\))
    (-5/6 #\Y #\))
    (11/12 #\X #\\)
    (-11/12 #\Y #\/)
    (1 #\/ #\X #\\)
    (-1 #\\ #\Y #\/)
    (13/12 #\/ #\| #\/ #\X #\\)
    (-13/12 #\\ #\! #\\ #\Y #\/)
    (7/6 #\| #\) #\/ #\X #\\)
    (-7/6 #\! #\) #\\ #\Y #\/)
    (5/4 #\/ #\| #\\ #\/ #\X #\\)
    (-5/4 #\\ #\! #\/ #\\ #\Y #\/))

%-----------------------------------------------------------------------

%% Sagittal Mixed-short ASCII representation

\include "ekmel.ily"

\ekmelicUserStyle sagshort #'(
    (0 #\e)
    (1/12 #\/)
    (-1/12 #\\)
    (1/6 #\f)
    (-1/6 #\t)
    (1/4 #\^)
    (-1/4 #\v)
    (1/3 #\# #\t)
    (-1/3 #\b #\f)
    (5/12 #\# #\\)
    (-5/12 #\b #\/)
    (1/2 #\#)
    (-1/2 #\b)
    (7/12 #\# #\/)
    (-7/12 #\b #\\)
    (2/3 #\# #\f)
    (-2/3 #\b #\t)
    (3/4 #\# #\^)
    (-3/4 #\b #\v)
    (5/6 #\x #\t)
    (-5/6 #\b #\b #\f)
    (11/12 #\x #\\)
    (-11/12 #\b #\b #\/)
    (1 #\x)
    (-1 #\b #\b)
    (13/12 #\x #\/)
    (-13/12 #\b #\b #\\)
    (7/6 #\x #\f)
    (-7/6 #\b #\b #\t)
    (5/4 #\x #\^)
    (-5/4 #\b #\b #\v))

%-----------------------------------------------------------------------

%% Mixed Sagittal ASCII notation

\include "ekmel.ily"

\ekmelicUserStyle msagascii #'(
    (0 #\e)
    (1 #\x)
    (-1 #\b #\b)
    (1/2 #\#)
    (-1/2 #\b)
    (1/4 #\^)
    (-1/4 #\v)
    (1/6 #\f)
    (-1/6 #\t)
    (1/12 #\/)
    (-1/12 #\\))

%-----------------------------------------------------------------------

%% Another alternative ASCII notation
%% This is a variant of the HEWM notation without `>`, `<` and `x`.

\include "ekmel.ily"

\ekmelicUserStyle altascii #'(
    (1 #\# #\#)
    (-1 #\b #\b)
    (1/2 #\#)
    (-1/2 #\b)
    (1/4 #\^)
    (-1/4 #\v)
    (1/6 #\+ #\+)
    (-1/6 #\- #\-)
    (1/12 #\+)
    (-1/12 #\-))

%-----------------------------------------------------------------------




%%----------------------------------------------------------------------
%% Some of the following notation styles have been introduced as
%% predefined styles in Ekmelily 1.2, but removed in version 1.7
%% since they are just freely invented combinations of individual
%% accidentals.
%%----------------------------------------------------------------------

%% Bussotti notation (24-EDO)
%% Fractional sharp symbols with single slash after Sylvano Bussotti.

\include "ekmel-24.ily"

\ekmelicStyle arrow

\ekmelicUserStyle bus #'(
    (3/4 #xE474)
    (1/4 #xE472)
    (-5/4 #xE472 #xE260 #xE264)
    (-3/4 #xE472 #xE264)
    (-1/4 #xE472 #xE260))

%-----------------------------------------------------------------------

%% Wiggle / Stein / Couper notation (24-EDO)
%% Quarter-tone sharp symbol with wiggly tail,
%% completed with the symbols of the Stein / Couper notation.

\include "ekmel-24.ily"

\ekmelicStyle stc

\ekmelicUserStyle wstc #'(
    (1/4 #xE475))

%-----------------------------------------------------------------------

%% Stein / Penderecki notation (24-EDO)
%% Filled quarter-tone flat symbol after Krzysztof Eugeniusz Penderecki,
%% completed with the symbols of the Stein / Zimmermann notation.

\include "ekmel-24.ily"

\ekmelicStyle stz

\ekmelicUserStyle stp #'(
    (-1/4 #xE478))

%-----------------------------------------------------------------------

%% Stein / Grisey notation (24-EDO)
%% Three-quarter-tones flat symbol with double stem after Gérard Grisey,
%% completed with the symbols after Richard Stein.

\include "ekmel-24.ily"

\ekmelicStyle stz

\ekmelicUserStyle stg #'(
    (-3/4 #xE486))

%-----------------------------------------------------------------------

%% Xenakis notation (18-EDO)
%% One-third-tone and two-third-tones sharp symbols after Iannis Xenakis.

\include "ekmel-36.ily"

\ekmelicUserStyle xen #'(
    (2/3 #xE471)
    (-2/3 #xE470 #xE264)
    (1/3 #xE470)
    (-1/3 #xE471 #xE264))

%-----------------------------------------------------------------------

%% Stockhausen notation (24-EDO)
%% Fractional sharp symbols (with one and three slashes) and
%% quarter-tone flat symbol (with slash) after Karlheinz Stockhausen.

\include "ekmel-24.ily"

\ekmelicStyle stz

\ekmelicUserStyle stockhausen #'(
    (3/4 #xED5A)
    (-3/4 #xED59 #xE260)
    (1/4 #xED58)
    (-1/4 #xED59))

%-----------------------------------------------------------------------

%% Diatonic notation (24-EDO)
%% Standard sharp/flat symbols, single thru quintuple, like the
%% predefined Diatonic notation (dia) but for quarter-tones.
%% The quadruple symbols (U+F61C, U+F61D) are private supplements
%% in the Ekmelos font.
%% Note that here, the order of alterations is significant since the
%% standard accidentals in the default notation (stc) are rearranged.

\include "ekmel-24.ily"

\ekmelicUserStyle diaQuarter #'(
    (1 #xF61C)
    (-1 #xF61D)
    (3/4 #xE265)
    (-3/4 #xE266)
    (1/2 #xE263)
    (-1/2 #xE264)
    (1/4 #xE262)
    (-1/4 #xE260)
    (5/4 #xF61C #xE262)
    (-5/4 #xF61D #xE260))

%-----------------------------------------------------------------------




%%----------------------------------------------------------------------
%% The following notation styles use markup,
%% in particular, to select glyphs from LilyPond's Emmentaler font
%% instead of an external (SMuFL compliant) font.
%%----------------------------------------------------------------------

%% Single glyph notation (48-EDO)
%% Standard sharp/flat symbols and other single glyphs for quarter-
%% and eighth-tones, as well as parentheses for cautionary accidentals.
%% [-]9/8 and [-]5/4 are omitted here since they are set automatically
%% to combinations of [-]1/8 and [-]1/4 with [-]1.

\include "ekmel-48.ily"

\ekmelicUserStyle lilysingle #`(
    (0 ,(markup #:natural))
    (1/8 ,(markup #:musicglyph "accidentals.natural.arrowup"))
    (-1/8 ,(markup #:musicglyph "accidentals.natural.arrowdown"))
    (1/4 ,(markup #:semisharp))
    (-1/4 ,(markup #:semiflat))
    (3/8 ,(markup #:musicglyph "accidentals.sharp.arrowdown"))
    (-3/8 ,(markup #:musicglyph "accidentals.flat.arrowup"))
    (1/2 ,(markup #:sharp))
    (-1/2 ,(markup #:flat))
    (5/8 ,(markup #:musicglyph "accidentals.sharp.arrowup"))
    (-5/8 ,(markup #:musicglyph "accidentals.flat.arrowdown"))
    (3/4 ,(markup #:sesquisharp))
    (-3/4 ,(markup #:sesquiflat))
    (7/8 ,(markup #:musicglyph "accidentals.sharp.slashslashslash.stemstem"))
    (-7/8 ,(markup #:musicglyph "accidentals.flatflat.slash"))
    (1 ,(markup #:doublesharp))
    (-1 ,(markup #:doubleflat))
    (leftparen ,(markup #:musicglyph "accidentals.leftparen"))
    (rightparen ,(markup #:musicglyph "accidentals.rightparen")))

%-----------------------------------------------------------------------

%% Combined notation (24-EDO)
%% Standard sharp/flat symbols, but two sharps instead of a doublesharp
%% symbol, combined with an open arrowhead for quarter-tones.

\include "ekmel-24.ily"

\ekmelicUserStyle lilycombined #`(
    (0 ,(markup #:natural))
    (1/4 ,(markup #:translate '(0 . 0.5) #:arrow-head Y UP #f))
    (-1/4 ,(markup #:translate '(0 . -0.5) #:arrow-head Y DOWN #f))
    (1/2 ,(markup #:sharp))
    (-1/2 ,(markup #:flat))
    (3/4 ,(markup #:combine
           #:sharp
           #:translate '(0.55 . 2.4) #:arrow-head Y UP #f))
    (-3/4 ,(markup #:combine
            #:flat
            #:translate '(0 . -1.6) #:arrow-head Y DOWN #f))
    (1 ,(markup #:pattern 2 X 0.15 #:sharp))
    (-1 ,(markup #:doubleflat))
    (5/4 ,(markup #:combine
           #:pattern 2 X 0.15 #:sharp
           #:translate '(1.15 . 2.4) #:arrow-head Y UP #f))
    (-5/4 ,(markup #:combine
            #:doubleflat
            #:translate '(0.35 . -1.6) #:arrow-head Y DOWN #f)))

%-----------------------------------------------------------------------

%% Numeric notation
%% Alteration values instead of accidental symbols.
%% They are drawn as fraction, or integer if the denominator is 1,
%% with the command "\ekmelic-fraction" and with a 3 steps smaller
%% font size.
%% This is applicable to all tunings.
%% The "void" notation ensures that the default accidental is used
%% for each alteration.

#(define-markup-command (numeric-accidental layout props)
  ()
  (let ((alt (ly:chain-assoc-get 'alteration props 0)))
    (interpret-markup layout
      (cons '((font-size . -3)) props)
      (markup #:vcenter #:ekmelic-fraction alt))))

\ekmelicStyle void

\ekmelicUserStyle numeric #`(
  (default ,(markup #:numeric-accidental)))

%-----------------------------------------------------------------------
