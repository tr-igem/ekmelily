%%
%% Sample for Ekmelily
%% using markup to draw LilyPond Emmentaler accidentals.
%%

\version "2.24.0"

\include "ekmel-48.ily"

\language "nederlands"


\paper {
  left-margin = 12 \mm
  right-margin = 12 \mm
  bottom-margin = 10 \mm
  indent = 10 \mm
  short-indent = 10 \mm
  ragged-right = ##t
}


\markup \column {
  \wordwrap {
    Sample for
    \with-url #"http://www.ekmelic-music.org/en/extra/ekmelily.htm"
    \with-color #darkblue "Ekmelily"
    using markup to draw LilyPond accidentals.
  }
  \line { Font: \typewriter { Emmentaler }}
  \line { Include file: \typewriter { ekmel-48.ily }}
  \line { Notation style: \typewriter { user-defined }}
  \vspace #2
}

%%----------------------------------------------------------------------

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

%%----------------------------------------------------------------------

\score {
  \fixed c'' {
    c4
    ciq
    cih
    ciseq
    cis
    cisiq
    cisih
    cisqq
    cisis
    cisisiq
    cisisih
    r
    \break
    c4
    ceq
    ceh
    cesiq
    ces
    ceseq
    ceseh
    cesqq
    ceses
    ceseseq
    ceseseh
    r
  }

  \midi { }

  \layout {
    \context {
      \Score
      \omit BarNumber
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/16)
    }
    \context {
      \Staff
      \remove "Time_signature_engraver"
    }
  }
}
