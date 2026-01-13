%%
%% Samples for Ekmelily
%% demonstrating accidental symbols in various use cases.
%%

\version "2.24.0"

ekmUse = "72 sims +"
% ekmUse = "72 sag +"
\include "cosmufl.ily"

\ekmelicOutputSuffix
\pointAndClickOff


\paper {
  left-margin = 12 \mm
  right-margin = 12 \mm
  bottom-margin = 10 \mm
  indent = 10 \mm
  short-indent = 10 \mm
  ragged-right = ##t
}

\layout {
  \context {
    \Score
    \omit BarNumber
    \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/16)
  }
  \context {
    \Staff
    \remove "Time_signature_engraver"
    \accidentalStyle neo-modern
  }
}


\markup \column {
  \line {
    Samples for Ekmelily demonstrating accidental symbols in various use cases.
  }
  \line {
    Tuning: \typewriter { \ekm-tuning } ,
    Notation style: \typewriter { \ekmelic-style-name }
  }
  \vspace #1
}

%%----------------------------------------------------------------------

\markup \line { Normal, cautionary, and suggested accidentals }
\score {
  \new Staff \relative c'' {
    cir4 cil cih cis
    der del deh des
    cil? c?
    \set suggestAccidentals = ##t
    cis deh
  }
}

%%----------------------------------------------------------------------

\markup \line { Key signatures }
\score {
  \new Staff \relative c'' {
    \key f #'((0 . 0)
              (1 . 0)
              (2 . -1/4)
              (3 . 0)
              (4 . 0)
              (5 . 0)
              (6 . -1/4))
    aeh4 ceh eeh c

    \key c #'((0 . 1/4)
              (1 . 0)
              (2 . 0)
              (3 . 1/4)
              (4 . 0)
              (5 . 0)
              (6 . 0))
    cih4 ciseh e fih
  }
  \layout {
    \context {
      \Score
      keyAlterationOrder = #'(
        (6 . -1/2) (2 . -1/2) (5 . -1/2) (1 . -1/2) (4 . -1/2) (0 . -1/2) (3 . -1/2)
        (3 .  1/2) (0 .  1/2) (4 .  1/2) (1 .  1/2) (5 .  1/2) (2 .  1/2) (6 .  1/2)
        (6 . -1/4) (2 . -1/4) (5 . -1/4) (1 . -1/4) (4 . -1/4) (0 . -1/4) (3 . -1/4)
        (3 .  1/4) (0 .  1/4) (4 .  1/4) (1 .  1/4) (5 .  1/4) (2 .  1/4) (6 .  1/4)
        (6 . -3/4) (2 . -3/4) (5 . -3/4) (1 . -3/4) (4 . -3/4) (0 . -3/4) (3 . -3/4)
        (3 .  3/4) (0 .  3/4) (4 .  3/4) (1 .  3/4) (5 .  3/4) (2 .  3/4) (6 .  3/4)
        (6 .   -1) (2 .   -1) (5 .   -1) (1 .   -1) (4 .   -1) (0 .   -1) (3 .   -1)
        (3 .    1) (0 .    1) (4 .    1) (1 .    1) (5 .    1) (2 .    1) (6 .    1)
      )
    }
  }
}

%%----------------------------------------------------------------------

\markup \line { Grace note accidentals }
\score {
  \new Staff \relative c'' {
    \grace ber8 a4
    \slashedGrace bil16 c4
    \appoggiatura eeh8 d4
    \acciaccatura { besel16 bir } c4
    \grace { cel16 dih e fir } e1
  }
}

%%----------------------------------------------------------------------

\markup \line { Trill pitch accidentals, measure 3 uses \typewriter "cosmufl.ily" }
\score {
  \new Staff
  \relative c'' {
    \pitchedTrill d1 \startTrillSpan eil
    d1 \startTrillSpan ^ \markup \ekmelic-char #-1/4
    \ekmSmuflOn #'trill
    d1 \ekmStartTrillSpanAccidental #0 #-1/4
  }
}

%%----------------------------------------------------------------------

\markup \line { Ambitus accidentals }
\score {
  \new Staff \relative c'' {
    c4 eisel d a cesir,1
  }
  \layout {
    \context {
      \Staff
      \consists "Ambitus_engraver"
    }
  }
}

%%----------------------------------------------------------------------

\markup \line { Accidentals in chord names }
\score {
  <<
    \new ChordNames {
      \chordmode {
        ces1
        cis
        cih
        c:aug7
        <c' e' ges' b'>
      }
    }
    \new Staff {
      \accidentalStyle default
      \chordmode {
        ces1
        cis
        cih
        c:aug7
        <c' e' ges' b'>
      }
    }
  >>
}

%%----------------------------------------------------------------------

\markup \line { Accidentals in cue voices }
\score {
  \new Staff \relative c'' {
    <<
      {
        \override MultiMeasureRest.staff-position = #-6
        R1*5
      }
      \new CueVoice \relative c'' {
        cir4 cil cih c

        \grace ber8 a4
        \slashedGrace bil16 c4
        \appoggiatura eeh8 d4
        \acciaccatura { besel16 bir } c4

        \pitchedTrill d1 \startTrillSpan eil
        d1 \startTrillSpan ^ \markup \ekmelic-char #-1/4
        \ekmSmuflOn #'trill
        d1 \ekmStartTrillSpanAccidental #0 #-1/4
      }
    >>
    cir1
  }
}

%%----------------------------------------------------------------------

\markup \line { Accidentals in ossia staves (with a 3 steps smaller font size) }
\score {
  \new Staff = "main"
  \relative c'' {
    <<
      \new Staff \with {
        alignAboveContext = #"main"
        \remove "Time_signature_engraver"
        fontSize = #-3
        \override StaffSymbol.staff-space = #(magstep -3)
        \override StaffSymbol.thickness = #(magstep -3)
        \override VerticalAxisGroup.staff-staff-spacing.basic-distance = #5
        \accidentalStyle neo-modern
      }
      \relative c'' {
        cir4 cil cih c

        \grace ber8 a4
        \slashedGrace bil16 c4
        \appoggiatura eeh8 d4
        \acciaccatura { besel16 bir } c4

        \pitchedTrill d1 \startTrillSpan eil
        d1 \startTrillSpan ^ \markup \ekmelic-char #-1/4
        \ekmSmuflOn #'trill
        d1 \ekmStartTrillSpanAccidental #0 #-1/4
      }
      \relative c'' {
        R1*5
      }
    >>
    cir1
  }
}

%%----------------------------------------------------------------------
