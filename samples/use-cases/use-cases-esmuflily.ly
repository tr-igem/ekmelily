%%
%% Samples for Ekmelily + Esmuflily
%% demonstrating accidental symbols in various use cases.
%%

\version "2.24.0"

% ekmFont = "#"

% \include "../../../Ekmelos/ly/ekmelos-paths.ily"
\include "ekmel.ily"
\include "esmufl.ily"
\language "deutsch"

% \ekmelicStyle arrow
% \ekmelicStyle bos
% \ekmelicStyle fern
% \ekmelicStyle gostc
% \ekmelicStyle gostz
% \ekmelicStyle haba
% \ekmelicStyle hesse
% \ekmelicStyle msag
% \ekmelicStyle rhm
% \ekmelicStyle sag
% \ekmelicStyle sims
% \ekmelicStyle wys

% \ekmelicStyle alteration
% \ekmelicStyle step

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

    \ekmSmuflOn #'all
    \override NoteHead.style = #'triangle
    \override AmbitusNoteHead.style = #'cross
  }
  \context {
    \Staff
    \remove "Time_signature_engraver"
    \accidentalStyle neo-modern
  }
}


\markup \column {
  "Samples for Ekmelily + Esmuflily demonstrating accidental symbols in various use cases."
  \line { "Include files:" \typewriter { ekmel.ily } , \typewriter { esmufl.ily }}
  \line { "Notation style:" \typewriter { \ekmelic-style-name }}
  \line { "Note head style:" \typewriter { triangle }}
  \line { "Ambitus note head style:" \typewriter { cross }}
  \vspace #2
}

%%----------------------------------------------------------------------

\markup "Normal, cautionary, and suggested accidentals:"
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

\markup "Key signatures:"
\score {
  \new Staff \relative c'' {
    \key f #'((0 . 0)
              (1 . 0)
              (2 . -1/4)
              (3 . 0)
              (4 . 0)
              (5 . -1/4)
              (6 . -1/4))
    ah4 ceh eh c

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

\markup "Grace note accidentals:"
\score {
  \new Staff \relative c'' {
    \grace her8 a4
    \slashedGrace hil16 c4
    \appoggiatura eh8 d4
    \acciaccatura { hesel16 hir } c4
    \grace { cel16 dih e fir } e1
  }
}

%%----------------------------------------------------------------------

\markup "Trill pitch accidentals:"
\score {
  \new Staff \relative c'' {
    \pitchedTrill d1 \startTrillSpan eil
    d1 \startTrillSpan ^ \markup \ekmelic-char #1/6
  }
}

%%----------------------------------------------------------------------

\markup "Ambitus accidentals:"
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

\markup "Accidentals in cue voices:"
\score {
  \new Staff \relative c'' {
    <<
      {
        \override MultiMeasureRest.staff-position = #-6
        R1*4
      }
      \new CueVoice \relative c'' {
        cir4 cil cih c

        \grace her8 a4
        \slashedGrace hil16 c4
        \appoggiatura eh8 d4
        \acciaccatura { hesel16 hir } c4

        \pitchedTrill d1 \startTrillSpan eil
        d1 \startTrillSpan ^ \markup \ekmelic-char #1/6
      }
    >>
    cir1
  }
}

%%----------------------------------------------------------------------

\markup "Accidentals in ossia staves (here with a 3 steps smaller font size):"
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

        \grace her8 a4
        \slashedGrace hil16 c4
        \appoggiatura eh8 d4
        \acciaccatura { hesel16 hir } c4

        \pitchedTrill d1 \startTrillSpan eil
        d1 \startTrillSpan ^ \markup \ekmelic-char #1/6
      }
      \relative c'' {
        R1*4
      }
    >>
    cir1
  }
}

%%----------------------------------------------------------------------
