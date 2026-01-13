%%
%% Sample for Ekmelily
%% Adaption of LSR "Microtonal notation and theory of harmony"
%% <http://lsr.di.unimi.it/LSR/Item?id=786>
%%
%% from J. S. Bach: St. Matthew Passion,
%% "Was mein Gott will, das gescheh allzeit"
%%

\version "2.24.0"

% ekmFont = "Bravura"
\include "ekmel-24.ily"

% \ekmStyle stc
% \ekmStyle stz
% \ekmStyle go
% \ekmStyle stvt
% \ekmStyle arrow
% \ekmStyle sag
% \ekmStyle msag
% \ekmStyle arabic
% \ekmStyle persian
% \ekmStyle four
% \ekmStyle haba
% \ekmStyle bl

\ekmelicOutputSuffix
\pointAndClickOff


\paper {
  left-margin = 12 \mm
  right-margin = 12 \mm
  bottom-margin = 10 \mm
  indent = 20 \mm
  short-indent = 20 \mm
  ragged-right = ##t
}


\markup \column {
  \line {
    Sample for
    \with-url #"https://github.com/tr-igem/ekmelily"
    \with-color #darkblue "Ekmelily"
  }
  \line {
    Adaption of LSR
    \with-url #"http://lsr.di.unimi.it/LSR/Item?id=786"
    \with-color #darkblue "\"Microtonal notation and theory of harmony\""
  }
  \line {
    from J. S. Bach: St. Matthew Passion,
    "\"Was mein Gott will, das gescheh allzeit\""
  }
  \line { Font: \typewriter { \ekmelic-font-name }}
  \line { Include file: \typewriter { ekmel-24.ily }}
  \line { Notation style: \typewriter { \ekmelic-style-name }}
  \vspace #2
}

%%----------------------------------------------------------------------

justMinor = #'(
  (0 . 0)
  (1 . 0)
  (2 . -1/4)
  (3 . 0)
  (4 . 0)
  (5 . -1/4)
  (6 . -1/4)
)

\score {
  <<
    \new Staff \with {
      instrumentName = \markup \center-column { "Sopran" "Alto" }
    }
    \relative c'' {
      \key b \justMinor
      <<
        {
          \partial 4
          dih4 | ciseh b e dih8 cis | cis2 b
        } \\ {
          fis4 | e8 fis giseh aiseh b4 b | b aiseh fis2
        }
      >>
    }

    \new Staff \with {
      instrumentName = \markup \center-column { "Tenor" "Bass" }
    }
    \relative c' {
      \key b \justMinor
      <<
        {
          \clef "treble_8"
          aih8 b | ciseh diseh e4 b8 ciseh dih4 | gis, cis diseh2
        } \\ {
          fis,8 giseh | a4 giseh gih fis | eiseh fis b,2
        }
      >>
    }
  >>

  \layout {
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/16)
      \accidentalStyle modern
    }
  }
}
