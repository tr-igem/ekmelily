%%
%% Samples for Ekmelily
%% demonstrating the NoteNames context.
%%

\version "2.24.0"

\include "ekmel.ily"

% \ekmelicStyle arrow
% \ekmelicStyle rhm
% \ekmelicStyle sims
% \ekmelicStyle hesse
% \ekmelicStyle sag
% \ekmelicStyle msag
% \ekmelicStyle wys
% \ekmelicStyle gostz
% \ekmelicStyle gostc
% \ekmelicStyle bos
% \ekmelicStyle fern
% \ekmelicStyle haba

\ekmelicOutputSuffix
\pointAndClickOff


\paper {
  left-margin = 12 \mm
  right-margin = 12 \mm
  bottom-margin = 10 \mm
  indent = 10 \mm
  short-indent = 10 \mm
  ragged-right = ##t
  ragged-bottom = ##t
  system-system-spacing.basic-distance = #17
}

\layout {
  \context {
    \Score
    \omit BarNumber
    \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/32)
  }
  \context {
    \Staff
    \remove "Time_signature_engraver"
    \accidentalStyle neo-modern
  }
}


\markup \column {
  \line {
    Samples for
    \with-url #"http://www.ekmelic-music.org/en/extra/ekmelily.htm"
    \with-color #darkblue "Ekmelily"
    demonstrating the NoteNames context.
  }
  \line { Include file: \typewriter "ekmel.ily" }
  \line { Notation style: \typewriter { \ekmelic-style-name }}
  \line { Font: \typewriter { \ekmelic-font-name }}
  \line { Note names entered in language \typewriter "nederlands" . }
}

%%----------------------------------------------------------------------

melody = {
  ael4 eih' gil' diser'' <fisir' c'' ases''>1
  \bar "|"
  \break
}

music =
#(define-music-function (desc)
  (string?)
  #{
    s2 ^ \markup { \typewriter #desc }
    \melody
  #})

names =
#(define-music-function (acc)
  (boolean-or-symbol?)
  #{
    \set printAccidentalNames = #acc
    s2
    \melody
  #})

%%----------------------------------------------------------------------

\markup \column {
  \vspace #2.5
  \typewriter "\\set printAccidentalNames = #..."
  \vspace #1.2
}
\score {
  <<
    \new Staff {
      \cadenzaOn
      \music #"#t"
      \music #"#f"
      \music #"'lily"
      \music #"'all"
      \music #"'alteration"
      \music #"'fraction"
      \music #"'accidental"
    }
    \new NoteNames {
      \names ##t
      \names ##f
      \names #'lily
      \names #'all
      \names #'alteration
      \names #'fraction
      \names #'accidental
    }
  >>
}

\pageBreak

%%----------------------------------------------------------------------

\markup \column {
  \vspace #2.0
  \typewriter "\\set printNotesLanguage = \"norsk\""
  \typewriter "\\set printOctaveNames = ##t"
  \typewriter "\\set noteNameSeparator = \" \""
  \typewriter "\\set printAccidentalNames = #..."
  \vspace #1.2
}
\score {
  <<
    \new Staff {
      \cadenzaOn
      \music #"'lily"
      \music #"'all"
    }
    \new NoteNames {
      \set printNotesLanguage = "norsk"
      \set printOctaveNames = ##t
      \set noteNameSeparator = " "
      \names #'lily
      \names #'all
    }
  >>
}

%%----------------------------------------------------------------------

\markup \column {
  \vspace #4.0
  \typewriter "\\set printNotesLanguage = \"english\""
  \typewriter "\\set printOctaveNames = #'scientific"
  \typewriter "\\set noteNameSeparator = \" + \""
  \typewriter "\\set printAccidentalNames = #..."
  \vspace #1.2
}
\score {
  <<
    \new Staff {
      \cadenzaOn
      \music #"#t"
      \music #"'lily"
      \music #"'alteration"
    }
    \new NoteNames {
      \set printNotesLanguage = "english"
      \set printOctaveNames = #'scientific
      \set noteNameSeparator = " + "
      \names ##t
      \names #'lily
      \names #'alteration
    }
  >>
}

%%----------------------------------------------------------------------
