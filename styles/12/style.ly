%%
%% Demonstrate a notation style of Ekmelily
%% for 12-EDO
%%

\version "2.19.0"

\include "ekmel-12.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmelicStyle std
% \ekmelicStyle sag
% \ekmelicStyle msag

languageNames = #'(
  "nederlands"
  "english"
  ;"deutsch"
  ;"català"
  ;"español"
  "italiano"
  ;"français"
  ;"português"
  ;"norsk"
  ;"suomi"
  ;"svenska"
  ;"vlaams"
)

\styleFontOutputSuffix


\markup \style-and-usage #"12-EDO" #"ekmel-12.ily"
\pageBreak
\markup \notenames-for

\score {
  \new Staff \relative c' {
    \scoreDefs

    \noteNamesAndAlter {
      c cis cisis
      d dis disis
      e eis eisis
      f fis fisis
      g gis gisis
      a ais aisis
      h his hisis
      \bar"" \break
      c ces ceses
      h hes heses
      a as ases
      g ges geses
      f fes feses
      e es eses
      d des deses
      \bar "|" \break
    }
  }

  \layout { }
}
