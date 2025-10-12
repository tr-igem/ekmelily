%%
%% Demonstrate a notation style of Ekmelily
%% for 19-EDO
%%

\version "2.19.0"

\include "ekmel-19.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmStyle msag
% \ekmStyle sag
% \ekmStyle std

% \ekmStyle alteration
% \ekmStyle alteration-slash
% \ekmStyle step

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


\markup \style-and-usage #"19-EDO" #"ekmel-19.ily"
\pageBreak
\markup \notenames-for

\score {
  \new Staff \relative c' {
    \scoreDefs

    \noteNamesAndAlter {
      c cis cisis
      d dis disis
      e eis
      f fis fisis
      g gis gisis
      a ais aisis
      h his
      \bar"" \break
      c ces
      h hes heses
      a as ases
      g ges geses
      f fes
      e es eses
      d des deses
      \bar "|" \break
    }
  }

  \layout { }
}
