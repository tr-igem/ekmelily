%%
%% Demonstrate a notation style of Ekmelily
%% for 48-EDO
%%

\version "2.19.0"

\include "ekmel-48.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmStyle gostz
% \ekmStyle msag
% \ekmStyle sag

% \ekmStyle alteration
% \ekmStyle alteration-slash
% \ekmStyle step

languageNames = #'(
  "nederlands"
  "english"
  ;"deutsch"
)

\styleFontOutputSuffix


\markup \style-and-usage #"48-EDO" #"ekmel-48.ily"
\pageBreak
\markup \notenames-for

\score {
  \new Staff \relative c'' {
    \scoreDefs

    \noteNamesAndAlter {
      c cil cih ciseh cisel cis cisil cisih cisiseh cisisel cisis cisisil cisisih
      \bar "" \break
      c cel ceh cesih cesil ces cesel ceseh cesesih cesesil ceses cesesel ceseseh
      \bar "|" \break
      g gil gih giseh gisel gis gisil gisih gisiseh gisisel gisis gisisil gisisih
      \bar "" \break
      g gel geh gesih gesil ges gesel geseh gesesih gesesil geses gesesel geseseh
      \bar "|" \break
    }
  }

  \layout { }
}
