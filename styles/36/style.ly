%%
%% Demonstrate a notation style of Ekmelily
%% for 36-EDO
%%

\version "2.19.0"

\include "ekmel-36.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmStyle arrow
% \ekmStyle go
% \ekmStyle haba
% \ekmStyle msag
% \ekmStyle sag
% \ekmStyle wys

% \ekmStyle alteration
% \ekmStyle alteration-slash
% \ekmStyle step

languageNames = #'(
  "nederlands"
  "english"
  ;"deutsch"
  ;"norsk"
  ;"suomi"
  ;"svenska"
)

\styleFontOutputSuffix


\markup \style-and-usage #"36-EDO" #"ekmel-36.ily"
\pageBreak
\markup \notenames-for

\score {
  \new Staff \relative c' {
    \scoreDefs

    \noteNamesAndAlter {
      c cil cisel cis cisil cisisel cisis cisisil
      d dil disel dis disil disisel disis disisil
      \bar "" \break
      e eil eisel eis eisil eisisel eisis eisisil
      f fil fisel fis fisil fisisel fisis fisisil
      \bar "" \break
      g gil gisel gis gisil gisisel gisis gisisil
      a ail aisel ais aisil aisisel aisis aisisil
      \bar "" \break
      h hil hisel his hisil hisisel hisis hisisil
      \bar "|"
      c cel cesil ces cesel cesesil ceses cesesel
      \bar "" \break
      h hel hesil hes hesel hesesil heses hesesel
      a al  asil  as  asel  asesil  ases  asesel
      \bar "" \break
      g gel gesil ges gesel gesesil geses gesesel
      f fel fesil fes fesel fesesil feses fesesel
      \bar "" \break
      e el  esil  es  esel  esesil  eses  esesel
      d del desil des desel desesil deses desesel
      \bar "|" \break
    }
  }

  \layout { }
}
