%%
%% Demonstrate a notation style of Ekmelily
%% for 5-limit JI
%%

\version "2.19.0"

\include "ekmel-5ji.ily"
\include "../styledefs.ily"

% \language "nederlands"

% \ekmelicStyle he
% \ekmelicStyle msag
% \ekmelicStyle sag

% \ekmelicStyle alteration
% \ekmelicStyle step

languageNames = #'(
  "nederlands"
  ;"english"
  ;"deutsch"
  ;"català"
  ;"español"
  ;"italiano"
  ;"français"
  ;"português"
  ;"norsk"
  ;"suomi"
  ;"svenska"
  ;"vlaams"
)

\styleFontOutputSuffix


\markup \style-and-usage #"5-limit JI" #"ekmel-5ji.ily"
\pageBreak
\markup \notenames-for

\score {
  \new Staff \relative c'' {
    \scoreDefs

    \noteNamesAndAlter {
      c cih cihh cihhh cisehhh cisehh ciseh
      cis cisih cisihh cisihhh cisisehhh cisisehh cisiseh
      cisis cisisih cisisihh cisisihhh
      \bar "" \break
      c ceh cehh cehhh cesihhh cesihh cesih
      ces ceseh cesehh cesehhh cesesihhh cesesihh cesesih
      ceses ceseseh cesesehh cesesehhh
      \bar "|" \break

      g gih gihh gihhh gisehhh gisehh giseh
      gis gisih gisihh gisihhh gisisehhh gisisehh gisiseh
      gisis gisisih gisisihh gisisihhh
      \bar "" \break
      g geh gehh gehhh gesihhh gesihh gesih
      ges geseh gesehh gesehhh gesesihhh gesesihh gesesih
      geses geseseh gesesehh gesesehhh
      \bar "|" \break
    }
  }

  \layout { }
}
