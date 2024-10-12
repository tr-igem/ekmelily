%%
%% Demonstrate a notation style of Ekmelily
%% for 31-EDO
%%

\version "2.19.0"

\include "ekmel-31.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmelicStyle msag
% \ekmelicStyle sag
% \ekmelicStyle std
% \ekmelicStyle sth
% \ekmelicStyle stz

% \ekmelicStyle alteration
% \ekmelicStyle step

languageNames = #'(
  "nederlands"
  ;"deutsch"
  ;"español"
  "italiano"
  ;"français"
  ;"português"
)

\styleFontOutputSuffix


\markup \style-and-usage #"31-EDO" #"ekmel-31.ily"
\pageBreak
\markup \notenames-for

\score {
  \new Staff \relative c' {
    \scoreDefs

    \noteNamesAndAlter {
      c ci cis cisi cisis cisisi
      d di dis disi disis disisi
      \bar "" \break
      e ei eis eisi eisis eisisi
      f fi fis fisi fisis fisisi
      \bar "" \break
      g gi gis gisi gisis gisisi
      a ai ais aisi aisis aisisi
      \bar "" \break
      h hi his hisi hisis hisisi
      \bar "|"
      c ce ces cese ceses cesese
      \bar "" \break
      h he hes hese heses hesese
      a ah as ase ases asese
      \bar "" \break
      g ge ges gese geses gesese
      f fe fes fese feses fesese
      \bar "" \break
      e eh es ese eses esese
      d de des dese deses desese
      \bar "|" \break
    }
  }

  \layout { }
}
