%%
%% Demonstrate a notation style of Ekmelily
%% for 96-EDO
%%

\version "2.19.0"

\include "ekmel-96.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmStyle msag
% \ekmStyle om
% \ekmStyle persian
% \ekmStyle sag

% \ekmStyle alteration
% \ekmStyle alteration-slash
% \ekmStyle step

languageNames = #'(
  "nederlands"
  "english"
  "deutsch"
)

\styleFontOutputSuffix


\markup \style-and-usage #"96-EDO" #"ekmel-96.ily"
\pageBreak
\markup \notenames-for

\score {
  %% Uncomment \removeWithTag
  %% to ignore the respective alteration

  %% (om = no negative alterations)
  % \removeWithTag #'om

  \new Staff \relative c'' {
    \scoreDefs

    \noteNamesAndAlter {
      c cir cil ciher cih ciseh cihir cisel ciser cis
      cisir cisil cisiher cisih cisiseh cisihir cisisel cisiser
      \tag #'om {
        cisis
        \bar "" \break
        c cer cel cehir ceh cesih ceher cesil cesir ces
        ceser cesel cesehir ceseh cesesih ceseher cesesil cesesir ceses
      }
      \bar "|" \break

      g gir gil giher gih giseh gihir gisel giser gis
      gisir gisil gisiher gisih gisiseh gisihir gisisel gisiser
      \tag #'om {
        gisis
        \bar "" \break
        g ger gel gehir geh gesih geher gesil gesir ges
        geser gesel gesehir geseh gesesih geseher gesesil gesesir geses
      }
      \bar "|" \break
    }
  }

  \layout { }
}
