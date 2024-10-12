%%
%% Demonstrate a notation style of Ekmelily
%% for Arabic (24-EDO)
%%

\version "2.19.0"

\include "ekmel-arabic.ily"
\include "../styledefs.ily"

\language "arabic"

% \ekmelicStyle arabic
% \ekmelicStyle helmakam

% \ekmelicStyle alteration
% \ekmelicStyle step

languageNames = #'(
  "italiano"
  "arabic"
)

\styleFontOutputSuffix


\markup \style-and-usage #"Arabic (24-EDO)" #"ekmel-arabic.ily"
\pageBreak
\markup \notenames-for

\score {
  %% Uncomment \removeWithTag
  %% to ignore the respective alteration

  %% (arabic)
  % \removeWithTag #'arabic

  \new Staff \relative c' {
    \scoreDefs

    \noteNamesAndAlter {
      c cdd cd ctqd cx \tag #'arabic { cfhd cshd }
      d ddd dd dtqd dx \tag #'arabic { dfhd dshd }
      \bar "" \break
      e edd ed etqd ex \tag #'arabic { efhd eshd }
      f fdd fd ftqd fx \tag #'arabic { ffhd fshd }
      \bar "" \break
      g gdd gd gtqd gx \tag #'arabic { gfhd gshd }
      a add ad atqd ax \tag #'arabic { afhd ashd }
      \bar "" \break
      b bdd bd btqd bx \tag #'arabic { bfhd bshd }
      \bar "|"
      c cdb cb ctqb cbb \tag #'arabic { cfhb cshb }
      \bar "" \break
      b bdb bb btqb bbb \tag #'arabic { bfhb bshb }
      a adb ab atqb abb \tag #'arabic { afhb ashb }
      \bar "" \break
      g gdb gb gtqb gbb \tag #'arabic { gfhb gshb }
      f fdb fb ftqb fbb \tag #'arabic { ffhb fshb }
      \bar "" \break
      e edb eb etqb ebb \tag #'arabic { efhb eshb }
      d ddb db dtqb dbb \tag #'arabic { dfhb dshb }
      \bar "|" \break
    }
  }

  \layout { }
}
