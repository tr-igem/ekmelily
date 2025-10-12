%%
%% Demonstrate a notation style of Ekmelily
%% for 53-EDO
%%

\version "2.19.0"

\include "ekmel-53.ily"
\include "../styledefs.ily"

\language "makam"

% \ekmStyle aeu
% \ekmStyle aeuek
% \ekmStyle dia
% \ekmStyle sag
% \ekmStyle thm

% \ekmStyle alteration
% \ekmStyle alteration-slash
% \ekmStyle step

languageNames = #'(
  "makam"
  ;"thm"  ;; for style thm instead of english
  ;"ktm"
  "english"
  ;"number"
)

\styleFontOutputSuffix


\markup \style-and-usage #"53-EDO" #"ekmel-53.ily"
\pageBreak
\markup \notenames-for

\score {
  %% Uncomment \removeWithTag
  %% to ignore the respective alteration

  %% (aeu, aeuek)
  % \removeWithTag #'aeu

  %% (thm)
  % \removeWithTag #'(noaeu thm)

  %% (sag alteration step)
  % \removeWithTag #'noaeu

  %% (dia)
  % \removeWithTag #'(noaeu dia thm)

  \new Staff \relative c'' {
    \scoreDefs

    \noteNamesAndAlter {
                 c
                 cc
      \tag #'aeu ci
      \tag #'noaeu { \hideNotes ci \unHideNotes }
                 ceb
                 cb
      \tag #'dia ck
      \tag #'thm cbm
      \tag #'thm ct
      \bar "" \break
                 c
                 cfc
                 cfi
                 cfu
                 cfb
      \tag #'dia cfk
      \tag #'thm cfbm
      \tag #'thm cft
      \bar "|" \break

                 g
                 gc
      \tag #'aeu gi
      \tag #'noaeu { \hideNotes gi \unHideNotes }
                 geb
                 gb
      \tag #'dia gk
      \tag #'thm gbm
      \tag #'thm gt
      \bar "" \break
                 g
                 gfc
                 gfi
                 gfu
                 gfb
      \tag #'dia gfk
      \tag #'thm gfbm
      \tag #'thm gft
      \bar "|" \break

      %% notes for style thm
      %c,
      %ceb
      %dfb
      %d
      %efk
      %efi
      %e
      %f
      %feb
      %\bar "" \break
      %gfb
      %g
      %afk
      %afi
      %a
      %bfk
      %bfi
      %b
      %\bar "|" \break
    }
  }

  \layout { }
}
