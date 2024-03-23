%%
%% Demonstrate a notation style of Ekmelily
%% for 24-EDO
%%

\version "2.19.0"

\include "ekmel-24.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmelicStyle stc
% \ekmelicStyle stz
% \ekmelicStyle go
% \ekmelicStyle stvt
% \ekmelicStyle arrow
% \ekmelicStyle sag
% \ekmelicStyle msag
% \ekmelicStyle arabic
% \ekmelicStyle persian
% \ekmelicStyle four
% \ekmelicStyle haba

%userStyle = #'(
%  "persianext"
%  ((-1/4 #xE460)
%   (1/4 #xE461)
%   (-3/4 #xF674)
%   (3/4 #xF675)
%  )
%  ((#x00 #xE261)
%   (#x1A #xE461) ; 1/4
%   (#x1B #xE460)
%   (#x28 #xE262) ; 1/2
%   (#x29 #xE260)
%   (#x36 #xF675) ; 3/4
%   (#x37 #xF674)
%   (#x44 #xE263) ; 1
%   (#x45 #xE264)
%   ;(#x50 #) ; 5/4
%   ;(#x51 #)
%))
%
%\ekmelicUserStyle #(first userStyle) #(second userStyle)

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


\markup \style-and-usage #"24-EDO" #"ekmel-24.ily"
\pageBreak
\markup \notenames-for

\score {
  \new Staff \relative c' {
    \scoreDefs

    \noteNamesAndAlter {
      c cih ciseh cis cisih cisiseh cisis cisisih
      d dih diseh dis disih disiseh disis disisih
      \bar "" \break
      e eih eiseh eis eisih eisiseh eisis eisisih
      f fih fiseh fis fisih fisiseh fisis fisisih
      \bar "" \break
      g gih giseh gis gisih gisiseh gisis gisisih
      a aih aiseh ais aisih aisiseh aisis aisisih
      \bar "" \break
      h hih hiseh his hisih hisiseh hisis hisisih
      \bar "|"
      c ceh cesih ces ceseh cesesih ceses ceseseh
      \bar "" \break
      h heh hesih hes heseh hesesih heses heseseh
      a ah  asih  as  aseh  asesih  ases  aseseh
      \bar "" \break
      g geh gesih ges geseh gesesih geses geseseh
      f feh fesih fes feseh fesesih feses feseseh
      \bar "" \break
      e eh  esih  es  eseh  esesih  eses  eseseh
      d deh desih des deseh desesih deses deseseh
      \bar "|" \break
    }
  }

  \layout { }
}
