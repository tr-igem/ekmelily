%%
%% Demonstrate a notation style of Ekmelily
%% for 72-EDO
%%

\version "2.19.0"

\include "ekmel.ily"
\include "../styledefs.ily"

\language "deutsch"

% \ekmelicStyle arrow
% \ekmelicStyle bos
% \ekmelicStyle fern
% \ekmelicStyle gostc
% \ekmelicStyle gostz
% \ekmelicStyle haba
% \ekmelicStyle hesse
% \ekmelicStyle msag
% \ekmelicStyle rhm
% \ekmelicStyle sag
% \ekmelicStyle sims
% \ekmelicStyle wys

% \ekmelicStyle alteration
% \ekmelicStyle step

%% HEWM (Helmholtz / Ellis / Wolf / Monzo) notation
%userStyle = #'(
%  "hewm"
%  ((0 #\n)
%   (1 #\x)
%   (-1 #\b #\b)
%   (1/2 #\#)
%   (-1/2 #\b)
%   (1/4 #\^)
%   (-1/4 #\v)
%   (1/6 #\>)
%   (-1/6 #\<)
%   (1/12 #\+)
%   (-1/12 #\-))
%  ((#x00 #\n)
%   (#x12 #\+) ; 1/12
%   (#x13 #\-)
%   (#x14 #\>) ; 1/6
%   (#x15 #\<)
%   (#x1A #\^) ; 1/4
%   (#x1B #\v)
%   (#x28 #\#) ; 1/2
%   (#x29 #\b)
%   (#x44 #\x) ; 1
%))

%% Sims/Maneri ASCII notation
%userStyle = #'(
%  "simsascii"
%  ((0 #\n)
%   (1 #\x)
%   (-1 #\b #\b)
%   (1/2 #\#)
%   (-1/2 #\b)
%   (1/4 #\[)
%   (-1/4 #\])
%   (1/6 #\>)
%   (-1/6 #\<)
%   (1/12 #\^)
%   (-1/12 #\v))
%  ((#x00 #\n)
%   (#x12 #\^) ; 1/12
%   (#x13 #\v)
%   (#x14 #\>) ; 1/6
%   (#x15 #\<)
%   (#x1A #\[) ; 1/4
%   (#x1B #\])
%   (#x28 #\#) ; 1/2
%   (#x29 #\b)
%   (#x44 #\x) ; 1
%))

% \ekmelicUserStyle #(first userStyle) #(second userStyle)

languageNames = #'(
  "nederlands"
  "english"
  ;"deutsch"
  ;"norsk"
  ;"suomi"
  ;"svenska"
)

\styleFontOutputSuffix


\markup \style-and-usage #"72-EDO" #"ekmel.ily"
\pageBreak
\markup \notenames-for

\score {
  %% Uncomment \removeWithTag
  %% to ignore the respective alteration

  %% 1/12
  %% (go gostc gostz stz stc stvt four fern stockhausen)
  % \removeWithTag #'r

  %% 1/6
  %% (go stz stc stvt four fern stockhausen)
  % \removeWithTag #'l

  %% 1/3
  %% (go stz stc stvt four stockhausen)
  % \removeWithTag #'d

  %% 1/4
  % \removeWithTag #'h

  \new Staff \relative c'' {
    \scoreDefs

    \noteNamesAndAlter {
               c
      \tag #'r cir
      \tag #'l cil
      \tag #'h cih
      \tag #'h ciseh
      \tag #'d cisel
      \tag #'r ciser
      \tag #'h cis
      \tag #'r cisir
      \tag #'d cisil
      \tag #'h cisih
      \tag #'h cisiseh
      \tag #'l cisisel
      \tag #'r cisiser
               cisis
      \tag #'r cisisir
      \tag #'l cisisil
      \tag #'h cisisih
      \bar "" \break
               c
      \tag #'r cer
      \tag #'l cel
      \tag #'h ceh
      \tag #'h cesih
      \tag #'d cesil
      \tag #'r cesir
      \tag #'h ces
      \tag #'r ceser
      \tag #'d cesel
      \tag #'h ceseh
      \tag #'h cesesih
      \tag #'l cesesil
      \tag #'r cesesir
               ceses
      \tag #'r ceseser
      \tag #'l cesesel
      \tag #'h ceseseh
      \bar "|" \break

               g
      \tag #'r gir
      \tag #'l gil
      \tag #'h gih
      \tag #'h giseh
      \tag #'d gisel
      \tag #'r giser
      \tag #'h gis
      \tag #'r gisir
      \tag #'d gisil
      \tag #'h gisih
      \tag #'h gisiseh
      \tag #'l gisisel
      \tag #'r gisiser
               gisis
      \tag #'r gisisir
      \tag #'l gisisil
      \tag #'h gisisih
      \bar "" \break
               g
      \tag #'r ger
      \tag #'l gel
      \tag #'h geh
      \tag #'h gesih
      \tag #'d gesil
      \tag #'r gesir
      \tag #'h ges
      \tag #'r geser
      \tag #'d gesel
      \tag #'h geseh
      \tag #'h gesesih
      \tag #'l gesesil
      \tag #'r gesesir
               geses
      \tag #'r geseser
      \tag #'l gesesel
      \tag #'h geseseh
      \bar "|" \break
    }
  }

  \layout { }
}
