%% This file is part of Ekmelily - Notation of microtonal music with LilyPond.
%% Copyright (C) 2013-2024  Thomas Richter <thomas-richter@aon.at>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License at <http://www.gnu.org/licenses/>
%% for more details.
%%
%%
%% File: ekmel-53.ily  -  Include file for the 53-EDO tuning
%% Latest revision: 2024-03-26
%%
%%
%% This file is mainly intended for Turkish music based on the
%% Holdrian comma Hc = 1200 / 53
%%
%% Sources:
%%
%% * M. Kemal Karaosmanoğlu:
%%   "A TURKISH MAKAM MUSIC SYMBOLIC DATABASE FOR MUSIC INFORMATION
%%   RETRIEVAL: SymbTr" (ISMIR 2012)
%%
%% * Makam note names are taken from turkish-makam.ly in LilyPond 2.22
%%   Copyright (C) 2019--2020 Adam Good <goodadamgood@gmail.com>
%%

\version "2.19.22"


% Makam alterations (cp. 72-edo and 1/9-tone)
#(define-public KOMA          6/53)  % c   1/12   1/9
#(define-public CEYREK        12/53) %     3/12
#(define-public EKSIK-BAKIYE  18/53) % eb  4/12   3/9
#(define-public BAKIYE        24/53) % b   5/12   4/9
#(define-public KUCUK         30/53) % k   6/12   5/9
#(define-public BUYUKMUCENNEB 48/53) % bm  10/12  8/9
#(define-public TANINI        60/53) % t   11/12  10/9


% Tuning table
ekmTuning = #'(
  (-1     0 54/53 108/53 132/53 186/53 240/53 294/53)
  (#x12 . 6/53)
  (#x16 . 12/53)
  (#x20 . 18/53)
  (#x24 . 24/53)
  (#x28 . 30/53)
  (#x2E . 36/53)
  (#x32 . 42/53)
  (#x3C . 48/53)
  (#x40 . 54/53)
  (#x44 . 60/53))


% Language tables
ekmLanguages = #'(

;; Makam names
(makam . (
  0
  ()
  (#x12 c)
  (#x13 fc)
  (#x16 i)
  (#x17 fi)
  (#x20 eb)
  (#x21 fu)
  (#x24 b)
  (#x25 fb)
  (#x28 k)
  (#x29 fk)
  (#x3C bm)
  (#x3D fbm)
  (#x44 t)
  (#x45 ft)))

;; THM (Turkish folk music) names
;; Some combined names are unused
(thm . (
    2
    ()
    (#x17 -b-two)
    (#x20 -s-three)
    (#x25 -b-four)
    (#x29 -b)))

;; KTM (Turkish classical music) names
(ktm . (
  (cargah 0 . 0)
  (nimhicaz 0 . #x24)
  (hicaz 0 . #x28)
  (dikhicaz 0 . #x3C)
  (yegah 1 . 0)
  (nimhisar 1 . #x24)
  (hisar 1 . #x28)
  (dikhisar 1 . #x3C)
  (huseyniasiran 2 . 0)
  (acemasiran 3 . 0)
  (dikacem 3 . #x12)
  (irak 3 . #x24)
  (gevest 3 . #x28)
  (dikgevest 3 . #x3C)
  (rast 4 . 0)
  (nimzirgule 4 . #x24)
  (zirgule 4 . #x28)
  (dikzirgule 4 . #x3C)
  (dugah 5 . 0)
  (kurdi 5 . #x24)
  (dikkurdi 5 . #x28)
  (segah 5 . #x3C)
  (buselik 6 . 0)
  (dikbuselik 0 . #x13)))

;; English names
(english . (
  0
  ()
  (#x12 s)
  (#x13 f)
  (#x16 x)
  (#x17 ff)
  (#x20 sx)
  (#x21 fff)
  (#x24 xx)
  (#x25 ffff)))

;; English number names
;; Some combined names are unused
(number . (
  0
  ()
  (#x12 s-one)
  (#x13 b-one)
  (#x16 s-two)
  (#x17 b-two)
  (#x20 s-three)
  (#x21 b-three)
  (#x24 s-four)
  (#x25 b-four)))
)


% Notation tables
ekmNotations = #'(

;; AEU notation
(aeu . (
  (#x00 #xE261)
  (#x12 #xE444)
  (#x13 #xE443)
  (#x16)
  (#x17 #xF619)
  (#x20 #xE275)
  (#x21 #xF619)
  (#x24 #xE445)
  (#x25 #xE442)
  (#x28 #xE446)
  (#x29 #xE441)
  (#x2E)
  (#x2F)
  (#x32)
  (#x33)
  (#x3C #xE447)
  (#x3D #xE440)
  (#x40)
  (#x41)
  (#x44 #xED38)
  (#x45 #xED30)))

;; AEU notation with eksik-bakiye = koma (mirrored flat without slash)
(aeuek . (
  (#x00 #xE261)
  (#x12 #xE444)
  (#x13 #xE443)
  (#x16)
  (#x17 #xE443)
  (#x20 #xE275)
  (#x21 #xE443)
  (#x24 #xE445)
  (#x25 #xE442)
  (#x28 #xE446)
  (#x29 #xE441)
  (#x2E)
  (#x2F)
  (#x32)
  (#x33)
  (#x3C #xE447)
  (#x3D #xE440)
  (#x40)
  (#x41)
  (#x44 #xED38)
  (#x45 #xED30)))

;; THM notation
(thm . (
  (#x00 #xE261)
  (#x12 #xE450)
  (#x13 #xE454)
  (#x16 #xE451)
  (#x17 #xE455)
  (#x20 #xE452)
  (#x21 #xE456)
  (#x24 #xE445)
  (#x25 #xE457)
  (#x28 #xE453)
  (#x29 #xE441)
  (#x2E)
  (#x2F)
  (#x32)
  (#x33)
  (#x3C)
  (#x3D)
  (#x40)
  (#x41)
  (#x44)
  (#x45)))

;; Sagittal notation
(sag . (
  (#x00 #xE261)
  (#x12 #xE302)
  (#x13 #xE303)
  (#x16 #xE306)
  (#x17 #xE307)
  (#x20 #xE310)
  (#x21 #xE311)
  (#x24 #xE314)
  (#x25 #xE315)
  (#x28 #xE318)
  (#x29 #xE319)
  (#x2E #xE31E)
  (#x2F #xE31F)
  (#x32 #xE322)
  (#x33 #xE323)
  (#x3C #xE32C)
  (#x3D #xE32D)
  (#x40 #xE330)
  (#x41 #xE331)
  (#x44 #xE334)
  (#x45 #xE335)))

;; Diatonic notation
(dia . (
  (#x00 #xE261)
  (#x12 #xE262)
  (#x13 #xE260)
  (#x16 #xE263)
  (#x17 #xE264)
  (#x20 #xE265)
  (#x21 #xE266)
  (#x24 #xF61C)
  (#x25 #xF61D)
  (#x28)
  (#x29)
  (#x2E)
  (#x2F)
  (#x32)
  (#x33)
  (#x3C)
  (#x3D)
  (#x40)
  (#x41)
  (#x44)
  (#x45)))
)


% Padding table
ekmPadding = #'(
  (#xE450 . 1.1)
  (#xE454 . 1.2)
  (#xE455 . 1.1)
  (#xE456 . 1.1)
  (#xE457 . 1.1)
)


\include "ekmel-main.ily"
