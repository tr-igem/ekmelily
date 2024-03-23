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
%% File: ekmel-arabic.ily  -  Include file for Arabic scores (24-EDO)
%% Latest revision: 2024-03-12
%%
%% This file is a variant of "ekmel-24.ily" for Arabic scores, like
%% LilyPond's "arabic.ly" but with the correct accidentals
%% (U+ED30 - U+ED38 in SMuFL).
%% It supports Arabic maqamat and defines only the Arabic notation and
%% Italian note names.
%%
%% The tables of Arabic maqamat are taken from "arabic.ly"
%% Copyright (C) 2017 Amir Czwink <amir130@hotmail.de>
%% Copyright (C) 2008 Neil Puttock
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (#x1A . 1/4)
  (#x28 . 1/2)
  (#x36 . 3/4)
  (#x44 . 1)
  (#x50 . 5/4))


% Language tables
ekmLanguages = #'(

;; Italian names by Paolo Zuliani <zuliap@easynet.it>,
;; Eric Wurbel <wurbel@univ-tln.fr>
(italiano . (
  2
  ()
  (#x1a sd)
  (#x1b sb)
  (#x28 d)
  (#x29 b)
  (#x36 dsd)
  (#x37 bsb)
  (#x44 dd)
  (#x45 bb)
  (#x50 ddsd)
  (#x51 bbsb)))
)


% Notation tables
ekmNotations = #'(

;; Arabic notation
(arabic . (
  (#x00 #xED34)
  (#x1A #xED35)
  (#x1B #xED33)
  (#x28 #xED36)
  (#x29 #xED32)
  (#x36 #xED37)
  (#x37 #xED31)
  (#x44 #xED38)
  (#x45 #xED30)
  (#x50 #xED35 #xED38)
  (#x51 #xED33 #xED30)))
)


\include "ekmel-main.ily"


%% Arabic maqamat ordered by maqam family

% Bayati family
bayati = #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

% Hijaz family
hijaz = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

hijaz_kar = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

% Kurd/Kurdi family
kurd = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

% Rast family
rast = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)

% Sikah family
sikah = #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,SEMI-FLAT)
  (3 . ,SEMI-SHARP)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,SEMI-FLAT)
)

iraq = #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,SEMI-FLAT)
  (5 . ,SEMI-FLAT)
  (6 . ,SEMI-FLAT)
)


%% Alteration order for key signatures with quarter tones
\layout {
  \context {
    \Score
    keyAlterationOrder = #'(
      (6 . -1/2) (2 . -1/2) (5 . -1/2) (1 . -1/2) (4 . -1/2) (0 . -1/2) (3 . -1/2)
      (6 . -1/4) (2 . -1/4) (5 . -1/4) (1 . -1/4) (4 . -1/4) (0 . -1/4) (3 . -1/4)
      (3 .  1/2) (0 .  1/2) (4 .  1/2) (1 .  1/2) (5 .  1/2) (2 .  1/2) (6 .  1/2)
      (3 .  1/4) (0 .  1/4) (4 .  1/4) (1 .  1/4) (5 .  1/4) (2 .  1/4) (6 .  1/4)
      (6 .   -1) (2 .   -1) (5 .   -1) (1 .   -1) (4 .   -1) (0 .   -1) (3 .   -1)
      (3 .    1) (0 .    1) (4 .    1) (1 .    1) (5 .    1) (2 .    1) (6 .    1)
    )
  }
}
