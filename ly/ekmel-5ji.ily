%% This file is part of Ekmelily - Notation of microtonal music with LilyPond.
%% Copyright (C) 2024-2025  Thomas Richter <thomas-richter@aon.at>
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
%% File: ekmel-5ji.ily  -  Include file for 5-limit JI
%%
%%
%% Note names from the semitone and quarter-tone names in LilyPond 2.22
%% Copyright (C) 2010--2020 Valentin Villenave <valentin@villenave.net> et al.
%%
%% Pitches from
%% http://lsr.di.unimi.it/LSR/Item?id=786
%% https://marsbat.space/pdfs/notation.pdf
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (-1     0 204/200 408/200 498/200 702/200 906/200 1110/200)
  (#x12 .  22/200)
  (#x16 .  43/200)
  (#x1E .  65/200)
  (#x1A .  50/200)
  (#x20 .  71/200)
  (#x24 .  92/200)
  (#x28 . 114/200)
  (#x2E . 136/200)
  (#x32 . 157/200)
  (#x3A . 179/200)
  (#x36 . 164/200)
  (#x3C . 185/200)
  (#x40 . 207/200)
  (#x44 . 228/200)
  (#x48 . 250/200)
  (#x4C . 271/200)
  (#x54 . 293/200))


% Language tables
ekmLanguages = #'(

;; Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(nederlands . (
  0
  (("ees" . "es")
   ("aes" . "as"))
  (#x12 ih)
  (#x13 eh)
  (#x16 ihh)
  (#x17 ehh)
  (#x1E ihhh)
  (#x1F ehhh)
  (#x1A isehhh)
  (#x1B esihhh)
  (#x20 isehh)
  (#x21 esihh)
  (#x24 iseh)
  (#x25 esih)
  (#x28 is)
  (#x29 es)
  (#x2E isih)
  (#x2F eseh)
  (#x32 isihh)
  (#x33 esehh)
  (#x3A isihhh)
  (#x3B esehhh)
  (#x36 isisehhh)
  (#x37 esesihhh)
  (#x3C isisehh)
  (#x3D esesihh)
  (#x40 isiseh)
  (#x41 esesih)
  (#x44 isis)
  (#x45 eses)
  (#x48 isisih)
  (#x49 eseseh)
  (#x4C isisihh)
  (#x4D esesehh)
  (#x54 isisihhh)
  (#x55 esesehhh)))
)


% Notation tables
ekmNotations = #'(

;; Sagittal notation
(sag . (
  (#x00 #xE261)
  (#x12 #xE302)
  (#x13 #xE303)
  (#x16 #xE306)
  (#x17 #xE307)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x1E #xE30E)
  (#x1F #xE30F)
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
  (#x36 #xE326)
  (#x37 #xE327)
  (#x3A #xE32A)
  (#x3B #xE32B)
  (#x3C #xE32C)
  (#x3D #xE32D)
  (#x40 #xE330)
  (#x41 #xE331)
  (#x44 #xE334)
  (#x45 #xE335)
  (#x48 #xE302 #xE334)
  (#x49 #xE303 #xE335)
  (#x4C #xE306 #xE334)
  (#x4D #xE307 #xE335)
  (#x54 #xE30E #xE334)
  (#x55 #xE30F #xE335)))

;; Mixed Sagittal notation
(msag . (
  (#x00 #xE261)
  (#x12 #xE302)
  (#x13 #xE303)
  (#x16 #xE306)
  (#x17 #xE307)
  (#x1A #xE30F #xE262)
  (#x1B #xE30E #xE260)
  (#x1E #xE30E)
  (#x1F #xE30F)
  (#x20 #xE307 #xE262)
  (#x21 #xE306 #xE260)
  (#x24 #xE303 #xE262)
  (#x25 #xE302 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE302 #xE262)
  (#x2F #xE303 #xE260)
  (#x32 #xE306 #xE262)
  (#x33 #xE307 #xE260)
  (#x36 #xE30F #xE263)
  (#x37 #xE30E #xE264)
  (#x3A #xE30E #xE262)
  (#x3B #xE30F #xE260)
  (#x3C #xE307 #xE263)
  (#x3D #xE306 #xE264)
  (#x40 #xE303 #xE263)
  (#x41 #xE302 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE302 #xE263)
  (#x49 #xE303 #xE264)
  (#x4C #xE306 #xE263)
  (#x4D #xE307 #xE264)
  (#x54 #xE30E #xE263)
  (#x55 #xE30F #xE264)))

;; Extended Helmholtz-Ellis notation
(he . (
  (#x00 #xE261)
  (#x12 #xE2C7)
  (#x13 #xE2C2)
  (#x16 #xE2D1)
  (#x17 #xE2CC)
  (#x1E #xE2DB)
  (#x1F #xE2D6)
  (#x1A #xE2D7)
  (#x1B #xE2DA)
  (#x20 #xE2CD)
  (#x21 #xE2D0)
  (#x24 #xE2C3)
  (#x25 #xE2C6)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE2C8)
  (#x2F #xE2C1)
  (#x32 #xE2D2)
  (#x33 #xE2CB)
  (#x3A #xE2DC)
  (#x3B #xE2D5)
  (#x36 #xE2D8)
  (#x37 #xE2D9)
  (#x3C #xE2CE)
  (#x3D #xE2CF)
  (#x40 #xE2C4)
  (#x41 #xE2C5)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE2C9)
  (#x49 #xE2C0)
  (#x4C #xE2D3)
  (#x4D #xE2CA)
  (#x54 #xE2DD)
  (#x55 #xE2D4)))
)


% Text align table
ekmTextAlign = #'(
  ; he
  (#xE2C3 . 0)
  (#xE2C8 . 0)
  (#xE2CD . 0)
  (#xE2D2 . 0)
  (#xE2D7 . 0)
  (#xE2DC . 0)
  (#xE2C4 . 0)
  (#xE2C9 . 0)
  (#xE2CE . 0)
  (#xE2D3 . 0)
  (#xE2D8 . 0)
  (#xE2DD . 0)
  (#xE2C2 . 0)
  (#xE2C7 . 0)
  (#xE2CC . 0)
  (#xE2D1 . 0)
  (#xE2D6 . 0)
  (#xE2DB . 0)
)


\include "ekmel-main.ily"
