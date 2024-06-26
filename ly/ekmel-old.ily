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
%% File: ekmel-old.ily  -  Include file for old 72-EDO tuning
%% Latest revision: 2024-06-26
%%
%% Note names from the semitone and quarter-tone names in LilyPond 2.22
%% Copyright (C) 2010--2020 Valentin Villenave <valentin@villenave.net> et al.
%%
%% Extended with names for
%% - enharmonically equivalent quarter-tones
%% - five-quarters-tones
%% - sixth-tones
%% - twelfth-tones
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (#x12 . 1/12)
  (#x14 . 1/6)
  (#x1A . 1/4)
  (#x22 . 1/3)
  (#x24 . 5/12)
  (#x28 . 1/2)
  (#x2E . 7/12)
  (#x30 . 2/3)
  (#x36 . 3/4)
  (#x3E . 5/6)
  (#x40 . 11/12)
  (#x44 . 1)
  (#x48 . 13/12)
  (#x4A . 7/6)
  (#x50 . 5/4))


% Language tables
ekmLanguages = #'(

;; German names by Roland Meier <meier@informatik.th-darmstadt.de>,
;; Bjoern Jacke <bjoern.jacke@gmx.de>
;; incl. Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>
;; for ees* = es*, aes* = as*, b* = h*
(deutsch . (
  1
  (("ees" . "es")
   ("aes" . "as")
   ("h" . "b"))
  (#x12 ir)
  (#x13 er)
  (#x14 il)
  (#x15 el)
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh)
  (#x21b esih)
  (#x22 isel)
  (#x23 esil)
  (#x24 iser)
  (#x25 esir)
  (#x28 is)
  (#x29 es)
  (#x2e isir)
  (#x2f eser)
  (#x30 isil)
  (#x31 esel)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#x3e isisel)
  (#x3f esesil)
  (#x40 isiser)
  (#x41 esesir)
  (#x44 isis)
  (#x45 eses)
  (#x48 isisir)
  (#x49 eseser)
  (#x4a isisil)
  (#x4b esesel)
  (#x50 isisih)
  (#x51 eseseh)))
)


% Notation tables
ekmNotations = #'(

;; Arrow notation
(arrow .(
  (#x00 #xE261)
  (#x12 #xE47B)
  (#x13 #xE47C)
  (#x14 #xE2A4)
  (#x15 #xE2A1)
  (#x1A #xE27A)
  (#x1B #xE27B)
  (#x21A #xE27B #xE262)
  (#x21B #xE27A #xE260)
  (#x22 #xE2A1 #xE262)
  (#x23 #xE2A4 #xE260)
  (#x24 #xE47C #xE262)
  (#x25 #xE47B #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE47B #xE262)
  (#x2F #xE47C #xE260)
  (#x30 #xE2A4 #xE262)
  (#x31 #xE2A1 #xE260)
  (#x36 #xE27A #xE262)
  (#x37 #xE27B #xE260)
  (#x236 #xE27B #xE263)
  (#x237 #xE27A #xE264)
  (#x3E #xE2A1 #xE263)
  (#x3F #xE2A4 #xE264)
  (#x40 #xE47C #xE263)
  (#x41 #xE47B #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE47B #xE263)
  (#x49 #xE47C #xE264)
  (#x4A #xE2A4 #xE263)
  (#x4B #xE2A1 #xE264)
  (#x50 #xE27A #xE263)
  (#x51 #xE27B #xE264)))
)


\include "ekmel-main.ily"
