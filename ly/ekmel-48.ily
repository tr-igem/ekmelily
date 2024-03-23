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
%% File: ekmel-48.ily  -  Include file for 48-EDO tuning
%% Latest revision: 2024-03-10
%%
%% Note names from the semitone and quarter-tone names in LilyPond 2.22
%% Copyright (C) 2010--2020 Valentin Villenave <valentin@villenave.net> et al.
%%
%% Extended with names for
%% - enharmonically equivalent quarter-tones
%% - eighth-tones
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (#xA0 . 1/8)
  (#x1A . 1/4)
  (#xAA . 3/8)
  (#x28 . 1/2)
  (#xAC . 5/8)
  (#x36 . 3/4)
  (#xB6 . 7/8)
  (#x44 . 1)
  (#xB8 . 9/8)
  (#x50 . 5/4))


% Language tables
ekmLanguages = #'(

;; Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>,
;; Mark Knoop (microtonal.ily)
;; https://lists.gnu.org/archive/html/lilypond-user/2015-07/msg00485.html
(nederlands . (
  0
  (("ees" . "es")
   ("aes" . "as"))
  (#xa0 iq)
  (#xa1 eq)
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh)
  (#x21b esih)
  (#xaa iseq)
  (#xab esiq)
  (#x28 is)
  (#x29 es)
  (#xac isiq)
  (#xad eseq)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#xb6 isqq)
  (#xb7 esqq)
  (#x44 isis)
  (#x45 eses)
  (#xb8 isisiq)
  (#xb9 eseseq)
  (#x50 isisih)
  (#x51 eseseh)))

;; English names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(english . (
  0
  ()
  (#x0 #f -natural)
  (#xa0 es)
  (#xa1 ef)
  (#x1a qs)
  (#x1b qf)
  (#x21a saqf)
  (#x21b faqs)
  (#xaa tes)
  (#xab tef)
  (#x28 s -sharp)
  (#x29 f -flat)
  (#xac fes)
  (#xad fef)
  (#x36 tqs)
  (#x37 tqf)
  (#x236 ssaqf)
  (#x237 ffaqs)
  (#xb6 ses)
  (#xb7 sef)
  (#x44 ss x -sharpsharp)
  (#x45 ff -flatflat)
  (#xb8 nes)
  (#xb9 nef)
  (#x50 fqs)
  (#x51 fqf)))

;; German names by Roland Meier <meier@informatik.th-darmstadt.de>,
;; Bjoern Jacke <bjoern.jacke@gmx.de>
(deutsch . (
  1
  (("ee" . "e") "ee"
   ("aesese" . "asasa") ("aese" . "asa") ("ae" . "a") "ae"
   (hes . b))
  (#xa0 il)
  (#xa1 el)
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh)
  (#x21b esih)
  (#xaa isel)
  (#xab esil)
  (#x28 is)
  (#x29 es)
  (#xac isil)
  (#xad esel)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#xb6 isisel)
  (#xb7 esesil)
  (#x44 isis)
  (#x45 eses)
  (#xb8 isisil)
  (#xb9 esesel)
  (#x50 isisih)
  (#x51 eseseh)))
)


% Notation tables
ekmNotations = #'(

;; Sagittal notation
(sag . (
  (#x00 #xE261)
  (#xA0 #xE370)
  (#xA1 #xE371)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#xAA #xE37A)
  (#xAB #xE37B)
  (#x28 #xE318)
  (#x29 #xE319)
  (#xAC #xE37C)
  (#xAD #xE37D)
  (#x36 #xE326)
  (#x37 #xE327)
  (#xB6 #xE386)
  (#xB7 #xE387)
  (#x44 #xE334)
  (#x45 #xE335)
  (#xB8 #xE370 #xE334)
  (#xB9 #xE371 #xE335)
  (#x50 #xE30A #xE334)
  (#x51 #xE30B #xE335)))

;; Mixed Sagittal notation
(msag . (
  (#x00 #xE261)
  (#xA0 #xE370)
  (#xA1 #xE371)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x21A #xE30B #xE262)
  (#x21B #xE30A #xE260)
  (#xAA #xE371 #xE262)
  (#xAB #xE370 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#xAC #xE370 #xE262)
  (#xAD #xE371 #xE260)
  (#x36 #xE30A #xE262)
  (#x37 #xE30B #xE260)
  (#x236 #xE30B #xE47D)
  (#x237 #xE30A #xE264)
  (#xB6 #xE371 #xE47D)
  (#xB7 #xE370 #xE264)
  (#x44 #xE47D)
  (#x45 #xE264)
  (#xB8 #xE370 #xE47D)
  (#xB9 #xE371 #xE264)
  (#x50 #xE30A #xE47D)
  (#x51 #xE30B #xE264)))

;; Gould / Stein / Zimmermann notation
(gostz . (
  (#x00 #xE261)
  (#xa0 #xE272)
  (#xa1 #xE273)
  (#x1a #xE282)
  (#x1b #xE280)
  (#x21a #xE280 #xE262)
  (#x21b #xE282 #xE260)
  (#xaa #xE275)
  (#xab #xE270)
  (#x28 #xE262)
  (#x29 #xE260)
  (#xac #xE274)
  (#xad #xE271)
  (#x36 #xE283)
  (#x37 #xE281)
  (#x236 #xE280 #xE263)
  (#x237 #xE282 #xE264)
  (#xb6 #xE277)
  (#xb7 #xE278)
  (#x44 #xE263)
  (#x45 #xE264)
  (#xb8 #xE276)
  (#xb9 #xE279)
  (#x50 #xE282 #xE263)
  (#x51 #xE280 #xE264)))
)


\include "ekmel-main.ily"
