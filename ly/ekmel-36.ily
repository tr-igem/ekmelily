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
%% File: ekmel-36.ily  -  Include file for 36-EDO tuning
%% Latest revision: 2024-03-08
%%
%% Note names from the semitone and quarter-tone names in LilyPond 2.22
%% Copyright (C) 2010--2020 Valentin Villenave <valentin@villenave.net> et al.
%%
%% Extended with names for sixth-tones
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (#x14 . 1/6)
  (#x22 . 1/3)
  (#x28 . 1/2)
  (#x30 . 2/3)
  (#x3E . 5/6)
  (#x44 . 1)
  (#x4A . 7/6))


% Language tables
ekmLanguages = #'(

;; Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(nederlands . (
  0
  (("ees" . "es")
   ("aes" . "as"))
  (#x14 il)
  (#x15 el)
  (#x22 isel)
  (#x23 esil)
  (#x28 is)
  (#x29 es)
  (#x30 isil)
  (#x31 esel)
  (#x3e isisel)
  (#x3f esesil)
  (#x44 isis)
  (#x45 eses)
  (#x4a isisil)
  (#x4b esesel)))

;; English names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(english . (
  0
  ()
  (#x0 #f -natural)
  (#x14 xs)
  (#x15 xf)
  (#x22 rs)
  (#x23 rf)
  (#x28 s -sharp)
  (#x29 f -flat)
  (#x30 trs)
  (#x31 trf)
  (#x3e fxs)
  (#x3f fxf)
  (#x44 ss x -sharpsharp)
  (#x45 ff -flatflat)
  (#x4a sxs)
  (#x4b sxf)))

;; German names by Roland Meier <meier@informatik.th-darmstadt.de>,
;; Bjoern Jacke <bjoern.jacke@gmx.de>
(deutsch . (
  1
  (("ee" . "e") "ee"
   ("aesese" . "asasa") ("aese" . "asa") ("ae" . "a") "ae"
   (hes . b))
  (#x14 il)
  (#x15 el)
  (#x22 isel)
  (#x23 esil)
  (#x28 is)
  (#x29 es)
  (#x30 isil)
  (#x31 esel)
  (#x3e isisel)
  (#x3f esesil)
  (#x44 isis)
  (#x45 eses)
  (#x4a isisil)
  (#x4b esesel)))

;; Norwegian names by Arvid Grøtting <arvidg@ifi.uio.no>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(norsk . (
  1
  (("ees" . "es")
   ("aes" . "as")
   ("hess" . "b") ("heses" . "bes") "hes")
  (#x14 il)
  (#x15 el)
  (#x22 isel issel)
  (#x23 esil essil)
  (#x28 is iss)
  (#x29 es ess)
  (#x30 isil issil)
  (#x31 esel essel)
  (#x3e isisel ississel)
  (#x3f esesil essessil)
  (#x44 isis ississ)
  (#x45 eses essess)
  (#x4a isisil ississil)
  (#x4b esesel essessel)))

;; Finnish names by Heikki Junes <heikki.junes@hut.fi>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(suomi . (
  1
  (("ees" . "es") "ees"
   ("aesese" . "asasa") ("aese" . "asa") ("aes" . "as") "aes"
   ("hes" . "b"))
  (#x14 il)
  (#x15 el)
  (#x22 isel)
  (#x23 esil)
  (#x28 is)
  (#x29 es)
  (#x30 isil)
  (#x31 esel)
  (#x3e isisel)
  (#x3f esesil)
  (#x44 isis)
  (#x45 eses)
  (#x4a isisil)
  (#x4b esesel)))

;; Swedish names by Mats Bengtsson <mabe@violin.s3.kth.se>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(svenska . (
  1
  (("ees" . "es") "ees"
   ("aes" . "as") "aes"
   (hess . b) hess)
  (#x14 il)
  (#x15 el)
  (#x22 issel)
  (#x23 essil)
  (#x28 iss)
  (#x29 ess)
  (#x30 issil)
  (#x31 essel)
  (#x3e ississel)
  (#x3f essessil)
  (#x44 ississ)
  (#x45 essess)
  (#x4a ississil)
  (#x4b essessel)))
)


% Notation tables
ekmNotations = #'(

;; Gould notation
(go . (
  (#x00 #xE261)
  (#x14 #xE272)
  (#x15 #xE273)
  (#x22 #xE275)
  (#x23 #xE270)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x30 #xE274)
  (#x31 #xE271)
  (#x3E #xE277)
  (#x3F #xE278)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x4A #xE276)
  (#x4B #xE279)))

;; Arrow notation
(arrow . (
  (#x00 #xE261)
  (#x14 #xE2A4)
  (#x15 #xE2A1)
  (#x22 #xE2A1 #xE262)
  (#x23 #xE2A4 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x30 #xE2A4 #xE262)
  (#x31 #xE2A1 #xE260)
  (#x3E #xE2A1 #xE263)
  (#x3F #xE2A4 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x4A #xE2A4 #xE263)
  (#x4B #xE2A1 #xE264)))

;; Sagittal notation
(sag . (
  (#x00 #xE261)
  (#x14 #xE304)
  (#x15 #xE305)
  (#x22 #xE312)
  (#x23 #xE313)
  (#x28 #xE318)
  (#x29 #xE319)
  (#x30 #xE320)
  (#x31 #xE321)
  (#x3E #xE32E)
  (#x3F #xE32F)
  (#x44 #xE334)
  (#x45 #xE335)
  (#x4A #xE304 #xE334)
  (#x4B #xE305 #xE335)))

;; Mixed Sagittal notation
(msag . (
  (#x00 #xE261)
  (#x14 #xE304)
  (#x15 #xE305)
  (#x22 #xE305 #xE262)
  (#x23 #xE304 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x30 #xE304 #xE262)
  (#x31 #xE305 #xE260)
  (#x3E #xE305 #xE47D)
  (#x3F #xE304 #xE264)
  (#x44 #xE47D)
  (#x45 #xE264)
  (#x4A #xE304 #xE47D)
  (#x4B #xE305 #xE264)))

;; Wyschnegradsky notation
(wys . (
  (#x00 #xE261)
  (#x14 #xE421)
  (#x15 #xE42C)
  (#x22 #xE423)
  (#x23 #xE42E)
  (#x28 #xE425)
  (#x29 #xE430)
  (#x30 #xE427)
  (#x31 #xE432)
  (#x3E #xE429)
  (#x3F #xE434)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x4A #xE421 #xE263)
  (#x4B #xE42C #xE264)))

;; Bosanquet commatic notation
(bos . (
  (#x00 #xE261)
  (#x14 #xE479 #xE479)
  (#x15 #xE47A #xE47A)
  (#x22 #xE47A #xE47A #xE262)
  (#x23 #xE479 #xE479 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x30 #xE479 #xE479 #xE262)
  (#x31 #xE47A #xE47A #xE260)
  (#x3E #xE47A #xE47A #xE263)
  (#x3F #xE479 #xE479 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x4A #xE479 #xE479 #xE263)
  (#x4B #xE47A #xE47A #xE264)))

;; Hába notation
(haba . (
  (#x00 #xE261)
  (#x14 #xF661)
  (#x15 #xF66C)
  (#x22 #xF663)
  (#x23 #xF66E)
  (#x28 #xF665)
  (#x29 #xF670)
  (#x30 #xF667)
  (#x31 #xF66C #xF670)
  (#x3E #xF669)
  (#x3F #xF661 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x4A #xF661 #xE263)
  (#x4B #xF66C #xE264)))
)


\include "ekmel-main.ily"
