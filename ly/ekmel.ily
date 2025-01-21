﻿%% This file is part of Ekmelily - Notation of microtonal music with LilyPond.
%% Copyright (C) 2013-2025  Thomas Richter <thomas-richter@aon.at>
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
%% File: ekmel.ily  -  Include file for 72-EDO tuning
%%
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

;; Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(nederlands . (
  0
  (("ees" . "es")
   ("aes" . "as"))
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

;; English names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(english . (
  0
  ()
  (#x0 #f -natural)
  (#x12 ts)
  (#x13 tf)
  (#x14 xs)
  (#x15 xf)
  (#x1a qs)
  (#x1b qf)
  (#x21a saqf)
  (#x21b faqs)
  (#x22 rs)
  (#x23 rf)
  (#x24 fts)
  (#x25 ftf)
  (#x28 s -sharp)
  (#x29 f -flat)
  (#x2e sts)
  (#x2f stf)
  (#x30 trs)
  (#x31 trf)
  (#x36 tqs)
  (#x37 tqf)
  (#x236 ssaqf)
  (#x237 ffaqs)
  (#x3e fxs)
  (#x3f fxf)
  (#x40 ets)
  (#x41 etf)
  (#x44 ss x -sharpsharp)
  (#x45 ff -flatflat)
  (#x48 tts)
  (#x49 ttf)
  (#x4a sxs)
  (#x4b sxf)
  (#x50 fqs)
  (#x51 fqf)))

;; German names by Roland Meier <meier@informatik.th-darmstadt.de>,
;; Bjoern Jacke <bjoern.jacke@gmx.de>
(deutsch . (
  1
  (("ee" . "e") "ee"
   ("aesese" . "asasa") ("aese" . "asa") ("ae" . "a") "ae"
   (hes . b))
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

;; Norwegian names by Arvid Grøtting <arvidg@ifi.uio.no>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(norsk . (
  1
  (("ees" . "es")
   ("aes" . "as")
   ("hess" . "b") ("heses" . "bes") "hes")
  (#x12 ir)
  (#x13 er)
  (#x14 il)
  (#x15 el)
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh isseh)
  (#x21b esih essih)
  (#x22 isel issel)
  (#x23 esil essil)
  (#x24 iser isser)
  (#x25 esir essir)
  (#x28 is iss)
  (#x29 es ess)
  (#x2e isir issir)
  (#x2f eser esser)
  (#x30 isil issil)
  (#x31 esel essel)
  (#x36 isih issih)
  (#x37 eseh esseh)
  (#x236 isiseh ississeh)
  (#x237 esesih essessih)
  (#x3e isisel ississel)
  (#x3f esesil essessil)
  (#x40 isiser ississer)
  (#x41 esesir essessir)
  (#x44 isis ississ)
  (#x45 eses essess)
  (#x48 isisir ississir)
  (#x49 eseser essesser)
  (#x4a isisil ississil)
  (#x4b esesel essessel)
  (#x50 isisih ississih)
  (#x51 eseseh essesseh)))

;; Finnish names by Heikki Junes <heikki.junes@hut.fi>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(suomi . (
  1
  (("ees" . "es") "ees"
   ("aesese" . "asasa") ("aese" . "asa") ("aes" . "as") "aes"
   ("hes" . "b"))
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

;; Swedish names by Mats Bengtsson <mabe@violin.s3.kth.se>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(svenska . (
  1
  (("ees" . "es") "ees"
   ("aes" . "as") "aes"
   (hess . b) hess)
  (#x12 ir)
  (#x13 er)
  (#x14 il)
  (#x15 el)
  (#x1a ih)
  (#x1b eh)
  (#x21a isseh)
  (#x21b essih)
  (#x22 issel)
  (#x23 essil)
  (#x24 isser)
  (#x25 essir)
  (#x28 iss)
  (#x29 ess)
  (#x2e issir)
  (#x2f esser)
  (#x30 issil)
  (#x31 essel)
  (#x36 issih)
  (#x37 esseh)
  (#x236 ississeh)
  (#x237 essessih)
  (#x3e ississel)
  (#x3f essessil)
  (#x40 ississer)
  (#x41 essessir)
  (#x44 ississ)
  (#x45 essess)
  (#x48 ississir)
  (#x49 essesser)
  (#x4a ississil)
  (#x4b essessel)
  (#x50 ississih)
  (#x51 essesseh)))
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

;; Richter Herf / Maedel notation
(rhm . (
  (#x00 #xE261)
  (#x12 #xF603)
  (#x13 #xF600)
  (#x14 #xF604)
  (#x15 #xF601)
  (#x1A #xF605)
  (#x1B #xF602)
  (#x21A #xF602 #xE262)
  (#x21B #xF605 #xE260)
  (#x22 #xF601 #xE262)
  (#x23 #xF604 #xE260)
  (#x24 #xF600 #xE262)
  (#x25 #xF603 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xF603 #xE262)
  (#x2F #xF600 #xE260)
  (#x30 #xF604 #xE262)
  (#x31 #xF601 #xE260)
  (#x36 #xF605 #xE262)
  (#x37 #xF602 #xE260)
  (#x236 #xF602 #xE263)
  (#x237 #xF605 #xE264)
  (#x3E #xF601 #xE263)
  (#x3F #xF604 #xE264)
  (#x40 #xF600 #xE263)
  (#x41 #xF603 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xF603 #xE263)
  (#x49 #xF600 #xE264)
  (#x4A #xF604 #xE263)
  (#x4B #xF601 #xE264)
  (#x50 #xF605 #xE263)
  (#x51 #xF602 #xE264)))

;; Sims notation
(sims . (
  (#x00 #xE261)
  (#x12 #xE2A3)
  (#x13 #xE2A0)
  (#x14 #xE2A4)
  (#x15 #xE2A1)
  (#x1A #xE2A5)
  (#x1B #xE2A2)
  (#x21A #xE2A2 #xE262)
  (#x21B #xE2A5 #xE260)
  (#x22 #xE2A1 #xE262)
  (#x23 #xE2A4 #xE260)
  (#x24 #xE2A0 #xE262)
  (#x25 #xE2A3 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE2A3 #xE262)
  (#x2F #xE2A0 #xE260)
  (#x30 #xE2A4 #xE262)
  (#x31 #xE2A1 #xE260)
  (#x36 #xE2A5 #xE262)
  (#x37 #xE2A2 #xE260)
  (#x236 #xE2A2 #xE263)
  (#x237 #xE2A5 #xE264)
  (#x3E #xE2A1 #xE263)
  (#x3F #xE2A4 #xE264)
  (#x40 #xE2A0 #xE263)
  (#x41 #xE2A3 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE2A3 #xE263)
  (#x49 #xE2A0 #xE264)
  (#x4A #xE2A4 #xE263)
  (#x4B #xE2A1 #xE264)
  (#x50 #xE2A5 #xE263)
  (#x51 #xE2A2 #xE264)))

;; Hesse notation
(hesse . (
  (#x00 #xE261)
  (#x12 #xF609)
  (#x13 #xF606)
  (#x14 #xF60A)
  (#x15 #xF607)
  (#x1A #xF60B)
  (#x1B #xF608)
  (#x21A #xF608 #xE262)
  (#x21B #xF60B #xE260)
  (#x22 #xF607 #xE262)
  (#x23 #xF60A #xE260)
  (#x24 #xF606 #xE262)
  (#x25 #xF609 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xF609 #xE262)
  (#x2F #xF606 #xE260)
  (#x30 #xF60A #xE262)
  (#x31 #xF607 #xE260)
  (#x36 #xF60B #xE262)
  (#x37 #xF608 #xE260)
  (#x236 #xF608 #xE263)
  (#x237 #xF60B #xE264)
  (#x3E #xF607 #xE263)
  (#x3F #xF60A #xE264)
  (#x40 #xF606 #xE263)
  (#x41 #xF609 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xF609 #xE263)
  (#x49 #xF606 #xE264)
  (#x4A #xF60A #xE263)
  (#x4B #xF607 #xE264)
  (#x50 #xF60B #xE263)
  (#x51 #xF608 #xE264)))

;; Sagittal notation
(sag . (
  (#x00 #xE261)
  (#x12 #xE302)
  (#x13 #xE303)
  (#x14 #xE304)
  (#x15 #xE305)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x22 #xE312)
  (#x23 #xE313)
  (#x24 #xE314)
  (#x25 #xE315)
  (#x28 #xE318)
  (#x29 #xE319)
  (#x2E #xE31E)
  (#x2F #xE31F)
  (#x30 #xE320)
  (#x31 #xE321)
  (#x36 #xE326)
  (#x37 #xE327)
  (#x3E #xE32E)
  (#x3F #xE32F)
  (#x40 #xE330)
  (#x41 #xE331)
  (#x44 #xE334)
  (#x45 #xE335)
  (#x48 #xE302 #xE334)
  (#x49 #xE303 #xE335)
  (#x4A #xE304 #xE334)
  (#x4B #xE305 #xE335)
  (#x50 #xE30A #xE334)
  (#x51 #xE30B #xE335)))

;; Mixed Sagittal notation
(msag . (
  (#x00 #xE261)
  (#x12 #xE302)
  (#x13 #xE303)
  (#x14 #xE304)
  (#x15 #xE305)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x21A #xE30B #xE262)
  (#x21B #xE30A #xE260)
  (#x22 #xE305 #xE262)
  (#x23 #xE304 #xE260)
  (#x24 #xE303 #xE262)
  (#x25 #xE302 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE302 #xE262)
  (#x2F #xE303 #xE260)
  (#x30 #xE304 #xE262)
  (#x31 #xE305 #xE260)
  (#x36 #xE30A #xE262)
  (#x37 #xE30B #xE260)
  (#x236 #xE30B #xE47D)
  (#x237 #xE30A #xE264)
  (#x3E #xE305 #xE47D)
  (#x3F #xE304 #xE264)
  (#x40 #xE303 #xE47D)
  (#x41 #xE302 #xE264)
  (#x44 #xE47D)
  (#x45 #xE264)
  (#x48 #xE302 #xE47D)
  (#x49 #xE303 #xE264)
  (#x4A #xE304 #xE47D)
  (#x4B #xE305 #xE264)
  (#x50 #xE30A #xE47D)
  (#x51 #xE30B #xE264)))

;; Wyschnegradsky notation
(wys . (
  (#x00 #xE261)
  (#x12 #xE420)
  (#x13 #xE42B)
  (#x14 #xE421)
  (#x15 #xE42C)
  (#x1A #xE422)
  (#x1B #xE42D)
  (#x22 #xE423)
  (#x23 #xE42E)
  (#x24 #xE424)
  (#x25 #xE42F)
  (#x28 #xE425)
  (#x29 #xE430)
  (#x2E #xE426)
  (#x2F #xE431)
  (#x30 #xE427)
  (#x31 #xE432)
  (#x36 #xE428)
  (#x37 #xE433)
  (#x3E #xE429)
  (#x3F #xE434)
  (#x40 #xE42A)
  (#x41 #xE435)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE420 #xE263)
  (#x49 #xE42B #xE264)
  (#x4A #xE421 #xE263)
  (#x4B #xE42C #xE264)
  (#x50 #xE422 #xE263)
  (#x51 #xE42D #xE264)))

;; Hába notation
(haba . (
  (#x00 #xE261)
  (#x12 #xF660)
  (#x13 #xF66B)
  (#x14 #xF661)
  (#x15 #xF66C)
  (#x1A #xF662)
  (#x1B #xF66D)
  (#x21A #xF662)
  (#x21B #xF66D)
  (#x22 #xF663)
  (#x23 #xF66E)
  (#x24 #xF664)
  (#x25 #xF66F)
  (#x28 #xF665)
  (#x29 #xF670)
  (#x2E #xF666)
  (#x2F #xF66B #xF670)
  (#x30 #xF667)
  (#x31 #xF66C #xF670)
  (#x36 #xF668)
  (#x37 #xF66D #xF670)
  (#x236 #xF66D #xE263)
  (#x237 #xF662 #xE264)
  (#x3E #xF669)
  (#x3F #xF661 #xE264)
  (#x40 #xF66A)
  (#x41 #xF660 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xF660 #xE263)
  (#x49 #xF66B #xE264)
  (#x4A #xF661 #xE263)
  (#x4B #xF66C #xE264)
  (#x50 #xF662 #xE263)
  (#x51 #xF66D #xE264)))

;; Gould / Stein / Zimmermann notation (36 + 24-EDO)
(gost . gostz)
(gostz . (
  (#x00 #xE261)
  (#x12 #xE47B)
  (#x13 #xE47C)
  (#x14 #xE272)
  (#x15 #xE273)
  (#x1A #xE282)
  (#x1B #xE284)
  (#x21A #xE284 #xE262)
  (#x21B #xE282 #xE260)
  (#x22 #xE275)
  (#x23 #xE270)
  (#x24 #xE47C #xE262)
  (#x25 #xE47B #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE47B #xE262)
  (#x2F #xE47C #xE260)
  (#x30 #xE274)
  (#x31 #xE271)
  (#x36 #xE283)
  (#x37 #xE285)
  (#x236 #xE284 #xE263)
  (#x237 #xE282 #xE264)
  (#x3E #xE277)
  (#x3F #xE278)
  (#x40 #xE47C #xE263)
  (#x41 #xE47B #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE47B #xE263)
  (#x49 #xE47C #xE264)
  (#x4A #xE276)
  (#x4B #xE279)
  (#x50 #xE282 #xE263)
  (#x51 #xE284 #xE264)))

;; Gould / Stein / Couper notation (36 + 24-EDO)
(gostc . (
  (#x00 #xE261)
  (#x12 #xE47B)
  (#x13 #xE47C)
  (#x14 #xE272)
  (#x15 #xE273)
  (#x1A #xE282)
  (#x1B #xE284)
  (#x21A #xE284 #xE262)
  (#x21B #xE282 #xE260)
  (#x22 #xE275)
  (#x23 #xE270)
  (#x24 #xE47C #xE262)
  (#x25 #xE47B #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE47B #xE262)
  (#x2F #xE47C #xE260)
  (#x30 #xE274)
  (#x31 #xE271)
  (#x36 #xE283)
  (#x37 #xE489)
  (#x236 #xE284 #xE263)
  (#x237 #xE282 #xE264)
  (#x3E #xE277)
  (#x3F #xE278)
  (#x40 #xE47C #xE263)
  (#x41 #xE47B #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE47B #xE263)
  (#x49 #xE47C #xE264)
  (#x4A #xE276)
  (#x4B #xE279)
  (#x50 #xE282 #xE263)
  (#x51 #xE284 #xE264)))

;; Bosanquet commatic notation
(bos . (
  (#x00 #xE261)
  (#x12 #xE479)
  (#x13 #xE47A)
  (#x14 #xE479 #xE479)
  (#x15 #xE47A #xE47A)
  (#x1A #xE479 #xE479 #xE479)
  (#x1B #xE47A #xE47A #xE47A)
  (#x21A #xE47A #xE47A #xE47A #xE262)
  (#x21B #xE479 #xE479 #xE479 #xE260)
  (#x22 #xE47A #xE47A #xE262)
  (#x23 #xE479 #xE479 #xE260)
  (#x24 #xE47A #xE262)
  (#x25 #xE479 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE479 #xE262)
  (#x2F #xE47A #xE260)
  (#x30 #xE479 #xE479 #xE262)
  (#x31 #xE47A #xE47A #xE260)
  (#x36 #xE479 #xE479 #xE479 #xE262)
  (#x37 #xE47A #xE47A #xE47A #xE260)
  (#x236 #xE47A #xE47A #xE47A #xE263)
  (#x237 #xE479 #xE479 #xE479 #xE264)
  (#x3E #xE47A #xE47A #xE263)
  (#x3F #xE479 #xE479 #xE264)
  (#x40 #xE47A #xE263)
  (#x41 #xE479 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE479 #xE263)
  (#x49 #xE47A #xE264)
  (#x4A #xE479 #xE479 #xE263)
  (#x4B #xE47A #xE47A #xE264)
  (#x50 #xE479 #xE479 #xE479 #xE263)
  (#x51 #xE47A #xE47A #xE47A #xE264)))

;; Ferneyhough notation (18 + 24-EDO)
(fern . (
  (#x00 #xE261)
  (#x12 #xE47B)
  (#x13 #xE47C)
  (#x14 #xE47C #xE48E)
  (#x15 #xE47B #xE48F)
  (#x1A #xE48E)
  (#x1B #xE48F)
  (#x21A #xE48F #xE262)
  (#x21B #xE48E #xE260)
  (#x22 #xE48A)
  (#x23 #xE48B)
  (#x24 #xE47C #xE262)
  (#x25 #xE47B #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE47B #xE262)
  (#x2F #xE47C #xE260)
  (#x30 #xE48C)
  (#x31 #xE48D)
  (#x36 #xE48E #xE262)
  (#x37 #xE48F #xE260)
  (#x236 #xE48F #xE263)
  (#x237 #xE48E #xE264)
  (#x3E #xE47B #xE48E #xE262)
  (#x3F #xE47C #xE48F #xE260)
  (#x40 #xE47C #xE263)
  (#x41 #xE47B #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x48 #xE47B #xE263)
  (#x49 #xE47C #xE264)
  (#x4A #xE47C #xE48E #xE263)
  (#x4B #xE47B #xE48F #xE264)
  (#x50 #xE48E #xE263)
  (#x51 #xE48F #xE264)))
)


% Text align table
ekmTextAlign = #'(
  ; wys
  (#xE422 . 0)
  (#xE423 . 0)
  (#xE424 . 0)
  (#xE425 . 0)
  (#xE426 . 0)
  (#xE427 . 0)
  (#xE428 . 0)
  (#xE429 . 0)
  (#xE42A . 0)
  (#xE42B . 0.5)
  (#xE42C . 0.5)
  (#xE42D . 0.5)
  (#xE431 . 0.5)
  (#xE432 . 0.5)
  (#xE433 . 0.5)
  ; go
  (#xE274 . 0)
  (#xE275 . 0)
  (#xE276 . 0)
  (#xE277 . 0)
  (#xE272 . 0)
  (#xE273 . 0)
  ; stz
  (#xE282 . 0)
  (#xE283 . 0)
)


\include "ekmel-main.ily"
