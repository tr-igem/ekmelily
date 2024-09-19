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
%% File: ekmel-96.ily  -  Include file for 96-EDO tuning
%% Latest revision: 2024-09-19
%%
%% Note names from the semitone and quarter-tone names in LilyPond 2.22
%% Copyright (C) 2010--2020 Valentin Villenave <valentin@villenave.net> et al.
%%
%% Extended with names for
%% - enharmonically equivalent quarter-tones
%% - eighth-tones
%% - sixteenth-tones
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (#x12 . 1/16)
  (#xA0 . 1/8)
  (#x64 . 3/16)
  (#x1A . 1/4)
  (#x6E . 5/16)
  (#xAA . 3/8)
  (#x24 . 7/16)
  (#x28 . 1/2)
  (#x2E . 9/16)
  (#xAC . 5/8)
  (#x78 . 11/16)
  (#x36 . 3/4)
  (#x82 . 13/16)
  (#xB6 . 7/8)
  (#x40 . 15/16)
  (#x44 . 1))


% Language tables
ekmLanguages = #'(

;; Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>,
;; Mark Knoop (microtonal.ily)
;; https://lists.gnu.org/archive/html/lilypond-user/2015-07/msg00485.html
(nederlands . (
  0
  (("ees" . "es")
   ("aes" . "as"))
  (#x12 i)
  (#x13 e)
  (#xA0 iq)
  (#xA1 eq)
  (#x64 ihe)
  (#x65 ehi)
  (#x1A ih)
  (#x1B eh)
  (#x21A iseh)
  (#x21B esih)
  (#x6E ihi)
  (#x6F ehe)
  (#xAA iseq)
  (#xAB esiq)
  (#x24 ise)
  (#x25 esi)
  (#x28 is)
  (#x29 es)
  (#x2E isi)
  (#x2F ese)
  (#xAC isiq)
  (#xAD eseq)
  (#x78 isihe)
  (#x79 esehi)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#x82 isihi)
  (#x83 esehe)
  (#xB6 isqq)
  (#xB7 esqq)
  (#x40 isise)
  (#x41 esesi)
  (#x44 isis)
  (#x45 eses)))

;; English names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(english . (
  0
  ()
  (#x0 #f -natural)
  (#x12 xs)
  (#x13 xf)
  (#xA0 es)
  (#xA1 ef)
  (#x64 txs)
  (#x65 txf)
  (#x1A qs)
  (#x1B qf)
  (#x21A saqf)
  (#x21B faqs)
  (#x6E fxs)
  (#x6F fxf)
  (#xAA tes)
  (#xAB tef)
  (#x24 sxs)
  (#x25 sxf)
  (#x28 s -sharp)
  (#x29 f -flat)
  (#x2E nxs)
  (#x2F nxf)
  (#xAC fes)
  (#xAD fef)
  (#x78 elxs)
  (#x79 elxf)
  (#x36 tqs)
  (#x37 tqf)
  (#x236 ssaqf)
  (#x237 ffaqs)
  (#x82 ttxs)
  (#x83 ttxf)
  (#xB6 ses)
  (#xB7 sef)
  (#x40 ftxs)
  (#x41 ftxf)
  (#x44 ss x -sharpsharp)
  (#x45 ff -flatflat)))

;; German names by Roland Meier <meier@informatik.th-darmstadt.de>,
;; Bjoern Jacke <bjoern.jacke@gmx.de>
(deutsch . (
  1
  (("ee" . "e") "ee"
   ("aesese" . "asasa") ("aese" . "asa") ("ae" . "a") "ae"
   (hes . b))
  (#x12 ir)
  (#x13 er)
  (#xA0 il)
  (#xA1 el)
  (#x64 iher)
  (#x65 ehir)
  (#x1A ih)
  (#x1B eh)
  (#x21A iseh)
  (#x21B esih)
  (#x6E ihir)
  (#x6F eher)
  (#xAA isel)
  (#xAB esil)
  (#x24 iser)
  (#x25 esir)
  (#x28 is)
  (#x29 es)
  (#x2E isir)
  (#x2F eser)
  (#xAC isil)
  (#xAD esel)
  (#x78 isiher)
  (#x79 esehir)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#x82 isihir)
  (#x83 eseher)
  (#xB6 isisel)
  (#xB7 esesil)
  (#x40 isiser)
  (#x41 esesir)
  (#x44 isis)
  (#x45 eses)))
)


% Notation tables
ekmNotations = #'(

;; Sagittal notation
(sag . (
  (#x00 #xE261)
  (#x12 #xE302)
  (#x13 #xE303)
  (#xA0 #xE370)
  (#xA1 #xE371)
  (#x64 #xE344)
  (#x65 #xE345)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x6E #xE34E)
  (#x6F #xE34F)
  (#xAA #xE37A)
  (#xAB #xE37B)
  (#x24 #xE314)
  (#x25 #xE315)
  (#x28 #xE318)
  (#x29 #xE319)
  (#x2E #xE31E)
  (#x2F #xE31F)
  (#xAC #xE37C)
  (#xAD #xE37D)
  (#x78 #xE358)
  (#x79 #xE359)
  (#x36 #xE326)
  (#x37 #xE327)
  (#x82 #xE362)
  (#x83 #xE363)
  (#xB6 #xE386)
  (#xB7 #xE387)
  (#x40 #xE330)
  (#x41 #xE331)
  (#x44 #xE334)
  (#x45 #xE335)))

;; Mixed Sagittal notation
(msag . (
  (#x00 #xE261)
  (#x12 #xE302)
  (#x13 #xE303)
  (#xA0 #xE370)
  (#xA1 #xE371)
  (#x64 #xE344)
  (#x65 #xE345)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x21A #xE30B #xE262)
  (#x21B #xE30A #xE260)
  (#x6E #xE345 #xE262)
  (#x6F #xE344 #xE260)
  (#xAA #xE371 #xE262)
  (#xAB #xE370 #xE260)
  (#x24 #xE303 #xE262)
  (#x25 #xE302 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xE302 #xE262)
  (#x2F #xE303 #xE260)
  (#xAC #xE370 #xE262)
  (#xAD #xE371 #xE260)
  (#x78 #xE344 #xE262)
  (#x79 #xE345 #xE260)
  (#x36 #xE30A #xE262)
  (#x37 #xE30B #xE260)
  (#x236 #xE30B #xE47D)
  (#x237 #xE30A #xE264)
  (#x82 #xE345 #xE47D)
  (#x83 #xE344 #xE264)
  (#xB6 #xE371 #xE47D)
  (#xB7 #xE370 #xE264)
  (#x40 #xE303 #xE47D)
  (#x41 #xE302 #xE264)
  (#x44 #xE47D)
  (#x45 #xE264)))

;; Persian notation
(persian . (
  (#x00 #xE261)
  (#x12 #xF778)
  (#x13 #xF777)
  (#xA0 #xF779)
  (#xA1 #xF776)
  (#x64 #xF77A)
  (#x65 #xF775)
  (#x1A #xF676)
  (#x1B #xE460)
  (#x21A #xE460 #xE262)
  (#x21B #xF676 #xE260)
  (#x6E #xF77B)
  (#x6F #xF774)
  (#xAA #xF77C)
  (#xAB #xF773)
  (#x24 #xF77D)
  (#x25 #xF772)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x2E #xF77E)
  (#x2F #xF771)
  (#xAC #xF77F)
  (#xAD #xF770)
  (#x78 #xF780)
  (#x79 #xF76F)
  (#x36 #xF677)
  (#x37 #xF674)
  (#x236 #xE460 #xE263)
  (#x237 #xF676 #xE264)
  (#x82 #xF781)
  (#x83 #xF76E)
  (#xB6 #xF782)
  (#xB7 #xF76D)
  (#x40 #xF783)
  (#x41 #xF76C)
  (#x44 #xE263)
  (#x45 #xE264)))

;; OpenMusic notation
(om . (
  (#x00 #xE261)
  (#x12 #xF758)
  (#x13)
  (#xA0 #xE272)
  (#xA1)
  (#x64 #xF759)
  (#x65)
  (#x1A #xE282)
  (#x1B)
  (#x6E #xF75A)
  (#x6F)
  (#xAA #xE299)
  (#xAB)
  (#x24 #xF75B)
  (#x25)
  (#x28 #xE262)
  (#x29)
  (#x2E #xF75C)
  (#x2F)
  (#xAC #xE274)
  (#xAD)
  (#x78 #xF75D)
  (#x79)
  (#x36 #xE283)
  (#x37)
  (#x82 #xF75E)
  (#x83)
  (#xB6 #xE29B)
  (#xB7)
  (#x40 #xF75F)
  (#x41)
  (#x44)
  (#x45)))
)


\include "ekmel-main.ily"
