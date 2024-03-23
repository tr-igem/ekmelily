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
%% File: ekmel-24.ily  -  Include file for 24-EDO tuning
%% Latest revision: 2024-03-08
%%
%% Note names from the semitone and quarter-tone names in LilyPond 2.22
%% Copyright (C) 2010--2020 Valentin Villenave <valentin@villenave.net> et al.
%%
%% Extended with names for
%% - enharmonically equivalent quarter-tones
%% - five-quarters-tones
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

;; Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(nederlands . (
  0
  (("ees" . "es")
   ("aes" . "as"))
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh)
  (#x21b esih)
  (#x28 is)
  (#x29 es)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#x44 isis)
  (#x45 eses)
  (#x50 isisih)
  (#x51 eseseh)))

;; English names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(english . (
  0
  ()
  (#x0 #f -natural)
  (#x1a qs)
  (#x1b qf)
  (#x21a saqf)
  (#x21b faqs)
  (#x28 s -sharp)
  (#x29 f -flat)
  (#x36 tqs)
  (#x37 tqf)
  (#x236 ssaqf)
  (#x237 ffaqs)
  (#x44 ss x -sharpsharp)
  (#x45 ff -flatflat)
  (#x50 fqs)
  (#x51 fqf)))

;; German names by Roland Meier <meier@informatik.th-darmstadt.de>,
;; Bjoern Jacke <bjoern.jacke@gmx.de>
(deutsch . (
  1
  (("ee" . "e") "ee"
   ("aesese" . "asasa") ("aese" . "asa") ("ae" . "a") "ae"
   (hes . b))
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh)
  (#x21b esih)
  (#x28 is)
  (#x29 es)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#x44 isis)
  (#x45 eses)
  (#x50 isisih)
  (#x51 eseseh)))

;; Catalan names by Jaume Obrador <jobrador@ipc4.uib.es>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(catalan . català)
(català . (
  2
  ()
  (#x1a qd)
  (#x1b qb)
  (#x21a dqb)
  (#x21b bqd)
  (#x28 d)
  (#x29 b)
  (#x36 tqd)
  (#x37 tqb)
  (#x236 ddqb)
  (#x237 bbqd)
  (#x44 dd)
  (#x45 bb)
  (#x50 cqd)
  (#x51 cqb)))

;; Spanish names by Carlos García Suárez <cgscqmp@terra.es>,
;; Maximiliano G. G. <mxgdvg@yahoo.it>
(espanol . español)
(español . (
  2
  ()
  (#x1a cs)
  (#x1b cb)
  (#x21a scb)
  (#x21b bcs)
  (#x28 s)
  (#x29 b)
  (#x36 tcs)
  (#x37 tcb)
  (#x236 sscb xcb)
  (#x237 bbcs)
  (#x44 ss x)
  (#x45 bb)
  (#x50 ccs)
  (#x51 ccb)))

;; Italian names by Paolo Zuliani <zuliap@easynet.it>,
;; Eric Wurbel <wurbel@univ-tln.fr>
(italiano . (
  2
  ()
  (#x1a sd)
  (#x1b sb)
  (#x21a dsb)
  (#x21b bsd)
  (#x28 d)
  (#x29 b)
  (#x36 dsd)
  (#x37 bsb)
  (#x236 ddsb)
  (#x237 bbsd)
  (#x44 dd)
  (#x45 bb)
  (#x50 ddsd)
  (#x51 bbsb)))

;; French names by Valentin Villenave <valentin@villenave.net>
(français . (
  3
  (("ré" . "re"))
  (#x1a sd)
  (#x1b sb)
  (#x21a dsb)
  (#x21b bsd)
  (#x28 d)
  (#x29 b)
  (#x36 dsd)
  (#x37 bsb)
  (#x236 ddsb xsb)
  (#x237 bbsd)
  (#x44 dd x)
  (#x45 bb)
  (#x50 ddsd xsd)
  (#x51 bbsb)))

;; Portuguese names by Pedro Kröger <kroeger@pedrokroeger.net>
(portugues . português)
(português . (
  2
  ()
  (#x1a sqt)
  (#x1b bqt)
  (#x21a sbqt)
  (#x21b bsqt)
  (#x28 s)
  (#x29 b)
  (#x36 stqt)
  (#x37 btqt)
  (#x236 ssbqt)
  (#x237 bbsqt)
  (#x44 ss)
  (#x45 bb)
  (#x50 scqt)
  (#x51 bcqt)))

;; Norwegian names by Arvid Grøtting <arvidg@ifi.uio.no>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(norsk . (
  1
  (("ees" . "es")
   ("aes" . "as")
   ("hess" . "b") ("heses" . "bes") "hes")
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh isseh)
  (#x21b esih essih)
  (#x28 is iss)
  (#x29 es ess)
  (#x36 isih issih)
  (#x37 eseh esseh)
  (#x236 isiseh ississeh)
  (#x237 esesih essessih)
  (#x44 isis ississ)
  (#x45 eses essess)
  (#x50 isisih ississih)
  (#x51 eseseh essesseh)))

;; Finnish names by Heikki Junes <heikki.junes@hut.fi>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(suomi . (
  1
  (("ees" . "es") "ees"
   ("aesese" . "asasa") ("aese" . "asa") ("aes" . "as") "aes"
   ("hes" . "b"))
  (#x1a ih)
  (#x1b eh)
  (#x21a iseh)
  (#x21b esih)
  (#x28 is)
  (#x29 es)
  (#x36 isih)
  (#x37 eseh)
  (#x236 isiseh)
  (#x237 esesih)
  (#x44 isis)
  (#x45 eses)
  (#x50 isisih)
  (#x51 eseseh)))

;; Swedish names by Mats Bengtsson <mabe@violin.s3.kth.se>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(svenska . (
  1
  (("ees" . "es") "ees"
   ("aes" . "as") "aes"
   (hess . b) hess)
  (#x1a ih)
  (#x1b eh)
  (#x21a isseh)
  (#x21b essih)
  (#x28 iss)
  (#x29 ess)
  (#x36 issih)
  (#x37 esseh)
  (#x236 ississeh)
  (#x237 essessih)
  (#x44 ississ)
  (#x45 essess)
  (#x50 ississih)
  (#x51 essesseh)))

;; Vlaams names by Hendrik Maryns <hendrik.maryns@ugent.be>
;; Torsten Hämmerle <torsten.haemmerle@web.de>  quarter-tones added
(vlaams . (
  2
  ()
  (#x1a hk)
  (#x1b hb)
  (#x21a khb)
  (#x21b bhk)
  (#x28 k)
  (#x29 b)
  (#x36 khk)
  (#x37 bhb)
  (#x236 kkhb)
  (#x237 bbhk)
  (#x44 kk)
  (#x45 bb)
  (#x50 kkhk)
  (#x51 bbhb)))
)


% Notation tables
ekmNotations = #'(

;; Stein / Couper notation
(stc . (
  (#x00 #xE261)
  (#x1A #xE282)
  (#x1B #xE284)
  (#x21A #xE284 #xE262)
  (#x21B #xE282 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE283)
  (#x37 #xE489)
  (#x236 #xE284 #xE263)
  (#x237 #xE282 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE282 #xE263)
  (#x51 #xE284 #xE264)))

;; Stein / Zimmermann notation
(stz . (
  (#x00 #xE261)
  (#x1A #xE282)
  (#x1B #xE284)
  (#x21A #xE284 #xE262)
  (#x21B #xE282 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE283)
  (#x37 #xE285)
  (#x236 #xE284 #xE263)
  (#x237 #xE282 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE282 #xE263)
  (#x51 #xE284 #xE264)))

;; Gould notation
(go . (
  (#x00 #xE261)
  (#x1A #xE272)
  (#x1B #xE273)
  (#x21A #xE275)
  (#x21B #xE270)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE274)
  (#x37 #xE271)
  (#x236 #xE277)
  (#x237 #xE278)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE276)
  (#x51 #xE279)))

;; Stein / Van Blankenburg / Tartini notation
(stvt . (
  (#x00 #xE261)
  (#x1A #xE282)
  (#x1B #xE488)
  (#x21A #xE488 #xE262)
  (#x21B #xE282 #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE283)
  (#x37 #xE487)
  (#x236 #xE488 #xE263)
  (#x237 #xE282 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE282 #xE263)
  (#x51 #xE488 #xE264)))

;; Arrow notation
(arrow . (
  (#x00 #xE261)
  (#x1A #xE27A)
  (#x1B #xE27B)
  (#x21A #xE27B #xE262)
  (#x21B #xE27A #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE27A #xE262)
  (#x37 #xE27B #xE260)
  (#x236 #xE27B #xE263)
  (#x237 #xE27A #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE27A #xE263)
  (#x51 #xE27B #xE264)))

;; Sagittal notation
(sag . (
  (#x00 #xE261)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x28 #xE318)
  (#x29 #xE319)
  (#x36 #xE326)
  (#x37 #xE327)
  (#x44 #xE334)
  (#x45 #xE335)
  (#x50 #xE30A #xE334)
  (#x51 #xE30B #xE335)))

;; Mixed Sagittal notation
(msag . (
  (#x00 #xE261)
  (#x1A #xE30A)
  (#x1B #xE30B)
  (#x21A #xE30B #xE262)
  (#x21B #xE30A #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE30A #xE262)
  (#x37 #xE30B #xE260)
  (#x236 #xE30B #xE47D)
  (#x237 #xE30A #xE264)
  (#x44 #xE47D)
  (#x45 #xE264)
  (#x50 #xE30A #xE47D)
  (#x51 #xE30B #xE264)))

;; Arabic notation
(arabic . (
  (#x00 #xED34)
  (#x1A #xED35)
  (#x1B #xED33)
  (#x21A #xED33 #xED36)
  (#x21B #xED35 #xED32)
  (#x28 #xED36)
  (#x29 #xED32)
  (#x36 #xED37)
  (#x37 #xED31)
  (#x236 #xED33 #xED38)
  (#x237 #xED35 #xED30)
  (#x44 #xED38)
  (#x45 #xED30)
  (#x50 #xED35 #xED38)
  (#x51 #xED33 #xED30)))

;; Persian notation
(persian . (
  (#x00 #xE261)
  (#x1A #xE461)
  (#x1B #xE460)
  (#x21A #xE262 #xE460)
  (#x21B #xE260 #xE461)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE262 #xE461)
  (#x37 #xE260 #xE460)
  (#x236 #xE263 #xE460)
  (#x237 #xE264 #xE461)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE263 #xE461)
  (#x51 #xE264 #xE460)))

;; Digit 4 notation
(four . (
  (#x00 #xE261)
  (#x1A #xE47E)
  (#x1B #xE47F)
  (#x21A #xE47F #xE262)
  (#x21B #xE47E #xE260)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE47E #xE262)
  (#x37 #xE47F #xE260)
  (#x236 #xE47F #xE263)
  (#x237 #xE47E #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE47E #xE263)
  (#x51 #xE47F #xE264)))

;; Hába notation
(haba . (
  (#x00 #xE261)
  (#x1A #xEE64)
  (#x1B #xEE67)
  (#x21A #xEE68)
  (#x21B #xEE65)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xEE66)
  (#x37 #xEE69)
  (#x236 #xEE67 #xE263)
  (#x237 #xEE64 #xE264)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xEE64 #xE263)
  (#x51 #xEE67 #xE264)))
)


\include "ekmel-main.ily"


% Alteration order for key signatures with quarter tones
\layout {
  \context {
    \Score
    keyAlterationOrder = #'(
      (6 . -1/2) (2 . -1/2) (5 . -1/2) (1 . -1/2) (4 . -1/2) (0 . -1/2) (3 . -1/2)
      (3 .  1/2) (0 .  1/2) (4 .  1/2) (1 .  1/2) (5 .  1/2) (2 .  1/2) (6 .  1/2)
      (6 . -1/4) (2 . -1/4) (5 . -1/4) (1 . -1/4) (4 . -1/4) (0 . -1/4) (3 . -1/4)
      (3 .  1/4) (0 .  1/4) (4 .  1/4) (1 .  1/4) (5 .  1/4) (2 .  1/4) (6 .  1/4)
      (6 . -3/4) (2 . -3/4) (5 . -3/4) (1 . -3/4) (4 . -3/4) (0 . -3/4) (3 . -3/4)
      (3 .  3/4) (0 .  3/4) (4 .  3/4) (1 .  3/4) (5 .  3/4) (2 .  3/4) (6 .  3/4)
      (6 .   -1) (2 .   -1) (5 .   -1) (1 .   -1) (4 .   -1) (0 .   -1) (3 .   -1)
      (3 .    1) (0 .    1) (4 .    1) (1 .    1) (5 .    1) (2 .    1) (6 .    1)
    )
  }
}
