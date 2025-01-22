%% This file is part of Ekmelily - Notation of microtonal music with LilyPond.
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
%% File: ekmel-31.ily  -  Include file for 31-EDO tuning
%%
%%
%% Note names from the semitone names in LilyPond 2.22
%% Copyright (C) 2010--2020 Valentin Villenave <valentin@villenave.net> et al.
%%
%% Extended with names for 1st, 3rd, 5th degree
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (-1     0 30/31 60/31 78/31 108/31 138/31 168/31)
  (#x1A . 6/31)
  (#x28 . 12/31)
  (#x36 . 18/31)
  (#x44 . 24/31)
  (#x50 . 30/31))


% Language tables
ekmLanguages = #'(

;; Dutch names by Han-Wen Nienhuys <hanwen@xs4all.nl>
(nederlands . (
  0
  (("ees" . "es") (ee . eh) "ee"
   ("aes" . "as") (ae . ah) "ae")
  (#x1A i)
  (#x1B e)
  (#x28 is)
  (#x29 es)
  (#x36 isi)
  (#x37 ese)
  (#x44 isis)
  (#x45 eses)
  (#x50 isisi)
  (#x51 esese)))

;; German names by Roland Meier <meier@informatik.th-darmstadt.de>,
;; Bjoern Jacke <bjoern.jacke@gmx.de>
(deutsch . (
  1
  (("ees" . "es") (ee . eh) "ee"
   ("aes" . "as") (ae . ah) "ae")
  (#x1A i)
  (#x1B e)
  (#x28 is)
  (#x29 es)
  (#x36 isi)
  (#x37 ese)
  (#x44 isis)
  (#x45 eses)
  (#x50 isisi)
  (#x51 esese)))

;; Spanish names by Carlos García Suárez <cgscqmp@terra.es>,
;; Maximiliano G. G. <mxgdvg@yahoo.it>
(espanol . español)
(español . (
  2
  ()
  (#x1A cs)
  (#x1B cb)
  (#x28 s)
  (#x29 b)
  (#x36 tcs)
  (#x37 tcb)
  (#x44 ss x)
  (#x45 bb))) ; #x50,#x51 missing

;; Italian names by Paolo Zuliani <zuliap@easynet.it>,
;; Eric Wurbel <wurbel@univ-tln.fr>
(italiano . (
  2
  ()
  (#x1A sd)
  (#x1B sb)
  (#x28 d)
  (#x29 b)
  (#x36 dsd)
  (#x37 bsb)
  (#x44 dd)
  (#x45 bb))) ; #x50,#x51 missing

;; French names by Valentin Villenave <valentin@villenave.net>
(français . (
  3
  (("ré" . "re"))
  (#x1A sd)
  (#x1B sb)
  (#x28 d)
  (#x29 b)
  (#x36 dsd)
  (#x37 bsb)
  (#x44 dd x)
  (#x45 bb))) ; #x50,#x51 missing

;; Portuguese names by Pedro Kröger <kroeger@pedrokroeger.net>
(portugues . português)
(português . (
  2
  ()
  (#x1A sqt)
  (#x1B bqt)
  (#x28 s)
  (#x29 b)
  (#x36 stqt)
  (#x37 btqt)
  (#x44 ss)
  (#x45 bb))) ; #x50,#x51 missing
)


% Notation tables
ekmNotations = #'(

;; Standard notation (with arrows)
(std . (
  (#x00 #xE261)
  (#x1A #xE27A)
  (#x1B #xE27B)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE27A #xE262)
  (#x37 #xE27B #xE260)
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
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE30A #xE262)
  (#x37 #xE30B #xE260)
  (#x44 #xE47D)
  (#x45 #xE264)
  (#x50 #xE30A #xE47D)
  (#x51 #xE30B #xE264)))

;; Stein / Zimmermann notation
(stz . (
  (#x00 #xE261)
  (#x1A #xE282)
  (#x1B #xE284)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE283)
  (#x37 #xE285)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE282 #xE263)
  (#x51 #xE284 #xE264)))

;; Stein / Half flat notation
(sth . (
  (#x00 #xE261)
  (#x1A #xE282)
  (#x1B #xF612)
  (#x28 #xE262)
  (#x29 #xE260)
  (#x36 #xE283)
  (#x37 #xF613)
  (#x44 #xE263)
  (#x45 #xE264)
  (#x50 #xE282 #xE263)
  (#x51 #xF612 #xE264)))
)


% Text align table
ekmTextAlign = #'(
  ; stz
  (#xE282 . 0)
  (#xE283 . 0)
)


\include "ekmel-main.ily"
