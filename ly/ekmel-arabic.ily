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
%% Latest revision: 2024-09-10
%%
%% This file is a variant of "ekmel-24.ily" for Arabic scores,
%% like LilyPond's "arabic.ly" and "hel-arabic.ly"
%% but with the correct accidentals (U+ED30 - U+ED38 in SMuFL).
%%
%% The tables of Arabic maqamat (keys) are taken from
%% - "arabic.ly"
%%   Copyright (C) 2017 Amir Czwink <amir130@hotmail.de>
%%   Copyright (C) 2008 Neil Puttock
%% - "hel-arabic.ly"
%%   Copyright (C) 2014--2022 Hassan EL FATIHI <hassan.elfatihi@free.fr>
%%

\version "2.19.22"


% Tuning table
ekmTuning = #'(
  (#x1A . 1/4)
  (#x28 . 1/2)
  (#x36 . 3/4)
  (#x44 . 1)
  (#x54 . 5/2)
  (#x56 . 7/2))


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
  (#x45 bb)))

;; Arabic names by Hassan El fatihi <hassan.elfatihi@free.fr>
(arabic . (
  0
  ()
  (#x1A dd)
  (#x1B db)
  (#x28 d)
  (#x29 b)
  (#x36 tqd)
  (#x37 tqb)
  (#x44 x)
  (#x45 bb)
  (#x54 fhd)
  (#x55 fhb)
  (#x56 shd)
  (#x57 shb)))
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
  (#x54)
  (#x55)
  (#x56)
  (#x57)))

;; Helmakam notation
(helmakam . (
  (#x00 #xED34)
  (#x1A #xED35)
  (#x1B #xED33)
  (#x28 #xED36)
  (#x29 #xED32)
  (#x36 #xE446)
  (#x37 #xED31)
  (#x44 #xED38)
  (#x45 #xED30)
  (#x54 #xE447)
  (#x55 #xF61B)
  (#x56 #xED37)
  (#x57 #xE440)))
)


% Padding table
ekmPadding = #'(
  (#xED32 . 0.375)
  (#xED31 . 0.5)
  (#xED30 . 0.65)
)


\include "ekmel-main.ily"


%% Arabic maqamat (keys)

%% Rast: c d edb f g a bdb c c bb a g f edb d c
%% This key can also be used for:
%% Irak: bdb c d edb f g a bdb
%% Rahatalarouah: bdb c d edb fd g a bdb
%% Alboustankar: bdb c d edb f gb a bdb
%% Sajakar: c dd edb f g a bdb altering the note d with dd (dis)
rast = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)

%% Souznak: c' d' edb' f' g' ab' b' c'' c'' b' ab' g' f' edb' d' c'
souznak = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Alhizazkar: c db e f g ab b c c b ab g f e db c
alhizazkar = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Hizazkarkurdy: c db eb f g ab bb c c bb ab g f eb db c
hizazkarkurdy = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%% Nahawande: c d eb f g ab b c c bb ab g f eb d c
nahawande = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Nawaatar: c d eb fd g ab b c c b ab g fd eb d c
nawaatar = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,SHARP)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Nakriz: c d eb fd g a bb c c bb a g fd eb d c
nakriz = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,SHARP)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,FLAT)
)

%% Bayati: d edb f g a bb c d c bb a g f edb d c
%% Bayati: en do: c' ddb' eb' f' g' ab' bb' c'' c'' bb' ab'g' f'  eb' ddb' c'
bayati = #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%% Houssaini: d edb f g a bdb c d c bb a g f edb d c
%% Houssaini: en do: c ddb eb f g adb bb c c bb ab g f eb ddb c
houssaini =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,FLAT)
)

%% Karjkhar: d' edb' f' g' ab' b' c'' d'' d'' b' ab' g' f' edb d
%% Karjkhar: en do: c' ddb' eb' f' gb' a' bb' c''bb' a' gb' f' eb' dbd' c'
karjkhar =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,FLAT)
  (5 . ,NATURAL)
  (6 . ,FLAT)
)

%% Saba: d edb f gb a bb c d c bb a gb f edb d
%% Saba en do: c ddb eb fb g ab bb c c bb ab g fb eb ddb c
saba =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,FLAT)
  (3 . ,FLAT)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%% Kurd: d eb f g a bb c d c bb a g f eb d
%% Kurd: en do: c db eb f g ab bb c  c bb ab g f eb db c
kurd = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%%% Sahnaz: d' eb' fd' g' a' bb' cd'' d''
%%% Sahnaz: eb do: c' db' e' f' g' ab' b' c''
% Sahnaz (in D) is a transposition of Alhizazkar (in C)
%sahnaz = #`(
%  (0 . ,NATURAL)
%  (1 . ,FLAT)
%  (2 . ,NATURAL)
%  (3 . ,NATURAL)
%  (4 . ,NATURAL)
%  (5 . ,FLAT)
%  (6 . ,NATURAL)
%)

%% Gammes commençant par midb

%% Huzam: edb f g ab b c d edb edb d c b ab f ebd: mi et la altérées
%% Huzam: en do: c ddb edb fdb gdd adb bdb c c bdb adb gdd fdb edb ddb c
huzam =  #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,SEMI-FLAT)
  (3 . ,SEMI-FLAT)
  (4 . ,SEMI-SHARP)
  (5 . ,SEMI-FLAT)
  (6 . ,SEMI-FLAT)
)

%% Sikah: edb f g a bdb d edb edb d a bb a g f edb
%% Sikah: en do: c ddb edb fdd g adb bdb c c bdb adb gdb fdd edb ddb c
sikah = #`(
  (0 . ,NATURAL)
  (1 . ,SEMI-FLAT)
  (2 . ,SEMI-FLAT)
  (3 . ,SEMI-SHARP)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,SEMI-FLAT)
)

%% Yakah: g a bdb c d e fdd g g f edb d c bdb a g
%% Yakah: en do: c' ddb' edb' f' gdb' adb' bdb' c'' cdb'' bdb' adb' gdb' f' edb' ddb' c
yakah = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,SEMI-FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)

%% Hizaz Avec sib et mib: d' eb' fd' g' a' bdb' c'' d'' d'' c''bb' a' g' fd' eb' d'
%% Hizaz: en do: c db e f g adb bb c c bb ab g f e db c
hizaz = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,SEMI-FLAT)
  (6 . ,FLAT)
)

%% Jaharkah: f g a bb c d edb f f eb d c bb a g f
%% Jaharkah: en do : c d e f g a bdb c c bb a g f e d c
jaharkah = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,NATURAL)
  (6 . ,SEMI-FLAT)
)

%% Chatarabane: g ab b c d eb fd g f eb d c bb ab g
%% Chatarabane: en do : c db e f g ab b
chatarabane = #`(
  (0 . ,NATURAL)
  (1 . ,FLAT)
  (2 . ,NATURAL)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,NATURAL)
)

%% Farahfaza: g a bb c d eb f g f eb d c bb a g
%% Farahfaza: en do: c d eb f g ab bb
farahfaza = #`(
  (0 . ,NATURAL)
  (1 . ,NATURAL)
  (2 . ,FLAT)
  (3 . ,NATURAL)
  (4 . ,NATURAL)
  (5 . ,FLAT)
  (6 . ,FLAT)
)

%% Hijaz family
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
