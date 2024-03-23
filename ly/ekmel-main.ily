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
%% File: ekmel-main.ily  -  Main include file
%% Latest revision: 2024-03-12
%%

\version "2.19.22"


%% Enharmonical equivalence

#(define EKM-ACODE-ENHEQ #x0200)
#(define EKM-ACODE-MASK (lognot #x0201))
#(define EKM-ALTER-ENHEQ 1/1024)

#(define (ekm:enheq? alt)
  (<= 1024 (denominator alt)))

#(define (ekm:pure-alter alt)
  (if (ekm:enheq? alt) (- (abs alt) EKM-ALTER-ENHEQ) (abs alt)))


%% Tuning

#(define (ekm:tuning? ac)
  (or (= 0 ac) (pair? (assv (logand ac EKM-ACODE-MASK) ekmTuning))))

#(define (ekm:code->alter ac)
  (let* ((a (if (= 0 ac) 0 (assv-ref ekmTuning (logand ac EKM-ACODE-MASK))))
         (a (if (logtest ac EKM-ACODE-ENHEQ) (+ a EKM-ALTER-ENHEQ) a)))
    (if (odd? ac) (- a) a)))

#(define (ekm:alter->code alt)
  (let ((a (ekm:pure-alter alt)))
    (let ac ((t ekmTuning))
      (if (null? t) 0
      (if (eqv? a (cdar t))
        (let* ((c (caar t))
               (c (if (ekm:enheq? alt) (logior c EKM-ACODE-ENHEQ) c)))
          (if (negative? alt) (1+ c) c))
        (ac (cdr t)))))))


%% Language (pitch names)

ekmScaleNames = #'#(
  #(c d e f g a b)
  #(c d e f g a h)
  #(do re mi fa sol la si)
  #(do ré mi fa sol la si)
)

#(define EKM-ALTER-0 '(0 #f))

#(define ekm:language #f)
#(define ekm:acodes #f)

#(define (ekm:language-table lang)
  (let ((l (or (assq-ref ekmLanguages (string->symbol lang))
               ekm:language
               (cdar ekmLanguages))))
    (if (symbol? l) (assq-ref ekmLanguages l) l)))

#(define (ekm:scale-names-table l)
  (if (number? (car l)) (vector-ref ekmScaleNames (car l))
  (if (vector? (car l)) (car l) #f)))

#(define (ekm:alter-names-table l)
  (let ((a (cddr l)))
    (if (positive? (caar a)) (cons* EKM-ALTER-0 a) a)))

#(define (ekm:extra-names n x)
  (define (eq-pfx? a b)
    (if (symbol? a) (eq? a b) (string-prefix? a (symbol->string b))))
  (reverse
    (fold (lambda (a r)
      (if (null? r)
        r
      (if (pair? a)
        (if (eq-pfx? (car a) (last r))
          (cons*
            (if (symbol? (car a))
              (cdr a)
              (string->symbol (string-append
                (cdr a)
                (substring (symbol->string (last r)) (string-length (car a))))))
            r)
          r)
        (if (eq-pfx? a (last r)) (drop-right! r 1) r))))
      (list n)
      x)))

#(define (ekm:note-names sn al x pt)
  (append-map! (lambda (an)
    (map (lambda (n)
      (if pt (cons n pt) n))
      (if (symbol? sn)
        (ekm:extra-names (if an (symbol-append sn an) sn) x)
        (list (or an (string->symbol ""))))))
    (cdr al)))

#(define (ekm:set-language lang)
  (let* ((l (ekm:language-table lang))
         (s (ekm:scale-names-table l))
         (a (if s (ekm:alter-names-table l) #f))
         (ac (if s (map car a) '()))
         (p (if s
              (append-map! (lambda (i)
                (append-map! (lambda (al)
                  (ekm:note-names
                    (vector-ref s i)
                    al
                    (second l)
                    (ly:make-pitch -1 i (ekm:code->alter (car al)))))
                  a))
                (iota (vector-length s)))
              (filter-map (lambda (e)
                (if (ekm:tuning? (cddr e))
                  (begin
                    (set! ac (cons* (cddr e) ac))
                    (cons
                      (car e)
                      (ly:make-pitch -1 (cadr e) (ekm:code->alter (cddr e)))))
                  #f))
                l))))
    (set! ekm:language l)
    (set! ekm:acodes (cons* #f ac)) ;; dummy #f for delv! in ekm:set-notation
    (set! pitchnames p)
    (ly:parser-set-note-names p)))


%% Notation (style)

#(define (ekm:elem? x)
  (or (integer? x)
      (char? x)
      (markup? x)))

#(define (ekm:elem->markup l)
  (map (lambda (e)
    (cond
      ((integer? e) (ly:wide-char->utf-8 e))
      ((char? e) (ly:wide-char->utf-8 (char->integer e)))
      ((markup? e) e)
      (else empty-markup)))
    l))

#(define ekm:notation-name "")
#(define ekm:notation '())

#(define EKM-NOTATION `(
  (leftparen . ,(ekm:elem->markup '(#xE26A)))
  (rightparen . ,(ekm:elem->markup '(#xE26B)))
  (default)))

#(define (ekm:notation-table style)
  (let ((n (or (assq-ref ekmNotations (string->symbol style))
               (cdar ekmNotations))))
    (if (symbol? n) (assq-ref ekmNotations n) n)))

#(define (ekm:set-notation style)
  (let* ((ac (list-copy ekm:acodes))
         (n (ekm:notation-table style))
         (t (filter-map (lambda (e)
              (if (ekm:tuning? (car e))
                (begin
                  (delv! (car e) ac)
                  (cons* (ekm:code->alter (car e))
                         (ekm:elem->markup (cdr e))))
                #f))
              n))
         (r (map (lambda (e)
              (let ((enh #f))
                (if (logtest e EKM-ACODE-ENHEQ)
                  (begin
                    (set! enh (assv-ref n (logxor e EKM-ACODE-ENHEQ)))
                    (if (not enh)
                      (ly:warning "Missing accidental for enh. equivalent alteration code ~a."
                        (format #f "0x~:@(~x~)" e))))
                  (ly:warning "Missing accidental for alteration code ~a."
                    (format #f "0x~:@(~x~)" e)))
                (cons* (ekm:code->alter e)
                       (ekm:elem->markup (or enh '())))))
              (cdr ac)))) ;; missing codes except dummy car
    (set! ekm:notation-name style)
    (set! ekm:notation (append t r EKM-NOTATION))))


%% Font

#(define ekm:font-name
  (let ((font (ly:get-option 'ekmelic-font)))
    (if font
      (symbol->string font)
      (if (and (defined? 'ekmelicFont) (string? ekmelicFont))
        ekmelicFont
        "Ekmelos"))))

#(define ekm:font-size 5)


%% Main procs (stencils)

#(define-markup-command (ekmelic-acc layout props alt rst par)
  (rational? boolean? boolean?)
  #:properties ((font-size 0))
  (let ((a (or (assv-ref ekm:notation alt)
               (assv-ref ekm:notation 'default))))
    (fold (lambda (m acc)
      (ly:stencil-combine-at-edge
        acc
        X RIGHT
        (if (string? m)
          (interpret-markup layout
            (cons
             `((font-size . ,(+ ekm:font-size font-size))
               (font-name . ,ekm:font-name))
              props)
            m)
          (interpret-markup layout props m))
        0.12))
      empty-stencil
      (if rst
        (append (or (assv-ref ekm:notation 0) '("")) a)
      (if par
        (append
          (assv-ref ekm:notation 'leftparen)
          a
          (assv-ref ekm:notation 'rightparen))
        a)))))

#(define* (ekm:acc grob #:optional par)
  (grob-interpret-markup grob
    (make-ekmelic-acc-markup
      (ly:grob-property grob 'alteration)
      (not (ly:grob-property grob 'restore-first))
      par)))

#(define* (ekm:key grob #:optional cancel)
  (let ((c0 (ly:grob-property grob 'c0-position)))
    (fold (lambda (a sig)
      (ly:grob-set-property! grob 'alteration (cdr a)) ;; for markup
      (ly:stencil-stack
        (grob-interpret-markup grob
          (make-raise-markup
            (/ (car (key-signature-interface::alteration-positions a c0 grob)) 2)
            (make-ekmelic-acc-markup (if cancel 0 (cdr a)) #f #f)))
        X RIGHT sig 0.15)) ;; or 0.16 = 0.04em
      '()
      (ly:grob-property grob 'alteration-alist))))


%% Aux procs for ekmelicUserStyle

#(define (ekm:list-prefix pfx l)
  (cond
    ((null? pfx) l)
    ((null? l) #f)
    ((equal? (car pfx) (car l))
      (ekm:list-prefix (cdr pfx) (cdr l)))
    (else #f)))

#(define (ekm:list-replace! old new l)
  (if (null? l)
    '()
    (let ((tl (ekm:list-prefix old l)))
      (if tl
        (append new (ekm:list-replace! old new tl))
        (begin
          (set-cdr! l (ekm:list-replace! old new (cdr l)))
          l)))))


%% Main settings

language =
#(define-void-function (lang)
  (string?)
  (ekm:set-language lang))

ekmelicStyle =
#(define-void-function (style)
  (string?)
  (if (string=? "void" style)
    (set! ekm:notation EKM-NOTATION)
    (ekm:set-notation style)))

ekmelicUserStyle =
#(define-void-function (name def)
  (string? list?)
  (set! ekm:notation-name
    (if (string-null? name) (string-append ekm:notation-name "-user") name))
  (for-each (lambda (u)
    (if (and (pair? u) (not (null? (cdr u))))
      (let* ((old (assv-ref ekm:notation (car u)))
             (new (ekm:elem->markup (cdr u))))
        (if old
          (for-each (lambda (e)
            (set-cdr! e (ekm:list-replace! old new (cdr e))))
            ekm:notation)))))
    def))

ekmelicOutputSuffix =
#(define-void-function () ()
  (set! (paper-variable #f 'output-suffix) ekm:notation-name))


%% Markup commands

#(define-markup-command (ekmelic-style-name layout props)
  ()
  (interpret-markup layout props
    ekm:notation-name))

#(define-markup-command (ekmelic-font-name layout props)
  ()
  (interpret-markup layout props
    ekm:font-name))

#(define-markup-command (ekmelic-char layout props alt)
  (rational?)
  #:properties ((font-size 1))
  (interpret-markup
    layout
    (cons `((font-size . ,(- font-size 3))) props)
    (make-ekmelic-acc-markup alt #f #f)))

#(define-markup-command (ekmelic-elem layout props elem)
  (ekm:elem?)
  (interpret-markup layout
    (cons
      `((font-size . ,ekm:font-size)
        (font-name . ,ekm:font-name))
      props)
    (car (ekm:elem->markup (list elem)))))

%% Variant of \fraction
%% scm/lily/define-markup-commands.scm
#(define-markup-command (ekm-fraction layout props arg1 arg2)
  (markup? markup?)
  #:properties ((font-size 0))
  (let* ((mag (magstep font-size))
         (m1 (ly:stencil-aligned-to (interpret-markup layout props arg1) X CENTER))
         (m2 (ly:stencil-aligned-to (interpret-markup layout props arg2) X CENTER))
         (w (interval-union
              (ly:stencil-extent m1 X)
              (ly:stencil-extent m2 X)))
         (h (ly:stencil-extent m1 Y))
         (line (make-filled-box-stencil
                 w (cons (* mag -0.06) (* mag 0.06))))
         (sil (stack-stencils Y DOWN (* mag 0.2) (list m1 line m2))))
    ;; (+ 0.2 0.06 0.5)
    (ly:stencil-translate sil
      (cons (- (car w)) (+ (- (car h)) (* mag 0.76))))))

#(define (ekm:with-sign num arg)
  (if (negative? num)
    (make-concat-markup (list
      "-" (make-hspace-markup (if (integer? num) 0 0.1)) arg))
    arg))

#(define-markup-command (ekmelic-fraction layout props alt)
  (rational?)
  (let* ((a (ekm:pure-alter alt))
         (n (number->string (numerator a))))
    (interpret-markup layout props
      (ekm:with-sign alt
        (if (integer? a)
          n
          (make-ekm-fraction-markup
            n (number->string (denominator a))))))))

#(define-markup-command (ekmelic-fraction-small layout props alt)
  (rational?)
  #:properties ((font-size 0))
  (interpret-markup layout props
    (if (integer? alt)
      (make-ekmelic-fraction-markup alt)
      (ekm:with-sign alt
        (make-stencil-markup
          (ly:stencil-translate-axis
            (interpret-markup layout
              (cons `((font-size . ,(- font-size 3))) props)
              (make-ekmelic-fraction-markup (abs alt)))
            (* 0.14 (magstep font-size))
            Y))))))

#(define-markup-command (ekmelic-table layout props natural composite order)
  (boolean? boolean? number?)
  #:properties ((font-size 0)
                (width 4))
  (let* ((abc (if (< -2 order 2) + abs))
         (els (filter (lambda (el)
                (and (not (symbol? (car el)))
                     (not (ekm:enheq? (car el)))
                     (or composite (= 1 (length (cdr el))))
                     (or natural (not (zero? (car el))))))
                ekm:notation))
         (els (stable-sort els
                (lambda (y x)
                  ((if (negative? order) > <)
                    (abc (car y)) (abc (car x)))))))
    (stack-stencil-line 0
      (map (lambda (el)
        (interpret-markup layout props
          (markup #:center-column (
            #:line (#:hcenter-in width #:ekmelic-acc (car el) #f #f)
            #:fontsize (- font-size 2)
            #:normal-text
            #:line (#:ekmelic-fraction (car el))))))
        els))))


%% NoteNames context

#(define (ekm:pitch-name-old p l)
  (reverse
    (let ((s (ly:pitch-notename p))
          (a (ekm:alter->code (ly:pitch-alteration p))))
      (fold (lambda (e r)
        (if (and (= s (cadr e)) (= a (cddr e)))
          (cons* (car e) r)
          r))
        '()
        l))))

#(define (ekm:pitch-name p lang part sel)
  (let* ((l (ekm:language-table lang))
         (s (ekm:scale-names-table l))
         (sn (if (or (eq? 'alteration part) (not s))
               #t
               (vector-ref s (ly:pitch-notename p))))
         (al (if (or (boolean? part) (not s))
               EKM-ALTER-0
               (assv (ekm:alter->code (ly:pitch-alteration p))
                     (ekm:alter-names-table l))))
         (n (if (not s)
              (ekm:pitch-name-old p l)
            (if (and sn al)
              (ekm:note-names sn al (second l) #f) '()))))
    (if (eq? #t sel)
      n
    (if (null? n)
      (if (= 0 sel) " " empty-markup)
    (if (< sel 0)
      (make-override-markup '(baseline-skip . 2.0)
        (make-column-markup
          (map symbol->string n)))
      (symbol->string
        (if (< sel (length n)) (list-ref n sel) (last n))))))))

#(define (ekm:note-name-markup p ctx)
  (let* ((acc (ly:context-property ctx 'printAccidentalNames))
         (oct (ly:context-property ctx 'printOctaveNames)))
    (make-override-markup '(word-space . 0.15)
      (make-line-markup (list
        (if (eq? 'accidental acc)
          empty-markup
          (ekm:pitch-name p
            (ly:context-property ctx 'printNotesLanguage "")
            (or (eq? 'fraction acc) acc)
            (if (eq? 'all acc) -1 0)))
        (if (or (eq? #t acc) (eq? 'accidental acc))
          (make-raise-markup 0.5
            (make-ekmelic-acc-markup (ly:pitch-alteration p) #f #f))
          empty-markup)
        (if (eq? 'fraction acc)
          (make-ekmelic-fraction-small-markup (ly:pitch-alteration p))
          empty-markup)
        (if oct
          (let ((o (1+ (ly:pitch-octave p))))
            (if (eq? 'scientific oct)
              (make-sub-markup (number->string (+ 3 o)))
              (make-string (abs o) (if (> o 0) #\' #\,))))
          empty-markup))))))


%% Initializations

#(let ((s (assv-ref ekmTuning -1)))
  (ly:set-default-scale
    (ly:make-scale
      (if s (list->vector s) #(0 1 2 5/2 7/2 9/2 11/2)))))

#(ekm:set-language (symbol->string (caar ekmLanguages)))

#(ekm:set-notation (symbol->string
  (or (ly:get-option 'ekmelic-style) (caar ekmNotations))))

\layout {
  \context {
    \Score
    \override Accidental.stencil = #ekm:acc
    \override Accidental.horizontal-skylines = #'()
    \override Accidental.vertical-skylines = #'()
    \override AccidentalCautionary.stencil = #(lambda (grob) (ekm:acc grob #t))
    \override KeySignature.stencil = #ekm:key
    \override KeyCancellation.stencil = #(lambda (grob) (ekm:key grob #t))
    \override TrillPitchAccidental.stencil = #ekm:acc
    \override AmbitusAccidental.stencil = #ekm:acc
    noteNameFunction = #ekm:note-name-markup
  }
}
