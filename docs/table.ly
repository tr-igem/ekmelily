%%
%%  table.ly  -  Create lookup tables for Ekmelily
%%
%%  Written 2025 by Thomas Richter <thomas-richter@aon.at>
%%  This program is free software.
%%  Use, redistribute, and modify it as you wish.
%%
%%
%%  Use this file to create lookup tables with the accidentals
%%  and note names defined in a tuning (system) of Ekmelily.
%%
%%  See the sample at the end of the file
%%  which makes use of the commands described below.
%%  Modify it as you wish.
%%
%%
%%  Set the following variables as desired:
%%

ekmFont = "Bravura"
%%  Font for the accidentals. Default is "Ekmelos".

ekmUse = "72-sims-english"
%%  Tuning, notation style, and language, separated by `-`.
%%  Each part is optional. Default tuning is 24.
%%  The value can be a string, symbol, or number.
%%  The corresponding Ekmelily file is included automatically.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%  Commands:
%%
%%  - \ekmNoteTable SORT ENHARMONIC
%%    Create a music sequence of all notes within the one-line octave
%%    with the accidentals of the selected notation style,
%%    and with the alterations and the note names of the selected language
%%    below the notes.
%%
%%  - \ekm-notename-table SORT ENHARMONIC
%%    Draw a table with the note names of the selected language
%%    as a markup list.
%%
%%  - \ekm-accidental-table SORT ENHARMONIC
%%    Draw a table with the accidentals of the selected notation style
%%    as a markup list.
%%
%%  - \ekmTableOutputSuffix
%%    Set the tuning, notation style, and language
%%    as the output filename suffix.
%%
%%  Parameters:
%%
%%  * SORT (symbol):
%%    'ascending-absolute
%%    'ascending
%%    'descending
%%    'grouped-absolute
%%    'grouped
%%
%%  * ENHARMONIC (boolean):
%%    #t includes enharmonically equivalent tone steps.
%%
%%  Used markup properties:
%%
%%  * baseline-skip
%%
%%  * style ('()): Style of the alteration numbers; see `\ekm-fraction`.
%%    'slash
%%    'line
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\version "2.19.22"

#(define ekm:tuning #f)
#(define ekm:file #f)
#(define ekm:style #f)
#(define ekm:language-name #f)

#(let* ((s (or (ly:get-option 'ekmuse)
               (and (defined? 'ekmUse) ekmUse)
               (and (defined? 'ekmSystem) ekmSystem)
               ""))
        (s (if (symbol? s) (symbol->string s)
           (if (number? s) (number->string s 10) s)))
        (s (string-split s #\-))
        (t (if (string-null? (car s)) "24" (car s)))
        (f (if (string=? "72" t)
            "ekmel.ily"
            (string-append "ekmel-" t ".ily"))))
  (set! ekm:tuning t)
  (set! ekm:file f)
  (set! ekm:style
    (if (or (null? (cdr s)) (string-null? (second s))) #f (second s)))
  (set! ekm:language-name
    (if (or (> 3 (length s)) (string-null? (third s))) #f (third s)))
  (if (ly:find-file f)
    (ly:parser-include-string (format #f "\\include \"~a\"\n" f))
    (ly:error "Tuning '~a' does not exist" t)))

#(if (and ekm:style
          (assq (string->symbol ekm:style) ekmNotations))
  (ekm:set-notation ekm:style))

#(if (and ekm:language-name
          (assq (string->symbol ekm:language-name) ekmLanguages))
  (ekm:set-language ekm:language-name)
  (set! ekm:language-name (symbol->string (caar ekmLanguages))))


#(define (ekm:select enh ac)
  (or enh (not (logtest ac EKM-ACODE-ENHEQ))))

#(define (ekm:code->correct-alter ac)
  (let ((a (if (= 0 ac) 0 (assv-ref ekmTuning (logand ac EKM-ACODE-MASK)))))
    (if (odd? ac) (- a) a)))

#(define ekm:sort `(
  (ascending-absolute
    ,(lambda (x) (+ (* 2048 (first x)) (abs (second x))))
    ,third)
  (ascending
    ,(lambda (x) (+ (* 2048 (first x)) (second x)))
    ,third)
  (descending
    ,(lambda (x) (- (- (second x) (* 2048 (first x)))))
    ,third)
  (grouped
    ,(lambda (x)
      (+ (if (negative? (second x)) (* -2048 (second x)) (- (* 2048 (second x)) 1024))
         (first x)))
    ,fourth)
  (grouped-absolute
    ,(lambda (x) (+ (* 2048 (abs (second x))) (first x)))
    ,(lambda (x) (abs (fourth x))))
))

#(define (ekm:sort-key sort)
  (first (or (assq-ref ekm:sort sort) (cdar ekm:sort))))

#(define (ekm:sort-break sort)
  (second (or (assq-ref ekm:sort sort) (cdar ekm:sort))))

#(define (ekm:note-table sort enharmonic)
  ;; Return a list with elements
  ;; (SCALE-STEP ALTER-STEP SCALE-INDEX CORRECT-ALTER ALTER NOTE-NAME ...)
  ;; or #t for break
  (let* ((sca (or (assv-ref ekmTuning -1) '(0 1 2 5/2 7/2 9/2 11/2)))
         (alt (fold (lambda (e r)
                (if (and (positive? (car e)) (< (cdr e) r)) (cdr e) r))
                1 ekmTuning))
         (scs (map (lambda (a) (round (/ a alt))) sca))
         (lt (ekm:language-table ekm:language-name))
         (sc (ekm:scale-names-table lt))
         (tab (if sc
            ;; new table
            (append-map! (lambda (i)
              (filter-map (lambda (e)
                (and
                  (ekm:select enharmonic (car e))
                  (let ((alt (ekm:code->correct-alter (car e))))
                    (cons*
                      (list-ref scs i)
                      (ekm:alter->step alt)
                      i
                      alt
                      (ekm:code->alter (car e))
                      (map symbol->string
                        (ekm:note-names (vector-ref sc i) e (second lt) #f))))))
                (ekm:alter-names-table lt)))
              (iota (vector-length sc)))
            ;; old table
            (filter-map (lambda (e)
              (and
                (ekm:tuning? (cddr e))
                (let ((alt (ekm:code->correct-alter (cddr e)))
                      (i (cadr e)))
                  (list
                    (list-ref scs i)
                    (ekm:alter->step alt)
                    i
                    alt
                    (ekm:code->alter (cddr e))
                    (symbol->string (car e))))))
              lt)))
         (key (ekm:sort-key sort))
         (brk (ekm:sort-break sort))
         (tab (sort-list! tab (lambda (x y) (< (key x) (key y)))))
         (lbrk (brk (car tab))))
    (append-map! (lambda (e)
      (if (eqv? lbrk (brk e))
        (list e)
        (begin
          (set! lbrk (brk e))
          (list #t e))))
      tab)))

#(define-markup-command (ekm-column layout props mk)
  (markup?)
  (interpret-markup layout props
    (make-pad-to-box-markup '(-8 . 0) '(0 . 0)
      (make-right-align-markup mk))))

#(define-markup-command (ekm-code layout props text)
  ;; \typewriter in 2.25
  (markup?)
  (interpret-markup layout
    (cons '((font-family . typewriter)
            (font-encoding . latin1)
            (font-features . ("-liga")))
      props)
    text))

#(define-markup-command (ekm-alter-and-names layout props alter names)
  (rational? pair?)
  (interpret-markup layout props
    (markup #:general-align X CENTER
      (make-center-column-markup (cons*
        (make-ekmelic-fraction-small-markup alter)
        (map (lambda (n) (markup #:fontsize -2 #:ekm-code n)) names))))))


ekmNoteTable =
#(define-music-function (sort enharmonic)
  (symbol? boolean?)
  (make-music 'SequentialMusic 'elements
    (map (lambda (e)
      (if (boolean? e)
        (make-music 'LineBreakEvent
          'break-permission 'force)
        (make-music 'NoteEvent
          'pitch (ly:make-pitch 0 (third e) (fifth e))
          'duration (ly:make-duration 2)
          'articulations (list
            (make-music 'TextScriptEvent
              'direction -1
              'text (make-ekm-alter-and-names-markup (fourth e) (list-tail e 5)))))))
      (ekm:note-table sort enharmonic))))

#(define-markup-list-command
  (ekm-notename-table layout props sort enharmonic)
  (symbol? boolean?)
  #:properties ((baseline-skip))
  (cons*
    (interpret-markup layout props (markup
      #:pad-to-box '(0 . 0) `(,(- (* 0.5 baseline-skip)) . 0)
      #:line (
        " "
        #:ekm-column "Scale"
        #:ekm-column "Step"
        #:ekm-column "Alter"
        #:hspace 5
        #:ekm-code ekm:language-name)))
    (space-lines baseline-skip
      (map (lambda (e)
        (interpret-markup layout props
          (if (boolean? e)
            " "
            (markup #:line (
              " "
              #:ekm-column (format #f "~d" (third e))
              #:ekm-column (format #f "~d" (+ (first e) (second e)))
              #:ekm-column #:ekmelic-fraction-small (fourth e)
              #:hspace 5
              #:ekm-code (string-join
                (map (lambda (n) (format #f "~9a" n)) (list-tail e 5))
                " "))))))
        (ekm:note-table sort enharmonic)))))

#(define-markup-list-command
  (ekm-accidental-table layout props sort enharmonic)
  (symbol? boolean?)
  #:properties ((baseline-skip))
  (let ((key (ekm:sort-key sort))
        (tab (filter-map (lambda (e)
             (and
               (not (null? (cdr e)))
               (ekm:select enharmonic (car e))
               (let ((alt (ekm:code->correct-alter (car e))))
                 (cons*
                   0
                   (ekm:alter->step alt)
                   alt
                   (ekm:code->alter (car e))
                   (if (char? (second e))
                     (list (char->integer (second e))) ; user defined
                     (cdr e))))))
             (or (assq-ref ekmNotations (string->symbol ekm:notation-name)) '()))))
    (cons*
      (interpret-markup layout props (markup
        #:pad-to-box '(0 . 0) `(,(- (* 0.5 baseline-skip)) . 0)
        #:line (
          " "
          #:ekm-column "Step"
          #:ekm-column "Alter"
          #:hcenter-in 14 #:ekm-code ekm:notation-name
          "Code point")))
      (space-lines baseline-skip
        (map (lambda (e)
          (interpret-markup layout props (markup #:line (
            " "
            #:vcenter #:ekm-column (format #f "~d" (second e))
            #:vcenter #:ekm-column #:ekmelic-fraction-small (third e)
            #:vcenter #:hcenter-in 14 #:ekmelic-char (fourth e)
            #:vcenter #:ekm-code
              (string-join
                (map (lambda (c) (format #f "U+~:@(~4,'0x~)" c)) (list-tail e 4))
                " ")))))
          (sort-list! tab (lambda (x y) (< (key x) (key y)))))))))

ekmTableOutputSuffix =
#(define-void-function () ()
  (set! (paper-variable #f 'output-suffix)
    (string-join
      (list ekm:tuning ekm:notation-name ekm:language-name)
      "-")))




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%  Sample to create lookup tables
%%  Modify it as you wish.
%%

\ekmTableOutputSuffix

\paper {
  indent = 0
  short-indent = 0
  ragged-right = ##f
  ragged-bottom = ##t

  system-system-spacing =
    #'((basic-distance . 13)
       (minimum-distance . 10)
       (padding . 2)
       (stretchability . 10))
}

\markup { \column {
  \line {
    Notes
    of tuning #ekm:tuning \concat { ( \ekm-code #ekm:file ) }
  }
  \line { "Notation style:" \ekm-code #ekm:notation-name }
  \line { "Language:" \ekm-code #ekm:language-name }
  \vspace #1
}}

\score {
  {
    \omit Staff.TimeSignature
    \omit Staff.BarLine
    \omit Score.BarNumber
    \set Score.proportionalNotationDuration = #(ly:make-moment 1/16)

    \override Score.TextScript.font-size = #-1
    \override Score.TextScript.baseline-skip = #1.7
    \override Score.TextScript.staff-padding = #4.2
    \override Score.TextScript.style = #'slash

    \accidentalStyle dodecaphonic

    \ekmNoteTable #'ascending-absolute ##t
  }

  \layout { }
}

\pageBreak

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\markup { \column {
  \line {
    Note names
    of tuning #ekm:tuning \concat { ( \ekm-code #ekm:file ) }
  }
  \vspace #1
}}

\markuplist {
  \override #'(baseline-skip . 2.6)
  \override #'(style . slash)

  \ekm-notename-table #'ascending-absolute ##t
}

\pageBreak

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\markup { \column {
  \line {
    Accidentals
    of tuning #ekm:tuning \concat { ( \ekm-code #ekm:file ) }
  }
  \vspace #1
}}

\markuplist {
  \override #'(baseline-skip . 3.2)
  \override #'(style . slash)

  \ekm-accidental-table #'ascending-absolute ##t
}

\pageBreak
