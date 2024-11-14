%%
%% Common definitions for "style.ly" files
%% demonstrating a notation style of Ekmelily
%%
%% Written by Thomas Richter <thomas-richter@aon.at>
%% This program is free software.
%% Use, redistribute, and modify it as you wish.
%%

\version "2.19.0"

\include "../../Ekmelos/ly/ekmelos-map.ily"
\include "../ly/notation-names.ily"

\pointAndClickOff



styleFontOutputSuffix =
#(define-void-function () ()
  (set! (paper-variable #f 'output-suffix)
    (if (string=? "Ekmelos" ekm:font-name)
      ekm:notation-name
      (string-append ekm:notation-name "-" (string-downcase ekm:font-name)))))

#(define-markup-command (acc layout props code alt)
  (integer? rational?)
  (interpret-markup layout props (markup
    #:line (
      #:hspace 11
      #:override `(font-name . ,ekm:font-name)
      #:vcenter #:fontsize 4 #:hcenter-in 4
        #:pad-markup 0.5 (ly:wide-char->utf-8 code)
      #:vcenter #:typewriter #:fontsize -3 #:hcenter-in 7
        #:ekmelic-fraction alt
      #:vcenter #:typewriter #:hcenter-in 13
        (format #f "U+~:@(~4,'0x~)" code)
      #:vcenter #:typewriter
        (ekm-glyphname code)))))

%% Draw title and list of used accidental symbols
#(define-markup-command (style layout props tuning)
  (string?)
  (let* ((u (defined? 'userStyle))
         (n (if u (first userStyle)
                  (string->symbol ekm:notation-name)))
         (l (or (if u (third userStyle) (assq-ref ekmNotations n))
                '()))
         (l (filter-map (lambda (e)
              (if (= 2 (length e))
                (cons*
                  (if (char? (cadr e))
                    (char->integer (cadr e))
                    (cadr e))
                  (ekm:code->alter (car e))
                  (logand (car e) 1)) ;; sort weight
                #f))
              l)))
    (stack-lines DOWN 0.0 3.7
      (cons*
        (if u
          (interpret-markup layout props (markup
            #:sans (string-append
              "User defined notation style \x22" n "\x22 for " tuning)))
          (interpret-markup layout props (markup
            #:sans (string-append
              (assq-ref ekmNotationNames n)
              " notation style (" ekm:notation-name ") for " tuning))))
        (map (lambda (e)
          (interpret-markup layout props (markup
            #:acc (first e) (second e))))
          (sort-list l (lambda (x y) (< (cddr x) (cddr y)))))))))

%% Draw usage in a LilyPond input file
#(define-markup-command (usage layout props inc)
  (string?)
  (let* ((u (defined? 'userStyle))
         (m (if u
              (append
                (list (string-append "\\ekmelicUserStyle " (first userStyle) " #'("))
                (map (lambda (e)
                  (format #f "  (~a~a)"
                    (car e)
                    (string-concatenate
                      (map (lambda (c)
                        (format #f (if (char? c) " ~@c" " #x~:@(~x~)") c))
                        (cdr e)))))
                  (second userStyle))
                (list ")"))
              (list (string-append "\\ekmelicStyle " ekm:notation-name))))
         (m (cons*
              (string-append "\\include \x22" inc "\x22")
              m))
         (m (if (string=? "Ekmelos" ekm:font-name)
              m
              (cons*
                (string-append "ekmelicFont = \x22" ekm:font-name "\x22")
                m))))
    (interpret-markup layout props (markup
      #:override '(baseline-skip . 2.8)
      #:line (
        #:sans "Usage:"
        #:hspace 2
        #:typewriter (make-left-column-markup m))))))

#(define-markup-command (style-and-usage layout props tuning inc)
  (string? string?)
  (interpret-markup layout props (markup
    #:column ( #:style tuning  #:vspace 2.5  #:usage inc ))))

#(define-markup-command (notenames-for layout props)
  ()
  (interpret-markup layout props (markup
    #:column (
      #:line (
        #:sans "Notenames listed for "
        #:typewriter (string-join languageNames " "))
      #:vspace 1.0))))

#(define-markup-command (language-names layout props)
  ()
  (interpret-markup layout props
    (make-column-markup languageNames)))

%% Draw the notenames for all languages specified in languageNames
%% and the alteration of music, stacked vertically
#(define-markup-command (note-names-and-alter layout props music)
  (ly:music?)
  (let* ((p (ly:music-property music 'pitch))
         (a (ly:pitch-alteration p)))
    (interpret-markup layout props
      (make-center-column-markup (append
        (map (lambda (l) (ekm:pitch-name p l 'lily 0))
          languageNames)
        (list (markup
          #:pad-to-box '(0 . 0) '(0 . 2.4) 
          #:fontsize 5
          #:ekmelic-fraction-small a)))))))

noteNamesAndAlter =
#(define-music-function (music)
  (ly:music?)
  (for-some-music
    (lambda (m)
      (if (music-is-of-type? m 'note-event)
        (begin (set! m #{ #m _ \markup \note-names-and-alter #m #}) #t)
      (if (music-is-of-type? m 'skip-event)
        (begin (set! m #{ #m _ \markup \language-names #}) #t)
        #f)))
    music)
  music)



scoreDefs = {
  \clef treble
  \omit Staff.TimeSignature
  \accidentalStyle dodecaphonic

  \override Score.TextScript.self-alignment-X = #CENTER
  \override Score.TextScript.font-family = #'typewriter
  \override Score.TextScript.font-size = #-5
  \override Score.TextScript.baseline-skip = #1.6
  \override Score.TextScript.staff-padding = #4.2

  \cadenzaOn
}

\header {
  tagline = ""
}

\paper {
  %#(define fonts
  %  (make-pango-font-tree
  %    "Times New Roman"
  %    "Arial"
  %    "Lucida Console"
  %    1.0))

  left-margin = 5 \mm
  right-margin = 5 \mm
  top-margin = 5 \mm
  bottom-margin = 5 \mm
  indent = 0
  short-indent = 0
  ragged-right = ##f
  ragged-bottom = ##t

  oddHeaderMarkup = \markup {
    \unless \on-first-page \sans \column {
      \fill-line {
        \null
        \if \should-print-page-number \fromproperty #'page:page-number-string
      }
      \vspace #2
  }}
  evenHeaderMarkup = \markup {
    \sans \column {
      \fill-line {
        \if \should-print-page-number \fromproperty #'page:page-number-string
        \null
      }
      \vspace #2
  }}
  oddFooterMarkup = \markup \null
  evenFooterMarkup = \oddFooterMarkup

  system-system-spacing.padding = 4
}
