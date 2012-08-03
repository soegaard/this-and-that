#lang racket

;;;
;;; Implementing a lexer using the technique in Rob Pikes lecture.
;;; Video:  http://www.youtube.com/watch?v=HxaD_trXwRE
;;; Slides: http://rspace.googlecode.com/hg/slide/lex.html
;;;

(require racket/async-channel
         (only-in parser-tools/lex
                  position-token)
         (only-in srfi/13 string-prefix?)
         "../declare/declare.rkt")

;;;
;;; String utility
;;;

(define (char-in-string? c str) 
  (for/or ([s (in-string str)]) 
    (eqv? c s)))

;;;
;;; TYPES
;;;

(define-struct token (type val start end) #:transparent)

(define-struct lexer 
  (name   ; string     // used only for error reports.
   input  ; string     // the string being scanned.
   start  ; int        // start position of this item.
   pos    ; int        // current position in the input.
   width  ; int        // width of last rune read from input.
   tokens ; chan token // channel of scanned tokens.   
   )#:mutable #:transparent)

;;;
;;; Starting the lexer
;;; 

; A lexer initializes itself to lex a string and launches the state 
; machine as a goroutine, returning the lexer itself and a channel 
; of items.

(define (lex name input)
  (define tokens (make-async-channel))
  (define l (lexer name input 0 0 0 tokens))
  (thread (λ () (run l)))
  (values l tokens))

;;; 
;;; Running the lexer
;;;

; Run the lexer l until the state becomes #f.
(define (run l)
  (declare ([l lexer])
    (let loop ([state lex-start])
      ; (displayln (list 'run: 'state state))
      (when state
        (loop (state l))))
    (async-channel-put l.tokens 'eof)))

;;;
;;;
;;;

(define left-meta  "{{")
(define right-meta "}}")

;;;
;;; Lexer states
;;;

; A lexer state is a function from a lexer to a lexer state.

(define (lex-start l)
  ; start in text mode
  lex-text)


(define (lex-text l)
  ; stay in text mode until {{ is seen
  (declare ([l lexer])
    (let/ec return
      (let loop ()
        (when (string-prefix? left-meta (substring l.input l.pos))
          (when (> l.pos l.start)
            (emit l 'text)
            (return lex-left-meta)))
        (unless (eq? (next l) 'eof)
          (loop)))
      ; no left-meta was seen
      (when (> l.pos l.start)
        (emit l 'text))
      (emit l 'eof)
      #f)))

(define (lex-left-meta l)
  (declare ([l lexer])
    (l.pos! (+ l.pos (string-length left-meta)))
    (emit l 'left-meta)
    ; now inside {{ }}
    lex-inside-action))

(define (lex-right-meta l)
  (declare ([l lexer])
    (l.pos! (+ l.pos (string-length right-meta)))
    (emit l 'right-meta)
    ; now outside {{ }}
    lex-text))

(define (lex-inside-action l)  
  (declare ([l lexer])
    ; (displayln (list 'lex-inside-action 'l l))
    (let loop ()
      (cond
        [(string-prefix? right-meta (substring l.input l.pos))
         (emit l 'inside-meta)
         lex-right-meta]
        [else
         (define c (next l))
         (cond 
           [(or (eq? c 'eof) (eqv? c #\newline))
            (lex-error l "unclosed action")]
           [else (loop)])]))))

;;;
;;; Utils
;;;

(define (next l)
  ; return next character in input
  (declare ([l lexer])
    (cond
      [(>= l.pos (string-length l.input))
       (l.width! 0)
       'eof]
      [else
       (define ch (string-ref l.input l.pos))
       (l.width! 1)
       (l.pos!++)
       ch])))

(define (ignore l)
  (declare ([l lexer])
    (l.start! l.pos)))

(define (backup l)
  ; Can be called only once per call of next.
  (declare ([l lexer])
    (l.pos!+= (- l.width))))

(define (peek l)
  ; peek returns but does not consume
  ; the next rune in the input.
  (declare ([l lexer])
    (define ch (next l))
    (backup l)
    ch))

(define (accept l valid)
  ; accept consumes the next rune
  ; if it's from the valid set.
  (declare ([l lexer])
    (or (char-in-string? (next l) valid)
        (begin
          (backup l)
          #f))))

(define (accept-run l valid)
  ; acceptRun consumes a run of runes from the valid set.
  (let loop ()
    ; (displayln l)
    (if (char-in-string? (next l) valid)
        (loop)
        (begin (backup l) #f))))

;;;
;;; Emitting
;;;

; emits the current lexeme from the lexer l
; and gives it the type t
(define (emit l t)
  (declare ([l lexer])
    (match l
      [(lexer name input start pos width tokens)
       ; (displayln (list 'emit: (token t (substring input start pos))))
       (async-channel-put 
        tokens (token t (substring input start pos) start pos))
       (l.start! pos)])))

;;;
;;; Errors
;;;

(define (lex-error l fmt . args)
  (declare ([l lexer])
    ; error returns an error token and terminates the scan
    ; by passing back a nil pointer that will be the next
    ; state, terminating l.run.
    (async-channel-put l.tokens
                       (token 'error (apply format fmt args)
                              l.start (string-length l.input)))
    #f))

;;;
;;; Example
;;;

(define-values (l ts) 
  (lex "Example" "This just some random text.{{Here is the action!}}Back to text again."))
; lex starts the lexer in a seperate thread.
; The "parser" can get the produced tokens from the channel ts.

; "Parser"
(let loop ()
  (define t (async-channel-get ts))
  (unless (eq? t 'eof)
    (write t)
    (newline)
    (loop)))
