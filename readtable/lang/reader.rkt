(module readtable racket/base
  (require syntax/module-reader
           "converter.rkt"
           "read-and-read-syntax.rkt"
           "parse-reader-spec.rkt")
  
  (provide (rename-out [modified-read read]
                       [modified-read-syntax read-syntax]                       
                       ; [modified-get-info get-info]
                       )
                       get-info
                       )
  
  (define (get-info in modpath c d e)
    (displayln (list 'readtable-get-info in modpath c d e))
    (error)
    (λ (key defval) 
         (displayln (list 'modified-get-info 'key/val key defval))
         (case key
           [(color-lexer)
            (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)]
           [(configure-runtime) 
            '(#(racket/runtime-config configure #f))]
           [else             
            defval
            #;(if proc (proc key defval) defval)]))
    #;(modified-get-info in "racket/language-info" c d e))
  
  (define ((wrap-reader more) p)
    (displayln (list 'wrap-reader 'more more))
    (λ args
      (parameterize ([current-readtable (current-modified-readtable)])
        (syntax-property 
         (apply p args) 'module-language 
         ; #(racket/language-info get-info #f)
         #`#(readtable/lang/reader get-info #,more #,more #,more #,more #,more)
         ))))
  
  (define (skip-whitespace port)
    (let loop ()
      (define ch (peek-char port))
      (unless (eof-object? ch)
        (when (char-whitespace? ch)
          (read-char port)
          (loop)))))
  
  (define-values (modified-read 
                  modified-read-syntax
                  modified-get-info)
    (make-meta-reader
     'readtable ; self-sym
     "language path" ; path-desc-str     
     (λ (bstr)  ; module path parser
       (displayln (list 'bstr bstr))
       (define str (bytes->string/latin-1 bstr))
       (define sym (string->symbol str))       
       (and (module-path? sym) ; path to reader whose read and 
            (vector            ; read-syntax is to be used
             ;; try submod first:
             `(submod ,sym reader)
             ;; fall back to /lang/reader:
             (string->symbol (string-append str "/lang/reader")))))
     (wrap-reader 'foo)    ; convert read
     (wrap-reader 'bar)    ; convert read-syntax
     (λ (proc)             ; convert get-info
       (displayln (list 'modified-get-info 'proc proc))
       (λ (key defval) 
         (displayln (list 'modified-get-info 'key/val key defval))
         (case key
           [(color-lexer)
            (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)]
           [(configure-runtime) 
            '(#(racket/runtime-config configure #f))]
           [else             
            (if proc (proc key defval) defval)])))
     #:read-spec
     (λ (port) 
       (read-line port) ; skip everything after #lang readtable
       (define specs
         (for/list ([spec-no (in-naturals)]
                    #:break (equal? (peek-string 5 0 port) "#lang"))
           (parse-spec-line (read-line port))))
       (current-specs specs)       
       (current-modified-readtable (make-modified-readtable specs))
       (skip-whitespace port)
       (for ([c "#lang"]) (read-char port)) ; skip #lang
       (skip-whitespace port)
       (read-bytes-line port)))))
