#!/usr/bin/env mzscheme
#lang racket

(require (lib "1.ss" "srfi")
         (lib "13.ss" "srfi")
         (lib "14.ss" "srfi")
         (lib "cmdline.ss")
         scheme/match)

;;;;;;;;;;;;;; helper defines to make the verbose output clearer

(define syn-indent-cnt 0)
(define (syn-indent-cnt-same)
  (make-string syn-indent-cnt #\space))
(define (syn-indent-cnt-up)
  (begin0 (make-string syn-indent-cnt #\space) (set! syn-indent-cnt (+ 1 syn-indent-cnt))))
(define (syn-indent-cnt-dn)
  (begin (set! syn-indent-cnt (- syn-indent-cnt 1)) (make-string syn-indent-cnt #\space)))
(define (printi fmt . args)
  (printf "~a~a" (syn-indent-cnt-same) (apply format fmt args)))
(define (printi-up fmt . args)
  (when (*verbose*) (printf "~a~a" (syn-indent-cnt-up) (apply format fmt args))))
(define (printi-dn fmt . args)
  (when (*verbose*) (printf "~a~a" (syn-indent-cnt-dn) (apply format fmt args))))

(define pat-indent-cnt 0)
(define (pat-indent-cnt-same)
  (make-string pat-indent-cnt #\space))
(define (pat-indent-cnt-up)
  (begin0 (make-string pat-indent-cnt #\space) (set! pat-indent-cnt (+ 1 pat-indent-cnt))))
(define (pat-indent-cnt-dn)
  (begin (set! pat-indent-cnt (- pat-indent-cnt 1)) (make-string pat-indent-cnt #\space)))
(define (printip fmt . args)
  (printf "~a~a" (pat-indent-cnt-same) (apply format fmt args)))
(define (printip-up fmt . args)
  (when (*verbosep*) (printf "~a~a" (pat-indent-cnt-up) (apply format fmt args))))
(define (printip-dn fmt . args)
  (when (*verbosep*) (printf "~a~a" (pat-indent-cnt-dn) (apply format fmt args))))


;;;;;;;;;;;;;; structs

(define-struct term (file start-pos end-pos name vals)
  #:property prop:custom-write (lambda (t port write?)
                                 (if write? 
                                     (begin
                                       (write-string "(mt " port)
                                       (write (term-name t) port)
                                       (let ([l (term-vals t)])
                                         (unless (null? l)
                                           (write-string " " port)
                                           (write (car l) port)
                                           (for-each (lambda (e)
                                                       (write-string " " port)
                                                       ((if write? write display) e port))
                                                     (cdr l))))
                                       (write-string ")" port))
                                     (begin
                                       (write-string "[" port)
                                       (display (term-name t) port)
                                       (let ([l (term-vals t)])
                                         (unless (null? l)
                                           (write-string " : " port)
                                           (display (car l) port)
                                           (for-each (lambda (e)
                                                       (write-string " " port)
                                                       ((if write? write display) e port))
                                                     (cdr l))))
                                       (write-string "]" port)))))

(define-struct token (file start-pos end-pos chars)
  #:property prop:custom-write (lambda (t port write?)
                                 (if write? 
                                     (write (token-chars t) port)
                                     (begin
                                       (write-string "#\"" port)
                                       (display (token-chars t) port)
                                       (write-string "\"" port)))))

(define (term-equal? fst snd)
  (or (and (term? fst) (term? snd) 
           (token-equal? (term-name fst) (term-name snd))
           (eq? (length (term-vals fst)) (length (term-vals snd)))
           (andmap term-equal? (term-vals fst) (term-vals snd))) 
      (token-equal? fst snd)))

(define (token-equal? fst snd)
  (let ([fst-str (if (token? fst) (token-chars fst) fst)]
        [snd-str (if (token? snd) (token-chars snd) snd)])
    (equal? fst-str snd-str)))

(define (mt name . vals)
  (make-term '() '() '() name vals))

(define-struct memo ((post #:mutable) (taken #:mutable) (left-rec #:mutable)))

(define-struct language ((files #:mutable) (patterns #:mutable) (rules #:mutable) (choices #:mutable)))

(define (grown-rec? l1 l2)
  (if (not l1) #f
      (if (not l2) #t
          (> l1 l2))))


(define (any->symbol sym)
  (if (symbol? sym) sym (string->symbol (any->string sym))))


(define (any->string sym)
  (match sym 
    [(? string? sym) sym]
    [(? token? sym) (token-chars sym)]
    [(term _ _ _ "string" (list s)) (any->string s)]))

;;;;;;;;; grammar hash table ;;;;;;;;;;;;;;
(define (rule-exists? sym lang)
  (hash-has-key? (language-rules lang) (any->symbol sym)))

(define (rule-get sym lang)
  (hash-ref (language-rules lang) (any->symbol sym) (lambda () (raise-user-error 'forest "nonterminal \"~s\" not found~n" sym))))

(define (rule-put! sym expr lang)
  (hash-set! (language-rules lang) (any->symbol sym) expr))

;;;;;;;;; pattern hash table ;;;;;;;;;;;;;;
(define (pattern-ref name lang)
  (hash-ref (language-patterns lang) (any->symbol name) '()))

(define (pattern-put! name lst lang)
  (hash-set! (language-patterns lang) (any->symbol name) lst))


;;;;;;; initial grammar auto-generated
(define (load-core-lang lang) 
  (set-language-files! lang (cons "core" (language-files lang)))
  
  (rule-put! 'sexpr (mt "//" 
                        (mt "@" (mt "string" "null") (mt "name" "null"))
                        (mt "@" (mt "string" "^>") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "^>") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "^=") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "^=") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "^<") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "^<") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace"))) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "$") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "$") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr") (mt "name" "sexprs")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" ">") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" ">") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "@") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "@") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr") (mt "name" "sexprs")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "<") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "<") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "!") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "!") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "/") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "/") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexprs")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "//") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "//") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexprs")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "+") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "+") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexprs")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "term") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "term") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "name") (mt "name" "sexprs")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "import") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "import") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "stringliteral")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "newname") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "newname") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace"))) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "insert") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "insert") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "name") (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "rule") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "rule") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "name") (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "unexpanded") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "unexpanded") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "name" "sexpr")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "pattern") (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "+" (mt "+" (mt "string" "syntaxpattern") (mt "!" (mt "name" "namechar"))) (mt "name" "whitespace")) (mt "@" (mt "string" "unexpanded") (mt "name" "sexpr")) (mt "@" (mt "string" "unexpanded") (mt "name" "sexpr"))) (mt "+" (mt "string" ")") (mt "name" "whitespace"))))
                        (mt "@" (mt "string" "string") (mt "name" "stringliteral"))
                        (mt "name" "name")) lang)
  (rule-put! 'declarations (mt "/" (mt "+" (mt "name" "declaration") (mt "name" "declarations")) (mt "null")) lang)
  (rule-put! 'stringliteral (mt "+" (mt "+" (mt "name" "doublequotechar") (mt "<" (mt "name" "stringchars")) (mt "name" "doublequotechar")) (mt "name" "whitespace")) lang)
  (rule-put! 'newline (mt "/" (mt "+" (mt "name" "returnchar") (mt "name" "linefeedchar")) (mt "name" "returnchar") (mt "name" "linefeedchar")) lang)
  (rule-put! 'stringchars (mt "/" (mt "+" (mt "!" (mt "name" "doublequotechar")) (mt "name" "anychar") (mt "name" "stringchars")) (mt "null")) lang)
  (rule-put! 'linecomment (mt "+" (mt "string" ";") (mt "name" "commentchars") (mt "name" "newline")) lang)
  (rule-put! 'start (mt "+" (mt "name" "whitespace") (mt "name" "declarations") (mt "name" "endoffile")) lang)
  (rule-put! 'endoffile (mt "!" (mt "name" "anychar")) lang)
  (rule-put! 'namechars (mt "/" (mt "+" (mt "name" "namechar") (mt "name" "namechars")) (mt "null")) lang)
  (rule-put! 'declaration (mt ">" (mt "name" "sexpr")) lang)
  (rule-put! 'name (mt "//" (mt "@" (mt "string" "var") (mt "+" (mt "+" (mt "string" "'") (mt "name" "whitespace")) (mt "name" "nameliteral"))) (mt "@" (mt "string" "varlist") (mt "+" (mt "+" (mt "string" "'''") (mt "name" "whitespace")) (mt "name" "nameliteral"))) (mt "@" (mt "string" "name") (mt "name" "nameliteral"))) lang)
  (rule-put! 'null (mt "+" (mt "+" (mt "string" "(") (mt "name" "whitespace")) (mt "+" (mt "string" ")") (mt "name" "whitespace"))) lang)
  (rule-put! 'nameliteral (mt "+" (mt "<" (mt "+" (mt "name" "namechar") (mt "name" "namechars"))) (mt "name" "whitespace")) lang)
  (rule-put! 'commentchars (mt "/" (mt "+" (mt "!" (mt "name" "newline")) (mt "name" "anychar") (mt "name" "commentchars")) (mt "null")) lang)
  (rule-put! 'sexprs (mt "/" (mt "+" (mt "name" "sexpr") (mt "name" "sexprs")) (mt "null")) lang)
  (rule-put! 'namechar (mt "/" (mt "name" "letterchar") (mt "name" "digitchar") (mt "string" "$") (mt "string" "+") (mt "string" "<") (mt "string" "=") (mt "string" ">") (mt "string" "^") (mt "string" "`") (mt "string" "|") (mt "string" "~") (mt "string" "_") (mt "string" "!") (mt "string" "%") (mt "string" "&") (mt "string" "*") (mt "string" "-") (mt "string" "+") (mt "string" "?") (mt "string" ":") (mt "string" "/") (mt "string" "@") (mt "string" ".")) lang)
  (rule-put! 'whitespace (mt "/" (mt "+" (mt "/" (mt "name" "whitespacechar") (mt "name" "linecomment")) (mt "name" "whitespace")) (mt "null")) lang))

(define lang-num 0)
; init


;;;;;;;;;;; initial language definitions. must be after the struct definitions

; initial character and charset bindings
(define *init-rules* (list
    (cons 'lowercasechar   char-set:lower-case  )
    (cons 'uppercasechar   char-set:upper-case  )
    (cons 'letterchar      char-set:letter      )
    (cons 'digitchar       char-set:digit       )
    (cons 'whitespacechar  char-set:whitespace  )
    (cons 'hexdigitchar    char-set:hex-digit   )
    (cons 'blankchar       char-set:blank       )
    (cons 'asciichar       char-set:ascii       )
    (cons 'anychar         char-set:full        )
    
    (cons 'tabchar         #\tab      )
    (cons 'linefeedchar    #\linefeed )
    (cons 'returnchar      #\return   )
    (cons 'backslashchar   #\\        )
    (cons 'singlequotechar #\'        )
    (cons 'doublequotechar #\"        )))


; initial procedure patterns
(define *init-patterns* (list
                         (cons 'pattern (list (cons (mt "pattern" (mt "var" "pat") (mt "var" "repl")) 
                                                    (match-lambda*
                                                      [(list read-lang lang (cons _ repl) (cons _ pat)) 
                                                       (let ([name (term-name pat)])
                                                         (when (*verbosep*) (printip "add Pattern to ~a : ~a => ~a~n" name pat repl))
                                                         (pattern-put! name (cons (cons pat repl) (pattern-ref name lang)) lang)
                                                         (mt "name" name))]))))
                         (cons 'rule (list (cons (mt "rule" (mt "name" (mt "varlist" "names")) (mt "var" "rule")) 
                                                 (match-lambda*
                                                   [(list read-lang lang (cons _ rule) (cons _ names)) 
                                                    (let ([name (string-append* (map any->string names))]) (rule-put! name rule lang) (mt "name" name))])))) 
                         (cons 'newname (list (cons (mt "newname") 
                                                    (match-lambda*
                                                      [(list read-lang lang) (mt "name" (symbol->string (gensym)))]))))  
                         (cons 'import (list (cons (mt "import" (mt "var" "fil")) 
                                                   (match-lambda*
                                                     [(list read-lang lang (cons _ fil)) 
                                                      (let ([f (any->string fil)])
                                                        (when (*verbosep*) (printip "Import ~s~n" f))
                                                        (with-handlers 
                                                            ([exn:fail? (lambda (exn)
                                                                          (eprintf (make-errormessage (exn-message exn) (token-file fil) (token-start-pos fil) (token-end-pos fil))))])
                                                          (read-grammar (string->path f) read-lang)))
                                                      (mt "null")]))))
                         (cons 'insert (list (cons (mt "insert" (mt "name" (mt "varlist" "names")) (mt "var" "rule"))  
                                                   (match-lambda*
                                                     [(list read-lang lang (cons _ rule) (cons _ names)) 
                                                      (let ([name (string-append* (map any->string names))])
                                                        (if (rule-exists? name lang) 
                                                            (let ([term (rule-get name lang)])
                                                              (when (not (and (term? term) (token-equal? (term-name term) "//")))
                                                                (error 'Insert "insertion in something other than // : ~s -- ~s" name rule))
                                                              (when (*verbosep*) (printip "Insert ~s : ~a == ~a~n" name rule term))
                                                              (rule-put! name (apply mt (term-name term) (lset-union! term-equal? (list rule) (term-vals term))) lang)
                                                              rule)
                                                            (begin (rule-put! name (mt "//" rule) lang) rule)))]))))
                         (cons 'term (list (cons (mt "term" (mt "var" "var") (mt "varlist" "vals")) 
                                                 (match-lambda*
                                                   [(list read-lang lang (cons _ vals) (cons _ var)) 
                                                    (let* ([new-term (term '() '() '() var vals)])
                                                      (when (*verbosep*) (printip "Creating variable term ~a~n" new-term))
                                                      new-term)]))
                                           (cons (mt "term" (mt "name" (mt "varlist" "names")) (mt "varlist" "vals")) 
                                                 (match-lambda*
                                                   [(list read-lang lang (cons _ vals) (cons _ names)) 
                                                    (let* ([name (string-append* (map any->string names))]
                                                           [new-term (term '() '() '() (if (token? name) (token-chars name) name) vals)])
                                                      (when (*verbosep*) (printip "Creating term ~a~n" new-term))
                                                      new-term)]))))
                         (cons 'token (list (cons (mt "token" (mt "var" "chars")) 
                                                  (match-lambda*
                                                    [(list read-lang lang (cons _ tok)) 
                                                     (unless (token? tok) (raise-user-error 'Token "Content of Token term must be a token, found ~a" tok))
                                                     (when (*verbosep*) (printip "Creating token ~a~n" tok))
                                                     tok]))))))

(define (expand-term trm lang modify-lang)
  (cond [(or (token? trm) (string? trm)) trm]
        [(not (term? trm)) (raise-user-error 'expand-term "called with something other than a term or token: ~s" trm)]
        [(term? (term-name trm)) (when (*verbosep*) (printip "var-term ~a~n" trm)) trm]
        [(equal? (term-name trm) "unexpanded") (car (term-vals trm))]
        [else 
         (let pat-loop ([pats [reverse (pattern-ref (term-name trm) lang)]])
           (if (null? pats) 
               (make-term (term-file trm) (term-start-pos trm) (term-end-pos trm) (term-name trm) 
                          (map (lambda (t) (expand-term t lang modify-lang)) (term-vals trm)))
               (let ([bnd (let match-loop ([bindings null]
                                           [pattern (car (car pats))]
                                           [trm trm])
                            (when (*verbosep*) (printip-up "Matching ~a on ~a...~n" pattern trm))
                            (let ([res (match pattern 
                                         [(term _ _ _ "var" (list v)) (cons (cons v trm) bindings)]
                                         [(term _ _ _ n (list-rest r ... (list (term _ _ _ "varlist" (list v))))) 
                                          (if (and (term? trm) (>= (length (term-vals trm)) (length r)))
                                              (let* ([b (match n 
                                                          [(term _ _ _ "var" (list v)) (match-loop bindings (term-name pattern) (term-name trm))]
                                                          [_ (if (token-equal? (term-name trm) n) bindings #f)])]
                                                     [bnd (foldl (lambda (pat val b) (if b (match-loop b pat val) #f)) b r (take (term-vals trm) (length r)))])
                                                (if bnd (cons (cons v (drop (term-vals trm) (length r))) bnd) #f)) #f)]
                                         [(term _ _ _ n r) (if (and (term? trm) (eq? (length r) (length (term-vals trm))))
                                                               (let ([b (match n 
                                                                          [(term _ _ _ "var" (list v)) (match-loop bindings (term-name pattern) (term-name trm))]
                                                                          [_ (if (token-equal? (term-name trm) n) bindings #f)])])
                                                                 (foldl (lambda (pat val b) (if b (match-loop b pat val) #f)) b r (term-vals trm))) #f)]
                                         [(token _ _ _ c) (when (*verbosep*) (printip "Matching token ~a with ~a~n" pattern trm))
                                                          (if (token-equal? pattern trm) bindings #f)]
                                         [v (printf "got into notypeland with ~s!!~n" v) (if (eq? v trm) bindings #f)])])
                              (when (*verbosep*) (printip-dn "Matched ~a : ~a on ~a...~n" res pattern trm))
                              res))])
                 (if bnd (begin (when (*verbosep*) (printip-up "Matched pattern ~a on ~a using ~a~n" (car (car pats)) trm bnd))
                                (let* ([bnd (map (match-lambda [(cons n (list trms ...)) (cons n (map (lambda (t) (expand-term t lang modify-lang)) trms))]
                                                               [(cons n t) (cons n (expand-term t lang modify-lang))]) bnd)]
                                       [res 
                                        (expand-term (let replace-loop ([repl (cdr [car pats])])
                                                       (match repl 
                                                         [(? procedure?) (apply repl lang modify-lang bnd)]
                                                         [(term _ _ _ "var" (list v)) 
                                                          (let ([val (assoc v bnd token-equal?) ])
                                                            (if (not val) (eprintf (make-errormessage (format "Variable ~a not found in pattern definition '~a'~n" 
                                                                                                              (token-chars v) (term-name trm)) (token-file v) (token-start-pos v) (token-end-pos v))) 
                                                                (cdr val)))]
                                                         [(term l1 l2 l3 n r) (term l1 l2 l3 (replace-loop n) (foldr (match-lambda** [((term _ _ _ "varlist" (list v)) r) 
                                                                                                                                      (append (cdr (assoc v bnd token-equal?)) r)]
                                                                                                                                     [(v r) (cons (replace-loop v) r)]) '() r))]
                                                         [v v])) lang modify-lang)])
                                  (when (*verbosep*) (printip-dn "Replaced ~a with ~a~n" trm res))
                                  res)) 
                     (pat-loop (cdr pats))))))]))


(define (make-lang files patterns rules)
  (let ((lang (make-language files (make-hash) (make-hash) (make-hash))))
    
    (for-each (lambda (a) (rule-put! (car a) (cdr a) lang)) *init-rules*)
    (for-each (lambda (a) (pattern-put! (car a) (cdr a) lang)) *init-patterns*)
    
    (for-each (lambda (a) (rule-put! (car a) (cdr a) lang)) rules)
    (for-each (lambda (a) (pattern-put! (car a) (cdr a) lang)) patterns)
    lang))

(define (get-lang-for-reading infile)
  (let ([lang (make-lang '() '() '())]
        [grammar-file (find-grammar-for-ext infile)])
    (if grammar-file 
        (if (not (equal? infile grammar-file)) (read-grammar grammar-file lang) (load-core-lang lang))
        (eprintf "No grammar definition found for file ~a~n" infile))
    lang))

(define *ext-dir* "ext")
(define *ext-dir-list* (directory-list *ext-dir*))
(define (find-grammar-for-ext infile)
  (let* ([ext (bytes->string/latin-1 (filename-extension infile))]
         [lst (filter (lambda (fil) (equal? ext (last (drop-right (regexp-split #rx"\\." fil) 1)))) 
                      *ext-dir-list*)])
    (if (member infile lst) infile
      (let ((grammars (filter find-grammar-for-ext lst)))
          (if (pair? grammars) (build-path *ext-dir* (car grammars)) #f)))))

(define (prepare-language-choices exp lang) 
  (define (charset-to-try exp syms)
    (match exp
      [(? char?) (char-set exp)]
      [(? char-set?) exp]
      
      [(term _ _ _ "null" (list)) char-set:full]
      [(term _ _ _ "name" (list names ...)) 
       (let ([exp (string-append* (map any->string names))]) 
         (if (member exp syms) char-set:full (charset-to-try (rule-get exp lang) (cons exp syms))))]
      
      [(term _ _ _ "string" (list exp)) (char-set (string-ref (any->string exp) 0))]
      [(term _ _ _ "+" args) ; concatenation
       (let skip-not ([args args])
         (match (car args)
           [(term _ _ _ "!" (list arg)) (skip-not (cdr args))]
           [_ (if (null? args) char-set:full (charset-to-try (car args) syms))]))] ; go through all args so we will find all choice terms
      [(or (term _ _ _ "/" args) (term _ _ _ "//" args)) ; choice or parallel choice
       (apply char-set-union (map (lambda (exp) (charset-to-try exp syms)) args))]
      [(term _ _ _ "!" (list arg)) ; not
       char-set:full]
      [(term _ _ _ "<" (list arg)) ; capture literal
       (charset-to-try arg syms)]
      [(term _ _ _ "@" (list name arg)) ; gather named term
       (charset-to-try arg syms)]
      [(term _ _ _ ">" (list arg)) ; output
       (charset-to-try arg syms)]
      [(term _ _ _ "$" (list-rest arg mess)) ; parsing error : print mess when arg fails
       char-set:full]
      [(term _ _ _ "^>" (list arg)) ; indent
       (charset-to-try arg syms)]
      [(term _ _ _ "^=" (list arg)) ; same indentation
       (charset-to-try arg syms)]
      [(term _ _ _ "^<" (list)) ; dedent
       char-set:full]          
      
      [(term _ _ _ name (list arg)) ; anything else
       (raise-user-error 'parse "Term \"~a\" is not a valid parser action. Perhaps a Pattern of that name is not defined correctly in imported grammars.~nExpression was \"~a\"~n" name exp)]
      [_ (raise-user-error 'parse "Unknown or illegal parser action \"~a\"~n This error should never be reported. (You found a bug!)~n" exp)]))
  
  (define (prepare-loop exp syms)
    (match exp
      [(term _ _ _ "name" (list names ...)) 
       (let ([name (string-append* (map any->string names))]) 
         (if (member name syms) syms (prepare-loop (rule-get name lang) (cons name syms))))]      
      [(term _ _ _ "+" args) ; concatenation
       (foldr (lambda (exp syms) (prepare-loop exp syms)) syms args)] ; go through all args so we will find all choice terms
      [(or (term _ _ _ "/" args) (term _ _ _ "//" args)) ; choice or parallel choice
       (foldr (lambda (exp syms) 
                (hash-set! (language-choices lang) exp (charset-to-try exp null))
                (prepare-loop exp syms)) syms args)]
      [(or (term _ _ _ "!" (list arg)) (term _ _ _ "<" (list arg)) (term _ _ _ "@" (list _ arg)) (term _ _ _ ">" (list arg)) 
           (term _ _ _ "$" (list-rest arg _)) (term _ _ _ "^>" (list arg)) (term _ _ _ "^=" (list arg)))
       (prepare-loop arg syms)]          
      
      [_ syms]))
  
  (prepare-loop exp null))

(define (file-more-recent fil1 fil2)
  (and (file-exists? fil1) (file-exists? fil2) 
       (> (file-or-directory-modify-seconds fil1) (file-or-directory-modify-seconds fil2))))

;;;;;;;;;;;; write cached languages

(define (write-language t port . prefs)
  (define (list-body ls)
    (cond ((pair? ls) (apply write-language (car ls) port (cons "  " prefs)) 
                      (when (pair? (cdr ls)) (newline port) (display (apply string-append prefs) port))
                      (list-body (cdr ls)))
          (else ls)))
  (define (term-body ls)
    (cond ((pair? ls) (apply write-language (car ls) port prefs) 
                      (when (pair? (cdr ls)) (display " " port))
                      (term-body (cdr ls)))
          (else ls)))
  (cond 
    ((language? t) 
     (display (format "(make-lang ~s~n" (cons 'list (language-files t))) port)
     (write-language   
      (filter identity (hash-map (language-patterns t) 
                                 (lambda (k v)
                                   (let ((ikv (assoc k *init-patterns*)))
                                     (if (and ikv (eq? (cdr ikv) v)) #f (cons k v)))))) port "  ")
     (newline port)
     (write-language
      (filter identity (hash-map (language-rules t) 
                                 (lambda (k v)
                                   (let ((ikv (assoc k *init-rules*)))
                                     (if (and ikv (eq? (cdr ikv) v)) #f (cons k v)))))) port "  ")
     (display ")" port))
    ((term? t)
     (display "(mt " port)
     (write (term-name t) port)
     (display " " port)
     (term-body (term-vals t))
     (display ")" port))
    ((token? t)
     (write (token-chars t) port))
    ((string? t) (write t port))
    ((symbol? t) (display "'" port) (write t port))
    ((char-set? t) (display "{" port) (char-set-for-each (lambda (c) (write c port) (display " " port)) t) (display "}" port))
    ((proper-list? t)   
     (display "(list " port)
     (list-body t)
     (display ")" port))
    ((pair? t) 
     (display "(cons " port)
     (write-language (car t) port)  
     (display " " port)
     (write-language (cdr t) port)
     (display ")" port))
    (else (display t port))))



(define *lang-cache* (make-hash))

(define *file-cache* (make-hash))

(define *cache-dir* "cache/")

(define (read-grammar infile modify-lang)
  (when (or (*verbose*) (*verbosep*)) (printf "reading grammar ~a~n" infile))
  (let* ((files (cons (path->string infile) (language-files modify-lang)))
         (cache-file-name (string-append *cache-dir* (string-join (map (lambda (f) (regexp-replace "/" f "_")) files) ",") ".scm")))
    (if (and *cache* (every (lambda (f) (file-more-recent cache-file-name (search-file f))) files))
        (let ((cached-lang (call-with-input-file cache-file-name (lambda (port) (eval (read port) *ns*))))) 
          (set-language-files! modify-lang files)
          (set-language-patterns! modify-lang (language-patterns cached-lang))
          (set-language-rules! modify-lang (language-rules cached-lang))
          (set-language-choices! modify-lang (language-choices cached-lang))) 
        (parameterize ((*verbose* #f)
                       (*verbosep* #f))
          (parse-file infile (lambda (read-lang terms) (expand-term (car terms) read-lang modify-lang)))
          (set-language-files! modify-lang files)
          (call-with-output-file cache-file-name (lambda (port) (write-language modify-lang port)) #:exists 'truncate)))
    (prepare-language-choices (mt "name" "start") modify-lang)
    
    (when (or (*verbose*) (*verbosep*)) (printf "Done reading grammar ~a: ~a ~n" infile (language-files modify-lang)))))


(define *grammar-paths* (map string->path (list "." "grammars")))

(define (search-file infile)
  (if (file-exists? infile) infile  
      (let ([p (find (lambda (p) (file-exists? (build-path p infile))) *grammar-paths*)])
        (if p (build-path p infile)
            (raise-user-error 'search-file "File not found: ~a~n" infile)))))


(define (make-errormessage message file pos new-pos)
  ; make-kmp-restart-vector
  (define (find-line lines nline nchar)
    (if (> nchar (string-length (car lines))) 
        (find-line (cdr lines) (+ nline 1) (- nchar (string-length (car lines)) 1))
        (values (car lines) nline nchar)))
  (let*-values ([(infile) (search-file file)]
                [(str) (call-with-input-file infile (lambda (in) (bytes->string/latin-1 (read-bytes (file-size infile) in))))]
                [(line nline nchar) (find-line (regexp-split #rx"\n" str) 1 pos)])
    #;(print (regexp-split #rx"\n" str))
    (format "~a:~a: ~a~n~a~n~a~n" infile nline message line (string-append (string-take (regexp-replace* "[^\t ]" line " ") nchar) "^"))))


(define (parse-file infile process-term)
  (when (*verbose*) (printf " parsing ~s ...~n" infile))
  (let* ([infile (search-file infile)]
         [lang (get-lang-for-reading infile)]
         [len (file-size infile)]
         [str (call-with-input-file infile (lambda (in) (bytes->string/latin-1 (port->bytes in))))]
         [memo-hash (make-hasheq)]
         [indent-ls null])
    
    (define (invalidate-caches)
      (set! memo-hash (make-hasheq)))
    
    (define (rule-memo sym)
      (hash-ref memo-hash sym (lambda () (let ([h (make-hash)]) (hash-set! memo-hash sym h) h))))
    
    (define (parse exp pos path-ls)
      (if (*verbose*) 
          (let* ([exp-str (format "~a" exp)]
                 [formatted-exp (if (> (string-length exp-str) 60) (string-append (string-take exp-str 60) " ...") exp-str)])
            (printf "'~a'@~a~a~a~n" (and (< pos len) (string-ref str pos)) pos (syn-indent-cnt-up) formatted-exp)
            (let-values ([(new-pos taken) (parse-inner exp pos path-ls)])
              (printf "-> @~a~a~a : ~a~n" new-pos (syn-indent-cnt-dn) taken formatted-exp)
              (values new-pos taken)))
          (parse-inner exp pos path-ls)))
    
    (define (parse-inner exp pos path-ls)
      (match exp
        [(? char?)
         (values (if (and (< pos len) (char=? exp (string-ref str pos))) (+ pos 1) #f) null)]
        [(? char-set?)
         (values (if (and (< pos len) (char-set-contains? exp (string-ref str pos))) (+ pos 1) #f) null)]
        
        [(term _ _ _ "null" (list)) (values pos null)]
        [(term _ _ _ "name" (list names ...)) 
         (let* ([exp (string->symbol (string-append* (map any->string names)))]
                [rule (rule-get exp lang)]
                [memo (hash-ref (rule-memo exp) pos #f)])
           (if memo
               (if (and (memo-left-rec memo) (not (eq? (memo-left-rec memo) exp))) 
                   (parse rule pos (cons exp path-ls)) 
                   (begin        
                     (if (member exp path-ls) ; left recursion
                         (begin (when (*verbose*) (printi "     start of left recursion ~s~n" exp))
                                (set-memo-left-rec! memo exp)
                                (let path-loop ([path path-ls])
                                  (when (not (eq? (car path) exp))
                                    (set-memo-left-rec! (hash-ref (rule-memo (car path)) pos #f) exp) ; TODO: check this line
                                    (path-loop (cdr path)))))
                         (when (*verbose*) (printi "     cached ~s~n" exp)))
                     (values (memo-post memo) (memo-taken memo))))
               (let ([memo (make-memo #f null #f)])
                 (hash-set! (rule-memo exp) pos memo)
                 (let recurse-loop ()
                   (let*-values ([(post taken) (parse rule pos (cons exp path-ls))])
                     (if (memo-left-rec memo) ; now we're back down in left recursion
                         (begin (when (*verbose*) (printi "   --> ~s Left recursion : ~a, ~a~n" exp post taken))
                                (if (grown-rec? post (memo-post memo))
                                    ; we made progress parsing the left recursion. Save result and try again
                                    (begin (when (*verbose*) (printi "       go again (~a ~a) : ~a~n" post (memo-post memo) (grown-rec? post (memo-post memo))))
                                           (set-memo-post! memo post)
                                           (set-memo-taken! memo taken)
                                           (if (eq? (memo-left-rec memo) exp)
                                               (recurse-loop)
                                               (values post taken)))
                                    ; one recursion too many. return previous values
                                    (begin (when (*verbose*) (printi "       last time (~a ~a) : ~a~n" post (memo-post memo) (grown-rec? post (memo-post memo))))
                                           (set-memo-left-rec! memo #f)
                                           (values (memo-post memo) (memo-taken memo)))))
                         (begin
                           (set-memo-post! memo post)
                           (set-memo-taken! memo taken)
                           (values post taken))))))))]
        [(term _ _ _ "string" (list exp)) 
         (let string-loop ([exp (string->list (any->string exp))] [chls pos])
           (if (null? exp) (values chls null) 
               (let-values ([(new-pos taken) (parse (car exp) chls null)])
                 (if new-pos (string-loop (cdr exp) new-pos) (values #f null)))))]
        [(term _ _ _ "+" args) ; concatenation
         (let concat-loop ([args args] [curr-ls pos] [term null])
           (if (null? args) (values curr-ls (reverse! term))
               (let-values ([(new-pos taken) (parse (car args) curr-ls (if (eq? curr-ls pos) path-ls null))])
                 (if new-pos 
                     (concat-loop (cdr args) new-pos (append-reverse taken term))
                     (values #f null)))))]
        [(or (term _ _ _ "/" args) (term _ _ _ "//" args)) ; choice or parallel choice
         (let ([args (if (< pos len) (filter (lambda exp (char-set-contains? (hash-ref (language-choices lang) exp char-set:full) (string-ref str pos))) args) args)])
           (let choice-loop ([args args])
             (if (null? args) (values #f null)
                 (let-values ([(new-pos taken) (parse (car args) pos path-ls)])
                   (if new-pos (values new-pos taken) 
                       #;(if (equal? (term-name exp) "/") (values new-pos taken)
                             (let ([success-args (filter (lambda (arg) (let-values ([(new-pos taken) (parse arg pos path-ls)])
                                                                         new-pos)) (cdr args))])
                               (unless (null? success-args)
                                 (raise-user-error 'parse "parallel choice matches more than one alternative: \"~a\"~n" 
                                                   (apply string-append (map (lambda (a) (format "~n    ~a" a)) (cons (car args) success-args)))))
                               (values new-pos taken))) (choice-loop (cdr args)))))))]
        [(term _ _ _ "!" (list arg)) ; not
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (values (if new-pos #f pos) null))]
        [(term _ _ _ "<" (list arg)) ; capture literal
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           #;(when new-pos (printf "< ~a ~n" (substring str pos new-pos)))
           (let ([res (list (if new-pos (make-token infile pos new-pos (substring str pos new-pos)) #f))])
             (values new-pos res)))]
        [(or (term _ _ _ "@" (list (term _ _ _ "string" (list name)) arg)) ; gather named term
             (term _ _ _ "@" (list name arg))) 
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (if new-pos
               (let ([new-term (make-term infile pos new-pos (any->string name) taken)])
                 (values new-pos (list new-term)))
               (values new-pos taken)))]
        [(term _ _ _ ">" (list arg)) ; output
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (when new-pos
             (process-term lang taken))
           (values new-pos taken))]
        [(term _ _ _ "$" (list-rest arg mess)) ; parsing error : print mess when arg fails
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (unless new-pos (eprintf (make-errormessage (string-append* (map (match-lambda [(term _ _ _ "string" (list exp)) (any->string exp)]) mess)) infile pos new-pos)))
           (values new-pos taken))]
        [(term _ _ _ "^>" (list arg)) ; take indentation whitespace
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (when new-pos
             (when (*verbose*) (printf "^> ~s : ~s ~s~n" (- new-pos pos) (substring str pos new-pos) (map car indent-ls)))
             (set! indent-ls (cons (list (- new-pos pos) pos) indent-ls)))
           (values new-pos null))]
        [(term _ _ _ "^=" (list arg)) ; match indentation token
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (when (*verbose*) (printf "^= ~s : ~s ~s~n" (- new-pos pos) (substring str pos new-pos) (map car indent-ls)))
           (values (if (and new-pos (eq? (- new-pos pos) (caar indent-ls)))
                       new-pos #f) null))]
        [(term _ _ _ "^<" (list)) ; match indentation token
         (when (*verbose*) (printf "^< ~s -> ~s~n" (caar indent-ls) (caadr indent-ls)))
         (set! indent-ls (cdr indent-ls))
         (invalidate-caches)
         (values pos null)]
        
        [_ (raise-user-error 'parse "unknown or illegal parser action \"~s\"~n" exp)]))
    (with-handlers ([exn:fail:filesystem? 
                     (lambda (exn)
                       (fprintf (current-error-port) "~a~n" (exn-message exn)) #f)]
                    [exn:fail:user? 
                     (lambda (exn)
                       (fprintf (current-error-port) "error parsing ~s: ~a~n" infile (exn-message exn)) #f)]
                    #;[exn:fail? 
                       (lambda (exn)
                         (fprintf (current-error-port) "error parsing ~s: ~a~n" infile (exn-message exn)) #f)])
      (parameterize ([*current-directory* (path-only (simple-form-path infile))])
        (let*-values ([(res taken) (parse (mt "name" "start") 0 null)])
        (unless (and (number? res) (= res len))
          (let ([pos (foldl max 0 (hash-map memo-hash (lambda (k v) (foldl (lambda (kv s) (if (cdr kv) (max (cdr kv) s) s))
                                                                           0 (hash-map v (lambda (k v) (cons k (memo-post v))))))))])
            (fprintf (current-error-port) (make-errormessage "Could not parse. First char not parsed:" infile pos pos)))
          
          #f)))
      )))


;;;; more globals
(define *current-directory* (make-parameter "."))

;;;;;;;; define the namespace to do eval in
(define *ns* (make-base-namespace))
(namespace-set-variable-value! 'make-lang make-lang #t *ns*)
(namespace-set-variable-value! 'mt mt #t *ns*)

;;;;;;;;; main program: command line parsing
(define *verbose* (make-parameter #f))

(define *verbosep* (make-parameter #f))

(define *cache* #t)

(define (main argv)  
  (let ([infiles (command-line "forest" argv
                               (once-each
                                [("-v" "--verbose") "Print verbose messages"
                                                    (*verbose* #t)]
                                [("-p" "--pattern") "Print pattern expansion messages"
                                                    (*verbosep* #t)]
                                [("-c" "--clear") "Clear gramar cache"
                                                  (set! *cache* #f)])                               
                               (args filenames ; expects names of files to as last command-line arguments
                                     filenames)) ; return list of filenames to compile
                 ])
    (exit (if (andmap (lambda (infile)
                        (let ((modify-lang (make-lang '() '() '())))
                          (parse-file infile (lambda (read-lang terms) 
						    (for-each (lambda (term) (printf "> ~a~n" (expand-term term read-lang modify-lang))) terms)))))
                      infiles) 0 1))))

; interactive script
(main (current-command-line-arguments))
