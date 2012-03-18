#!/usr/bin/env mzscheme
#lang racket

(require (lib "1.ss" "srfi")
         (lib "13.ss" "srfi")
         (lib "14.ss" "srfi")
         (lib "cmdline.ss")
         scheme/match)

;(require profile)

(provide main)

;;;;;;;;;;;;;; helper defines to make the verbose output clearer

(define s-e-cnt 0)
(define (s-e-cnt-same)
  (make-string s-e-cnt #\space))
(define (s-e-cnt-up)
  (begin0 (make-string s-e-cnt #\space) (set! s-e-cnt (+ 1 s-e-cnt))))
(define (s-e-cnt-dn)
  (begin (set! s-e-cnt (- s-e-cnt 1)) (make-string s-e-cnt #\space)))
(define (printi fmt . args)
  (printf "~a~a" (s-e-cnt-same) (apply format fmt args)))
(define (printi-up fmt . args)
  (when *verbose* (printf "~a~a" (s-e-cnt-up) (apply format fmt args))))
(define (printi-dn fmt . args)
  (when *verbose* (printf "~a~a" (s-e-cnt-dn) (apply format fmt args))))


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
                                       (write-string "<" port)
                                       (display (term-name t) port)
                                       (let ([l (term-vals t)])
                                         (unless (null? l)
                                           (write-string ": " port)
                                           (display (car l) port)
                                           (for-each (lambda (e)
                                                       (write-string ", " port)
                                                       ((if write? write display) e port))
                                                     (cdr l))))
                                       (write-string ">" port)))))

(define-struct token (file start-pos end-pos chars)
  #:property prop:custom-write (lambda (t port write?)
                                 (if write? 
                                     (write (token-chars t) port)
                                     (begin
                                       (write-string "T\"" port)
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

(define-struct language (name patterns rules choices))

(define (grown-rec? l1 l2)
  (if (not l1) #f
      (if (not l2) #t
          (> l1 l2))))


(define (any->symbol sym)
  (if (symbol? sym) sym (string->symbol (any->string sym))))


(define (any->string sym)
  (cond [(string? sym) sym]
        [(token? sym) (token-chars sym)]))

;;;;;;;;; grammar hash table ;;;;;;;;;;;;;;

(define (rule-get sym lang)
  (hash-ref (language-rules lang) (any->symbol sym) (lambda () (raise-user-error 'forest "nonterminal \"~s\" not found~n" sym))))

(define (rule-put! sym expr lang)
  (hash-set! (language-rules lang) (any->symbol sym) expr))

;;;;;;;;; pattern hash table ;;;;;;;;;;;;;;
(define (pattern-ref name lang)
  (hash-ref (language-patterns lang) (any->symbol name) '()))

(define (pattern-put! name lst lang)
  (hash-set! (language-patterns lang) (any->symbol name) lst))


;;;;;;;;;;;;;;;

(define lang-num 0)
; init
(define (make-initialized-language)
  (set! lang-num (+ lang-num 1))
  (let ([lang (make-language lang-num (make-hasheq) (make-hasheq) (make-hasheq))])
    
    ; initial character and charset bindings
    (rule-put! 'LowerCaseChar   char-set:lower-case  lang)
    (rule-put! 'UpperCaseChar   char-set:upper-case  lang)
    (rule-put! 'LetterChar      char-set:letter      lang)
    (rule-put! 'DigitChar       char-set:digit       lang)
    (rule-put! 'WhitespaceChar  char-set:whitespace  lang)
    (rule-put! 'HexDigitChar    char-set:hex-digit   lang)
    (rule-put! 'BlankChar       char-set:blank       lang)
    (rule-put! 'AsciiChar       char-set:ascii       lang)
    (rule-put! 'AnyChar         char-set:full        lang)
    
    (rule-put! 'TabChar         #\tab      lang)
    (rule-put! 'LinefeedChar    #\linefeed lang)
    (rule-put! 'ReturnChar      #\return   lang)
    (rule-put! 'BackslashChar   #\\        lang)
    (rule-put! 'SingleQuoteChar #\'        lang)
    (rule-put! 'DoubleQuoteChar #\"        lang)
    
    
    
    ;;;;;;; initial grammar auto-generated
    
    (rule-put! 'Name (mt "//" (mt "@" (mt "String" "Var") (mt "+" (mt "+" (mt "String" "'") (mt "Name" "WhiteSpace")) (mt "Name" "NameLiteral")))
                         (mt "@" (mt "String" "VarList") (mt "+" (mt "+" (mt "String" ",") (mt "Name" "WhiteSpace")) (mt "Name" "NameLiteral")))
                         (mt "@" (mt "String" "SpecialVar") (mt "+" (mt "+" (mt "String" "`") (mt "Name" "WhiteSpace")) (mt "Name" "NameLiteral")))
                         (mt "@" (mt "String" "Name") (mt "Name" "NameLiteral"))) lang)
    (rule-put! 'Declaration (mt ">" (mt "Name" "Sexpr")) lang)
    (rule-put! 'NameChars (mt "/" (mt "+" (mt "Name" "NameChar") (mt "Name" "NameChars")) (mt "Null")) lang)
    (rule-put! 'EndOfFile (mt "!" (mt "Name" "AnyChar")) lang)
    (rule-put! 'start (mt "+" (mt "Name" "WhiteSpace") (mt "Name" "Declarations") (mt "Name" "EndOfFile")) lang)
    (rule-put! 'LineComment (mt "+" (mt "String" ";") (mt "Name" "CommentChars") (mt "Name" "Newline")) lang)
    (rule-put! 'StringChars (mt "/" (mt "+" (mt "!" (mt "Name" "DoubleQuoteChar")) (mt "Name" "AnyChar") (mt "Name" "StringChars")) (mt "Null")) lang)
    (rule-put! 'Newline (mt "/" (mt "+" (mt "Name" "ReturnChar") (mt "Name" "LinefeedChar")) (mt "Name" "ReturnChar") (mt "Name" "LinefeedChar")) lang)
    (rule-put! 'StringLiteral (mt "+" (mt "+" (mt "Name" "DoubleQuoteChar") (mt "<" (mt "Name" "StringChars")) (mt "Name" "DoubleQuoteChar")) (mt "Name" "WhiteSpace")) lang)
    (rule-put! 'Declarations (mt "/" (mt "+" (mt "Name" "Declaration") (mt "Name" "Declarations")) (mt "Null")) lang)
    (rule-put! 'Sexpr (mt "//" (mt "Name" "Name")
                          (mt "@" (mt "String" "Null") (mt "Name" "Null"))
                          (mt "@" (mt "String" "$") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "$") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr") (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" ">") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" ">") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "@") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "@") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr") (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "<") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "<") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "!") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "!") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "/") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "/") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "//") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "//") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "+") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "+") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Import") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Import") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "StringLiteral")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "NewName") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "NewName") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace"))) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Insert") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Insert") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Name") (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Rule") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Rule") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Name") (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Pattern") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Pattern") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr") (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "String") (mt "Name" "StringLiteral"))
                          ) lang)
    (rule-put! 'WhiteSpace (mt "/" (mt "+" (mt "/" (mt "Name" "WhitespaceChar") (mt "Name" "LineComment")) (mt "Name" "WhiteSpace")) (mt "Null")) lang)
    (rule-put! 'NameChar (mt "/" (mt "Name" "LetterChar") (mt "Name" "DigitChar") (mt "String" "$") (mt "String" "+") (mt "String" "<") (mt "String" "=") (mt "String" ">") (mt "String" "^") (mt "String" "`") (mt "String" "|") (mt "String" "~") (mt "String" "_") (mt "String" "!") (mt "String" "%") (mt "String" "&") (mt "String" "*") (mt "String" "-") (mt "String" "+") (mt "String" "?") (mt "String" ":") (mt "String" "/") (mt "String" "@") (mt "String" ".")) lang)
    (rule-put! 'Sexprs (mt "/" (mt "+" (mt "Name" "Sexpr") (mt "Name" "Sexprs")) (mt "Null")) lang)
    (rule-put! 'Null (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))) lang)
    (rule-put! 'CommentChars (mt "/" (mt "+" (mt "!" (mt "Name" "Newline")) (mt "Name" "AnyChar") (mt "Name" "CommentChars")) (mt "Null")) lang)
    (rule-put! 'NameLiteral (mt "+" (mt "<" (mt "+" (mt "Name" "NameChar") (mt "Name" "NameChars"))) (mt "Name" "WhiteSpace")) lang)
    
    
    ;;;;;;;; end of initial grammar
    
    (pattern-put! "Pattern" (list (cons (mt "Pattern" (mt "SpecialVar" "pat") 
                                            (mt  "SpecialVar" "repl")) 
                                        (match-lambda*
                                          [(list read-lang lang (cons _ repl) (cons _ pat)) 
                                           (let ([name (term-name pat)])
                                             (pattern-put! name (cons (cons pat repl) (pattern-ref name lang)) lang)
                                             (mt "Name" name))]))) lang)
    (pattern-put! "Rule" (list (cons (mt "Rule" (mt "Name" (mt "Var" "name")) 
                                         (mt "Var" "rule")) 
                                     (match-lambda*
                                       [(list read-lang lang (cons _ rule) (cons _ name)) (rule-put! name rule lang) (mt "Name" name)]))) lang) 
    (pattern-put! "NewName" (list (cons (mt "NewName") 
                                        (match-lambda*
                                          [(list read-lang lang) (mt "Name" (symbol->string (gensym)))]))) lang)  
    (pattern-put! "Import" (list (cons (mt "Import" (mt "Var" "fil")) 
                                       (match-lambda*
                                         [(list read-lang lang (cons _ fil)) 
                                          (let ([f (any->string fil)])
                                            (when *verbose* (printf "Import ~s, ~a ~a~n" f (language-name read-lang) (language-name lang)))
                                            (read-grammar f read-lang))
                                          (mt "Null")]))) lang)
    (pattern-put! "Insert" (list (cons (mt "Insert" (mt "Name" (mt "Var" "name"))
                                           (mt "Var" "rule"))  
                                       (match-lambda*
                                         [(list read-lang lang (cons _ rule) (cons _ name)) 
                                          (let ([term (rule-get name lang)])
                                            (when (not (and (term? term) (token-equal? (term-name term) "//")))
                                              (error 'Insert "insertion in something other than // : ~s -- ~s" name rule))
                                            (when *verbose* (printf "Insert into (lang ~a) ~s : ~a == ~a~n"  (language-name lang) name rule term))
                                            (rule-put! name (apply mt (term-name term) (lset-union! term-equal? (list rule) (term-vals term))) lang)
                                            rule)]))) lang)
    
    lang))


(define (expand-term trm lang modify-lang)
  (printi-up "expand-term ~a~n" trm)
  (let ([res
         (if (not (term? trm)) trm
             (let pat-loop ([pats [reverse (pattern-ref (term-name trm) lang)]])
               (if (not (pair? pats)) (make-term (term-file trm) (term-start-pos trm) (term-end-pos trm) (term-name trm) 
                                                 (map (lambda (t) (expand-term t lang modify-lang)) (term-vals trm)))
                   (let ([bnd (let loop ([bindings null]
                                         [pattern (car (car pats))]
                                         [trm trm])
                                (match pattern 
                                  [(term _ _ _ "Var" (list v)) (cons (cons v (expand-term trm lang modify-lang)) bindings)]
                                  [(term _ _ _ "SpecialVar" (list v)) (cons (cons v trm) bindings)]
                                  [(term _ _ _ n (list-rest r ... (list (term _ _ _ "VarList" (list v)))))
                                   (if (and (term? trm) (equal? (term-name trm) n))
                                       (cons (cons v (drop (term-vals trm) (length r))) (foldl (lambda (pat val b) (if b (loop b pat val) #f)) bindings r (take (term-vals trm) (length r)))) #f)]
                                  [(term _ _ _ n r)
                                   (when (eq? (length r) (term-vals trm)) (printf "Unequal lengths: ~a ~a~n" n r))
                                   (if (and (term? trm) (equal? (term-name trm) n))
                                       (foldl (lambda (pat val b) (if b (loop b pat val) #f)) bindings r (term-vals trm)) #f)]
                                  
                                  [v (if (eq? v trm) bindings #f)]))])
                     (if bnd (expand-term (let loop ([repl (cdr [car pats])])
                                            (match repl 
                                              [(? procedure? ) (apply repl lang modify-lang bnd)]
                                              [(term _ _ _ (or "Var" "SpecialVar") (list v)) (cdr (assoc v bnd token-equal?))]
                                              [(term v1 v2 v3 n (list-rest r ... (list (term _ _ _ "VarList" (list v)))))
                                               (term v1 v2 v3 n (append (map loop r) (cdr (assoc v bnd token-equal?))))]
                                              [(term v1 v2 v3 n r) (term v1 v2 v3 n (map loop r))]
                                              [v v])) lang modify-lang) 
                         (pat-loop (cdr pats)))))))])
    
    (printi-dn "result:~a~n" res)
    res))



; main helper procedures

(define (read-grammar infile lang)
  (let ([verbose *verbose*])
    (when *verbose* (printf "reading grammar ~a~n" infile))
    (set! *verbose* #f)
    (let* ([read-lang (make-initialized-language)]
           [res (parse-file infile read-lang (lambda (terms) (expand-term (car terms) read-lang lang)))])
      (set! *verbose* verbose)
      (when *verbose* (printf "Done reading grammar ~a~n" infile))
      (unless res
        (raise-user-error 'read-grammar "Error reading file ~a" infile)))))

(define (parse-file infile lang process-term)
  
  (when *verbose* (printf " parsing ~s ...~n" infile))
  (let* ([len (file-size infile)]
         [str (call-with-input-file infile (lambda (in) (bytes->string/latin-1 (read-bytes len in))))]
         [memo-hash (make-hasheq)])
    
    (define (rule-memo sym)
      (hash-ref memo-hash sym (lambda () (let  ([h (make-hash)]) (hash-set! memo-hash sym h) h))))
    
    (define (make-errormessage message pos new-pos)
      ; make-kmp-restart-vector
      (define (find-line lines nline nchar)
        (if (> nchar (string-length (car lines))) 
            (find-line (cdr lines) (+ nline 1) (- nchar (string-length (car lines)) 1))
            (values (car lines) nline nchar)))
      (let-values ([(base name must-be-dir?) (split-path infile)]
                   [(line nline nchar) (find-line (regexp-split #rx"\n" str) 1 pos)])
        #;(print (regexp-split #rx"\n" str))
        (format "~a:~a: ~a~n~a~n~a~n" infile nline message line (string-append (string-take (regexp-replace* "[^\t ]" line " ") nchar) "^"))))
    
    (define (must-try-rule? exp ch) 
      (define (try-inner exp ch syms)
        (match exp
          [(? char?)
           (char=? exp ch)]
          [(? char-set?)
           (char-set-contains? exp ch)]
          
          [(term _ _ _ "Null" (list)) #t]
          [(term _ _ _ "Name" (list name)) 
           (let ([exp (any->symbol name)]) 
             (if (member exp syms) #t (try-inner (rule-get exp lang) ch (cons exp syms))))]
          
          [(term _ _ _ "String" (list exp))
           (char=? (string-ref (any->string exp) 0) ch)]
          [(term _ _ _ "+" args) ; concatenation
           (try-inner (car args) ch syms)]
          [(or (term _ _ _ "/" args) (term _ _ _ "//" args)) ; choice or parallel choice
           (ormap (lambda (exp) (try-inner exp ch syms)) args)]
          [(term _ _ _ "!" (list arg)) ; not
           #t]
          [(term _ _ _ "<" (list arg)) ; capture literal
           (try-inner arg ch syms)]
          [(term _ _ _ "@" (list name arg)) ; gather named term
           (try-inner arg ch syms)]
          [(term _ _ _ ">" (list arg)) ; output
           (try-inner arg ch syms)]
          [(term _ _ _ "$" (list-rest arg mess)) ; parsing error : print mess when arg fails
           #t]
          [(term _ _ _ "^" (list arg)) ; take indentation whitespace
           #t]
          [(term _ _ _ "^=" (list arg)) ; match indentation token
           #t]
          
          
          [_ (raise-user-error 'must-try-rule? "unknown or illegal parser action \"~s\"~n" exp)]))
      (try-inner exp ch null))
    
    (define (parse exp pos path-ls)
      (if *verbose* 
          (let* ([exp-str (format "~a" exp)]
                 [formatted-exp (if (> (string-length exp-str) 60) (string-append (string-take exp-str 60) " ...") exp-str)])
            (printf "'~a'@~a~a~a~n" (and (< pos len) (string-ref str pos)) pos (s-e-cnt-up) formatted-exp)
            (let-values ([(new-pos taken) (parse-inner exp pos path-ls)])
              (printf "-> @~a~a~a : ~a~n" new-pos (s-e-cnt-dn) taken formatted-exp)
              (values new-pos taken)))
          (parse-inner exp pos path-ls)))
    
    (define (parse-inner exp pos path-ls)
      (match exp
        [(? char?)
         (values (if (and (< pos len) (char=? exp (string-ref str pos))) (+ pos 1) #f) null)]
        [(? char-set?)
         (values (if (and (< pos len) (char-set-contains? exp (string-ref str pos))) (+ pos 1) #f) null)]
        
        [(term _ _ _ "Null" (list)) (values pos null)]
        [(term _ _ _ "Name" (list name)) 
         (let* ([exp (any->symbol name)]
                [rule (rule-get exp lang)]
                [memo (hash-ref (rule-memo exp) pos #f)])
           (if memo
               (if (and (memo-left-rec memo) (not (eq? (memo-left-rec memo) exp))) 
                   (parse rule pos (cons exp path-ls)) 
                   (begin        
                     (if (member exp path-ls) ; left recursion
                         (begin (when *verbose* (printi "     start of left recursion ~s~n" exp))
                                (set-memo-left-rec! memo exp)
                                (let loop ([path path-ls])
                                  (when (not (eq? (car path) exp))
                                    (set-memo-left-rec! (hash-ref (rule-memo (car path)) pos #f) exp) ; TODO: check this line
                                    (loop (cdr path)))))
                         (when *verbose* (printi "     cached ~s~n" exp)))
                     (values (memo-post memo) (memo-taken memo))))
               (let ([memo (make-memo #f null #f)])
                 (hash-set! (rule-memo exp) pos memo)
                 (let loop ()
                   (let*-values ([(post taken) (parse rule pos (cons exp path-ls))])
                     (if (memo-left-rec memo) ; now we're back down in left recursion
                         (begin (when *verbose* (printi "   --> ~s Left recursion : ~a, ~a~n" exp post taken))
                                (if (grown-rec? post (memo-post memo))
                                    ; we made progress parsing the left recursion. Save result and try again
                                    (begin (when *verbose* (printi "       go again (~a ~a) : ~a~n" post (memo-post memo) (grown-rec? post (memo-post memo))))
                                           (set-memo-post! memo post)
                                           (set-memo-taken! memo taken)
                                           (if (eq? (memo-left-rec memo) exp)
                                               (loop)
                                               (values post taken)))
                                    ; one recursion too many. return previous values
                                    (begin (when *verbose* (printi "       last time (~a ~a) : ~a~n" post (memo-post memo) (grown-rec? post (memo-post memo))))
                                           (set-memo-left-rec! memo #f)
                                           (values (memo-post memo) (memo-taken memo)))))
                         (begin
                           (set-memo-post! memo post)
                           (set-memo-taken! memo taken)
                           (values post taken))))))))]
        [(term _ _ _ "String" (list exp)) 
         (let loop ([exp (string->list (any->string exp))] [chls pos])
           (if (null? exp) (values chls null) 
               (let-values ([(new-pos taken) (parse (car exp) chls null)])
                 (if new-pos (loop (cdr exp) new-pos) (values #f null)))))]
        [(term _ _ _ "+" args) ; concatenation
         (let loop ([args args] [curr-ls pos] [term null])
           (if (null? args) (values curr-ls (reverse! term))
               (let-values ([(new-pos taken) (parse (car args) curr-ls (if (eq? curr-ls pos) path-ls null))])
                 (if new-pos 
                     (loop (cdr args) new-pos (append-reverse taken term))
                     (values #f null)))))]
        [(or (term _ _ _ "/" args) (term _ _ _ "//" args)) ; choice or parallel choice
         (let* ([char-hash (hash-ref (language-choices lang) exp (lambda () (let ([new-hash (make-hasheq )])
                                                                              (hash-set! (language-choices lang) exp new-hash) new-hash)))]
                [ch (if (< pos len) (string-ref str pos) #f)]
                [args (hash-ref char-hash ch 
                                (lambda () (let ([new-args (if ch (filter (lambda (exp) (must-try-rule? exp ch)) args) args)])
                                             (when *verbose* (printf "filtered choice rule for '~a' : ~a --- ~a~n" ch (cons '/ new-args)(cons '/ args)))
                                             (hash-set! char-hash ch new-args)
                                             new-args)))])
           
           (let loop ([args args])
             (if (null? args) (values #f null)
                 (let-values ([(new-pos taken) (parse (car args) pos path-ls)])
                   (if new-pos (if (equal? (term-name exp) "/") (values new-pos taken)
                                   (let ([success-args (filter (lambda (arg) (let-values ([(new-pos taken) (parse arg pos path-ls)])
                                                                               new-pos)) (cdr args))])
                                     (unless (null? success-args)
                                       (raise-user-error 'parse "parallel choice matches more than one alternative: \"~a\"~n" 
                                                         (apply string-append (map (lambda (a) (format "~n    ~a" a)) (cons (car args) success-args)))))
                                     (values new-pos taken))) (loop (cdr args)))))))]
        [(term _ _ _ "!" (list arg)) ; not
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (values (if new-pos #f pos) null))]
        [(term _ _ _ "<" (list arg)) ; capture literal
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           #;(when new-pos (printf "< ~a ~n" (substring str pos new-pos)))
           (let ([res (list (if new-pos (make-token infile pos new-pos (substring str pos new-pos)) #f))])
             (values new-pos res)))]
        [(or (term _ _ _ "@" (list (term _ _ _ "String" (list name)) arg)) ; gather named term
             (term _ _ _ "@" (list name arg))) 
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (if new-pos
               (let ([new-term (make-term infile pos new-pos (any->string name) taken)])
                 (values new-pos (list new-term)))
               (values new-pos taken)))]
        [(term _ _ _ ">" (list arg)) ; output
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (when new-pos
             (process-term taken))
           (values new-pos taken))]
        [(term _ _ _ "$" (list-rest arg mess)) ; parsing error : print mess when arg fails
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (unless new-pos (eprintf (make-errormessage (string-append* (map (match-lambda [(term _ _ _ "String" (list exp)) (any->string exp)]) mess)) pos new-pos)))
           (values new-pos taken))]
        [(term _ _ _ "^" (list arg)) ; take indentation whitespace
         (let-values ([(new-pos taken) (parse arg pos path-ls)])
           (when new-pos (take-indent new-pos (substring str pos new-pos)))
           (values new-pos null))]
        [(term _ _ _ "^=" (list arg)) ; match indentation token
         (values (if (match-indent pos arg) pos #f) null)]
        
        [_ (raise-user-error 'parse "unknown or illegal parser action \"~s\"~n" exp)]))
    (with-handlers ([exn:fail:filesystem? 
                     (lambda (exn)
                       (fprintf (current-error-port) "could not open ~s~n" infile) #f)]
                    [exn:fail:user? 
                     (lambda (exn)
                       (fprintf (current-error-port) "error parsing ~s: ~a~n" infile (exn-message exn)) #f)]
                    [exn:fail? 
                     (lambda (exn)
                       (fprintf (current-error-port) "error parsing ~s: ~a~n" infile (exn-message exn)) #f)])
      (let*-values ([(current-dir) (current-directory)]
                    [(_) (current-directory (path-only (simple-form-path infile)))]
                    [(res taken) (parse (mt "Name" "start") 0 null)]
                    [(_) (current-directory current-dir)])
                   (unless (and (number? res) (= res len))
                     (let ([pos (foldl max 0 (hash-map memo-hash (lambda (k v) (foldl (lambda (kv s) (if (cdr kv) (max (cdr kv) s) s))
                                                                                      0 (hash-map v (lambda (k v) (cons k (memo-post v))))))))])
                       (fprintf (current-error-port) (make-errormessage "Could not parse. First char not parsed:" pos pos)))
                     
                     #f))))
  
  #;(when *print-init* (hash-for-each (language-rules lang) (lambda (sym value)
                                                            (printf "(rule-put! '~s ~s lang)~n" sym value))))
  )




;;;;;;;;; main program: command line parsing
(define *verbose* #f)
(define (verbose-mode v) (set! *verbose* v))

(define *print-init* #f)
(define (print-init-mode v) (set! *print-init* v))

(define (main argv)
  (let ([lang (make-initialized-language)]
        [modify-lang (make-initialized-language)])
    (define (top-level terms) 
      (printf "> ~a~n" (expand-term (car terms) lang modify-lang)) 
      (print terms) (newline))
    
    ;(getenv "FOREST_DIR") (current-directory)
    ;(printf "arguments: ~s~n"  argv)
    (let ([infiles (command-line "forest" argv
                                 (once-each
                                  [("-g" "--grammar") filename "Specify grammar file to use"
                                                      (read-grammar filename lang)]
                                  [("-v" "--verbose") "Print verbose messages"
                                                      (verbose-mode #t)]
                                  [("-i" "--initial") "Print initial language"
                                                      (print-init-mode #t)])
                                 #;(once-any
                                    [("-o" "--optimize-1") "Compile with optimization level 1"
                                                           (optimize-level 1)]
                                    ["--optimize-2"        "" ; show help on separate lines
                                                           "Compile with optimization level 2,"
                                                           "which implies all optimizations of level 1"
                                                           (optimize-level 2)])
                                 #;(multi
                                    [("-l" "--link-flags") lf ; flag takes one argument
                                                           "Add a flag for the linker"
                                                           (link-flags (cons lf (link-flags)))])
                                 (args filenames ; expects names of files to as last command-line arguments
                                       filenames)) ; return list of filenames to compile
                   ])
      (exit (if (andmap (lambda (infile)
                          (if (char=? (string-ref infile 0) #\@)
                              (let ([in (open-input-file (substring infile 1))])
                                (let loop ([infile (read-line in 'any)])
                                  (when (not (eof-object? infile))
                                    (unless (char=? (string-ref infile 0) #\#) 
                                      (parse-file infile lang top-level))
                                    (loop (read-line in 'any)))))
                              (parse-file infile lang top-level)))
                        infiles) 0 1)))))

;;;;;;;;;;;;;;;;;;; the rest is macros that should be part of the grammar definition.

;;; indentation matching
(define *indent-ls* null) 
(define (take-indent chls ws)
  (when *verbose* (printf "^ ~s~n" ws))
  (set! *indent-ls* (cons (list chls ws #f) *indent-ls*)))


(define (match-indent chls token)
  (when *verbose*
    (printf "^= ~s ~s: ~s " token (list->string (take chls (min (length chls) 6))) 
            (map (lambda (el) (list (list->string (take chls (min (length chls) 6))) (cadr el))) *indent-ls*)))
  (case token 
    [(SAME)
     (if (eq? chls (caar *indent-ls*))
         (cond [(third (first *indent-ls*)) #t]
               [(string=? (cadar *indent-ls*) (cadadr *indent-ls*))
                (set! *indent-ls* 
                      (cons (list (caar *indent-ls*) (cadar *indent-ls*) #t) (cddr *indent-ls*))) #t]
               [else #f]) #f)]
    [(INDENT)
     (and (eq? chls (caar *indent-ls*)) (string-prefix? (cadadr *indent-ls*) (cadar *indent-ls*)))]
    [(DEDENT) 
     (begin0 
       (and (eq? chls (caar *indent-ls*)) (string-prefix? (cadar *indent-ls*) (cadadr *indent-ls*)))
       (set! *indent-ls* (cons (car *indent-ls*) (cddr *indent-ls*)))
       )]))

; interactive script
(if (= (vector-length (current-command-line-arguments)) 0)
    (main #("-g" "java-2.0.peg.scm" "@jythonfiles.txt"))
    (main (current-command-line-arguments)))
#;(profile-thunk (lambda ()
                   ))




