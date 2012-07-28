#!/usr/bin/env mzscheme
#lang racket

(require (lib "1.ss" "srfi")
         (lib "13.ss" "srfi")
         (lib "14.ss" "srfi")
         (lib "cmdline.ss")
         scheme/match)

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
    
    (rule-put! 'Sexpr (mt "//" 
                          (mt "@" (mt "String" "Null") (mt "Name" "Null"))
                          (mt "@" (mt "String" "$") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "$") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr") (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" ">") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" ">") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "@") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "@") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr") (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "<") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "<") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "!") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "!") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "/") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "/") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "//") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "//") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "+") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "+") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Term") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Term") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Name") (mt "Name" "Sexprs")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Import") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Import") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "StringLiteral")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "NewName") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "NewName") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace"))) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Insert") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Insert") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Name") (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Rule") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Rule") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Name") (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "SyntaxPattern") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "SyntaxPattern") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr") (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "Unexpanded") (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "+" (mt "+" (mt "String" "Unexpanded") (mt "!" (mt "Name" "NameChar"))) (mt "Name" "WhiteSpace")) (mt "Name" "Sexpr")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))))
                          (mt "@" (mt "String" "String") (mt "Name" "StringLiteral"))
                          (mt "Name" "Name")) lang)
    (rule-put! 'Declarations (mt "/" (mt "+" (mt "Name" "Declaration") (mt "Name" "Declarations")) (mt "Null")) lang)
    (rule-put! 'StringLiteral (mt "+" (mt "+" (mt "Name" "DoubleQuoteChar") (mt "<" (mt "Name" "StringChars")) (mt "Name" "DoubleQuoteChar")) (mt "Name" "WhiteSpace")) lang)
    (rule-put! 'Newline (mt "/" (mt "+" (mt "Name" "ReturnChar") (mt "Name" "LinefeedChar")) (mt "Name" "ReturnChar") (mt "Name" "LinefeedChar")) lang)
    (rule-put! 'StringChars (mt "/" (mt "+" (mt "!" (mt "Name" "DoubleQuoteChar")) (mt "Name" "AnyChar") (mt "Name" "StringChars")) (mt "Null")) lang)
    (rule-put! 'LineComment (mt "+" (mt "String" ";") (mt "Name" "CommentChars") (mt "Name" "Newline")) lang)
    (rule-put! 'Start (mt "+" (mt "Name" "WhiteSpace") (mt "Name" "Declarations") (mt "Name" "EndOfFile")) lang)
    (rule-put! 'EndOfFile (mt "!" (mt "Name" "AnyChar")) lang)
    (rule-put! 'NameChars (mt "/" (mt "+" (mt "Name" "NameChar") (mt "Name" "NameChars")) (mt "Null")) lang)
    (rule-put! 'Declaration (mt ">" (mt "Name" "Sexpr")) lang)
    (rule-put! 'Name (mt "//" (mt "@" (mt "String" "Var") (mt "+" (mt "+" (mt "String" "'") (mt "Name" "WhiteSpace")) (mt "Name" "NameLiteral"))) (mt "@" (mt "String" "VarList") (mt "+" (mt "+" (mt "String" "'''") (mt "Name" "WhiteSpace")) (mt "Name" "NameLiteral"))) (mt "@" (mt "String" "Name") (mt "Name" "NameLiteral"))) lang)
    (rule-put! 'Null (mt "+" (mt "+" (mt "String" "(") (mt "Name" "WhiteSpace")) (mt "+" (mt "String" ")") (mt "Name" "WhiteSpace"))) lang)
    (rule-put! 'NameLiteral (mt "+" (mt "<" (mt "+" (mt "Name" "NameChar") (mt "Name" "NameChars"))) (mt "Name" "WhiteSpace")) lang)
    (rule-put! 'CommentChars (mt "/" (mt "+" (mt "!" (mt "Name" "Newline")) (mt "Name" "AnyChar") (mt "Name" "CommentChars")) (mt "Null")) lang)
    (rule-put! 'Sexprs (mt "/" (mt "+" (mt "Name" "Sexpr") (mt "Name" "Sexprs")) (mt "Null")) lang)
    (rule-put! 'NameChar (mt "/" (mt "Name" "LetterChar") (mt "Name" "DigitChar") (mt "String" "$") (mt "String" "+") (mt "String" "<") (mt "String" "=") (mt "String" ">") (mt "String" "^") (mt "String" "`") (mt "String" "|") (mt "String" "~") (mt "String" "_") (mt "String" "!") (mt "String" "%") (mt "String" "&") (mt "String" "*") (mt "String" "-") (mt "String" "+") (mt "String" "?") (mt "String" ":") (mt "String" "/") (mt "String" "@") (mt "String" ".")) lang)
    (rule-put! 'WhiteSpace (mt "/" (mt "+" (mt "/" (mt "Name" "WhitespaceChar") (mt "Name" "LineComment")) (mt "Name" "WhiteSpace")) (mt "Null")) lang)
    
    
    ;;;;;;;; end of initial grammar
    
    (pattern-put! "SyntaxPattern" (list (cons (mt "SyntaxPattern" (mt "UnVar" "pat") (mt "UnVar" "repl")) 
                                              PatternPattern)) lang)
    (pattern-put! "TermPattern" (list (cons (mt "TermPattern" (mt "Var" "pat") (mt "Var" "repl")) 
                                            PatternPattern)) lang)
    (pattern-put! "Rule" (list (cons (mt "Rule" (mt "Name" (mt "Var" "name")) (mt "Var" "rule")) 
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
                                            (with-handlers 
                                                ([exn:fail? (lambda (exn)
                                                              (eprintf (make-errormessage (exn-message exn) (token-file fil) (token-start-pos fil) (token-end-pos fil))))])
                                              (read-grammar f read-lang)))
                                          (mt "Null")]))) lang)
    (pattern-put! "Insert" (list (cons (mt "Insert" (mt "Name" (mt "Var" "name")) (mt "Var" "rule"))  
                                       (match-lambda*
                                         [(list read-lang lang (cons _ rule) (cons _ name)) 
                                          (let ([term (rule-get name lang)])
                                            (when (not (and (term? term) (token-equal? (term-name term) "//")))
                                              (error 'Insert "insertion in something other than // : ~s -- ~s" name rule))
                                            (when *verbose* (printf "Insert into (lang ~a) ~s : ~a == ~a~n"  (language-name lang) name rule term))
                                            (rule-put! name (apply mt (term-name term) (lset-union! term-equal? (list rule) (term-vals term))) lang)
                                            rule)]))) lang)
    (pattern-put! "Term" (list (cons (mt "Term" (mt "Name" (mt "Var" "name")) (mt "VarList" "vals")) 
                                     (match-lambda*
                                       [(list read-lang lang (cons _ vals) (cons _ name)) 
                                        (let ([new-term (term '() '() '() (if (token? name) (token-chars name) name) vals)])
                                          (when *verbose* (printf "Creating term ~a~n" new-term))
                                          new-term)]))) lang)
    (pattern-put! "Token" (list (cons (mt "Token" (mt "Var" "chars")) 
                                      (match-lambda*
                                        [(list read-lang lang (cons _ tok)) 
                                         (unless (token? tok) (raise-user-error 'Token "Content of Token term must be a token, found ~a" tok))
                                         (when *verbose* (printf "Creating token ~a~n" tok))
                                         tok]))) lang)
    
    lang))

(define PatternPattern (match-lambda*
                         [(list read-lang lang (cons _ repl) (cons _ pat)) 
                          (let ([name (term-name pat)])
                            (when *verbose* (printf "add Pattern to (lang ~a) ~a : ~a => ~a~n"  (language-name lang) name pat repl))
                            (pattern-put! name (cons (cons pat repl) (pattern-ref name lang)) lang)
                            (mt "Name" name))]))

(define (expand-term trm lang modify-lang)
  (cond [(not (term? trm)) trm]
        [(equal? (term-name trm) "Unexpanded") (car (term-vals trm))]
        [else 
         (let pat-loop ([pats [reverse (pattern-ref (term-name trm) lang)]])
           (if (null? pats) 
               (make-term (term-file trm) (term-start-pos trm) (term-end-pos trm) (term-name trm) 
                          (map (lambda (t) (expand-term t lang modify-lang)) (term-vals trm)))
               #;(let ([new-vals (map (lambda (t) (expand-term t lang modify-lang)) (term-vals trm))])
                   (if (andmap eq? new-vals (term-vals trm)) trm 
                       (make-term (term-file trm) (term-start-pos trm) (term-end-pos trm) (term-name trm) 
                                  new-vals)))               
               (let ([bnd (let match-loop ([bindings null]
                                           [pattern (car (car pats))]
                                           [trm trm])
                            (match pattern 
                              [(term _ _ _ "UnVar" (list v)) (cons (cons v trm) bindings)]
                              [(term _ _ _ "Var" (list v)) (cons (cons v (expand-term trm lang modify-lang)) bindings)]
                              [(term _ _ _ n (list-rest r ... (list (term _ _ _ "UnVarList" (list v)))))
                               (printf "UnVarList ~a :: ~a~n" pattern trm)
                               (if (and (term? trm) (equal? (term-name trm) n) (>= (length (term-vals trm)) (length r)))
                                   (let ([bnd (foldl (lambda (pat val b) (if b (match-loop b pat val) #f)) bindings r (take (term-vals trm) (length r)))])
                                     (if bnd (cons (cons v (drop (term-vals trm) (length r))) bnd) #f)) #f)]
                              [(term _ _ _ n (list-rest r ... (list (term _ _ _ "VarList" (list v)))))
                               (if (and (term? trm) (equal? (term-name trm) n) (>= (length (term-vals trm)) (length r)))
                                   (let ([bnd (foldl (lambda (pat val b) (if b (match-loop b pat val) #f)) bindings r (take (term-vals trm) (length r)))])
                                     (if bnd (cons (cons v (map (lambda (trm) (expand-term trm lang modify-lang)) (drop (term-vals trm) (length r)))) bnd) #f)) #f)]
                              [(term _ _ _ n r) (when *verbose* (printf "Matching term ~a with ~a~n" pattern trm))
                                                (if (and (term? trm) (token-equal? (term-name trm) n) (eq? (length r) (length (term-vals trm))))
                                                    (foldl (lambda (pat val b) (if b (match-loop b pat val) #f)) bindings r (term-vals trm)) #f)]
                              [(token _ _ _ c) (when *verbose* (printf "Matching token ~a with ~a~n" pattern trm))
                                               (if (token-equal? pattern trm) bindings #f)]
                              [v (if (eq? v trm) bindings #f)]))])
                 (if bnd (begin (when *verbose* (printi-up "Matched pattern ~a :: ~a~n" (car (car pats)) trm) (printf "~a~n" bnd))
                                (let ([res 
                                       (expand-term (let replace-loop ([repl (cdr [car pats])])
                                                      (match repl 
                                                        [(? procedure? ) (apply repl lang modify-lang bnd)]
                                                        [(term _ _ _ (or "Var" "UnVar") (list v)) 
                                                         (let ([val (assoc v bnd token-equal?)])
                                                           (if (not val) (eprintf (make-errormessage (format "Variable ~a not found in pattern definition '~a'~n" 
                                                                                                             (token-chars v) (term-name trm)) (token-file v) (token-start-pos v) (token-end-pos v))) 
                                                               (cdr val)))]
                                                        [(term l1 l2 l3 n r) (term l1 l2 l3 n (foldr (match-lambda** [((term _ _ _ (or "VarList" "UnVarList") (list v)) r) 
                                                                                                                      (append (cdr (assoc v bnd token-equal?)) r)]
                                                                                                                     [(v r) (cons (replace-loop v) r)]) '() r))]
                                                        [v v])) lang modify-lang)])
                                  (when *verbose* (printi-dn "Replaced ~a with ~a~n" trm res))
                                  res)) 
                     (pat-loop (cdr pats))))))]))



; main helper procedures

(define (prepare infile)
  (let ([lang (make-initialized-language)]
        [grammar-file (find-grammar-for-ext infile)])
    (if grammar-file 
        (when (not (eq? #t grammar-file)) (read-grammar grammar-file lang))
        (eprintf "No grammar definition found for file ~a~n" infile))
    lang))

(define *ext-dir* (build-path (current-directory) "ext"))
(define (find-grammar-for-ext infile)
  (let* ([ext (bytes->string/latin-1 (filename-extension infile))]
         [lst (filter (lambda (fil) (equal? ext (last (drop-right (regexp-split #rx"\\." fil) 1)))) 
                      (directory-list *ext-dir*))])
    (if (equal? ext "forest") #t 
                  (match (filter find-grammar-for-ext lst)
                    [(cons h _) (build-path *ext-dir* h)]
                    [_ #f]))))

(define *file-cache* (make-hash))

(define (read-grammar infile lang)
  (let ([verbose *verbose*])
    (when *verbose* (printf "reading grammar ~a~n" infile))
    (set! *verbose* #f)
    (let* ([read-lang (prepare infile)]
           [results (hash-ref *file-cache* infile '())]
           [res (if (null? results) (parse-file infile read-lang (lambda (terms) (set! results (cons terms results)) (expand-term (car terms) read-lang lang))) 
                    (for-each (lambda (terms) (expand-term (car terms) read-lang lang)) results))])
      (hash-set! *file-cache* infile (reverse results))
      
      (set! *verbose* verbose)
      (when *verbose* (printf "Done reading grammar ~a~n" infile))
      (unless res
        (raise-user-error 'read-grammar "Error reading file ~a" infile)))))

(define *grammar-paths* (list (current-directory) (build-path (current-directory) "grammars")))

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


(define (parse-file infile lang process-term)
  
  (when *verbose* (printf " parsing ~s ...~n" infile))
  (let* ([infile (search-file infile)]
         [len (file-size infile)]
         [str (call-with-input-file infile (lambda (in) (bytes->string/latin-1 (read-bytes len in))))]
         [memo-hash (make-hasheq)])
    
    (define (rule-memo sym)
      (hash-ref memo-hash sym (lambda () (let  ([h (make-hash)]) (hash-set! memo-hash sym h) h))))
    
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
          
          
          [(term _ _ _ name (list arg)) ; match indentation token
           (raise-user-error 'parse "Term \"~a\" is not a valid parser action. Perhaps a Pattern of that name is not defined correctly in imported grammars.~nExpression was \"~a\"~n" name exp)]
          [_ (raise-user-error 'parse "Unknown or illegal parser action \"~a\"~n This error should never be reported. (You found a bug!)~n" exp)]))
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
                                (let path-loop ([path path-ls])
                                  (when (not (eq? (car path) exp))
                                    (set-memo-left-rec! (hash-ref (rule-memo (car path)) pos #f) exp) ; TODO: check this line
                                    (path-loop (cdr path)))))
                         (when *verbose* (printi "     cached ~s~n" exp)))
                     (values (memo-post memo) (memo-taken memo))))
               (let ([memo (make-memo #f null #f)])
                 (hash-set! (rule-memo exp) pos memo)
                 (let recurse-loop ()
                   (let*-values ([(post taken) (parse rule pos (cons exp path-ls))])
                     (if (memo-left-rec memo) ; now we're back down in left recursion
                         (begin (when *verbose* (printi "   --> ~s Left recursion : ~a, ~a~n" exp post taken))
                                (if (grown-rec? post (memo-post memo))
                                    ; we made progress parsing the left recursion. Save result and try again
                                    (begin (when *verbose* (printi "       go again (~a ~a) : ~a~n" post (memo-post memo) (grown-rec? post (memo-post memo))))
                                           (set-memo-post! memo post)
                                           (set-memo-taken! memo taken)
                                           (if (eq? (memo-left-rec memo) exp)
                                               (recurse-loop)
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
         (let* ([char-hash (hash-ref (language-choices lang) exp (lambda () (let ([new-hash (make-hasheq )])
                                                                              (hash-set! (language-choices lang) exp new-hash) new-hash)))]
                [ch (if (< pos len) (string-ref str pos) #f)]
                [args (hash-ref char-hash ch 
                                (lambda () (let ([new-args (if ch (filter (lambda (exp) (must-try-rule? exp ch)) args) args)])
                                             (when *verbose* (printf "filtered choice rule for '~a' : ~a --- ~a~n" ch (cons '/ new-args)(cons '/ args)))
                                             (hash-set! char-hash ch new-args)
                                             new-args)))])
           
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
           (unless new-pos (eprintf (make-errormessage (string-append* (map (match-lambda [(term _ _ _ "String" (list exp)) (any->string exp)]) mess)) infile pos new-pos)))
           (values new-pos taken))]
        
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
      (let*-values ([(current-dir) (current-directory)]
                    [(_) (current-directory (path-only (simple-form-path infile)))]
                    [(res taken) (parse (mt "Name" "Start") 0 null)]
                    [(_) (current-directory current-dir)])
        (unless (and (number? res) (= res len))
          (let ([pos (foldl max 0 (hash-map memo-hash (lambda (k v) (foldl (lambda (kv s) (if (cdr kv) (max (cdr kv) s) s))
                                                                           0 (hash-map v (lambda (k v) (cons k (memo-post v))))))))])
            (fprintf (current-error-port) (make-errormessage "Could not parse. First char not parsed:" infile pos pos)))
          
          #f))))
  
  (when *print-init* (hash-for-each (language-rules lang) (lambda (sym value)
                                                            (printf "(rule-put! '~s ~s lang)~n" sym value))))
  )




;;;;;;;;; main program: command line parsing
(define *verbose* #f)
(define (verbose-mode v) (set! *verbose* v))

(define *print-init* #f)
(define (print-init-mode v) (set! *print-init* v))

(define (main argv)  
  (let ([infiles (command-line "forest" argv
                               (once-each
                                [("-v" "--verbose") "Print verbose messages"
                                                    (verbose-mode #t)]
                                [("-i" "--initial") "Print initial language"
                                                    (print-init-mode #t)])                               
                               (args filenames ; expects names of files to as last command-line arguments
                                     filenames)) ; return list of filenames to compile
                 ])
    (exit (if (andmap (lambda (infile)
                        (let ([lang (prepare infile)]
                              [modify-lang (make-initialized-language)])
                          (parse-file infile lang (lambda (terms) (printf "> ~a~n" (expand-term (car terms) lang modify-lang))))))
                      infiles) 0 1))))

; interactive script
(if (= (vector-length (current-command-line-arguments)) 0)
    (main #("-g" "grammars/java-1.0.peg.scm" "test/enum_example.java"))
    (main (current-command-line-arguments)))
