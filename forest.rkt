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
  (when (*verbosep*) (printf "~a~a" (pat-indent-cnt-same) (apply format fmt args))))
(define (printip-up fmt . args)
  (when (*verbosep*) (printf "~a~a" (pat-indent-cnt-up) (apply format fmt args))))
(define (printip-dn fmt . args)
  (when (*verbosep*) (printf "~a~a" (pat-indent-cnt-dn) (apply format fmt args))))


;;;;;;;;;;;;;; structs
(define (term=? term1 term2 recursive-equal?)
  (and (token-equal? (term-name term1) (term-name term2))
       (recursive-equal? (term-vals term1) (term-vals term2))))
(define (term-hash-1 term recursive-equal-hash)
  (+ (* 97 (recursive-equal-hash (if (token? (term-name term)) (token-chars (term-name term)) (term-name term))))
     (recursive-equal-hash (term-vals term))))
(define (term-hash-2 term recursive-equal-hash)
  (+ (* 79 (recursive-equal-hash (if (token? (term-name term)) (token-chars (term-name term)) (term-name term))))
     (recursive-equal-hash (term-vals term))))
(define (term-write t port write?)
  (if write? (begin
               (write-string "(mt " port) (write (term-name t) port)
               (let ([l (term-vals t)])
                 (unless (null? l)
                   (write-string " " port) (write (car l) port)
                   (for-each (lambda (e) (write-string " " port) ((if write? write display) e port)) (cdr l))))
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
        (write-string "]" port))))
(define-struct term (file start-pos end-pos name vals)
  #:methods gen:equal+hash
  [(define equal-proc term=?)
   (define hash-proc  term-hash-1)
   (define hash2-proc term-hash-2)]
  #:methods gen:custom-write [(define write-proc term-write)])

(define (token-equal? token1 token2)
  (let ([token1-str (if (token? token1) (token-chars token1) token1)]
        [token2-str (if (token? token2) (token-chars token2) token2)])
    (equal? token1-str token2-str)))
(define (token=? token1 token2 recursive-equal?)
  (recursive-equal? (token-chars token1) (token-chars token2)))
(define (token-hash-1 token recursive-equal-hash)
  (recursive-equal-hash (token-chars token)))
(define (token-hash-2 token recursive-equal-hash)
  (recursive-equal-hash (token-chars token)))
(define (token-write t port write?)
  (if write? 
      (write (token-chars t) port)
      (begin
        (write-string "#\"" port)
        (display (token-chars t) port)
        (write-string "\"" port))))
(define-struct token (file start-pos end-pos chars)
  #:methods gen:equal+hash
  [(define equal-proc token=?)
   (define hash-proc  token-hash-1)
   (define hash2-proc token-hash-2)]
  #:methods gen:custom-write [(define write-proc token-write)])


(define *depfiles* (list ))

(define (mt name . vals)
  (make-term #f #f #f name vals))

(define (mtk name)
  (make-token #f #f #f name))

(define (lmt f s e name . vals)
  (and f (list-ref *depfiles* f))
  (make-term (and f (string->path (list-ref *depfiles* f))) s e name vals))

(define (lmtk f s e name)
  (and f (list-ref *depfiles* f))
  (make-token (and f (string->path (list-ref *depfiles* f))) s e name))

(define-struct memo ((post #:mutable) (taken #:mutable) (left-rec #:mutable)))

(define-struct language ((files #:mutable) (depfiles #:mutable) (patterns #:mutable) (rules #:mutable) (choices #:mutable)))

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
    [_ (call-with-output-string (lambda (p) (display sym p)))]))

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
  (let* ([files (list "bootstrap/forest.forest")]
         [cache-file-name (build-path *forest-home* "bootstrap/bootstrap_forest.forest.cache")]
         [deps-file-name (path-replace-suffix cache-file-name ".deps")]
         [depfiles (or (and (file-exists? deps-file-name) (call-with-input-file deps-file-name (lambda (port) (string-split (port->string port) #px"[\n\r]")))) files)])
    (let ((cached-lang (call-with-input-file cache-file-name (lambda (port) (set! *depfiles* depfiles) (eval (read port) *ns*))))) 
      (set-language-files! lang files)
      (set-language-patterns! lang (language-patterns cached-lang))
      (set-language-rules! lang (language-rules cached-lang)))))


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


(define (do-import fil lang)
  (let ([f (any->string fil)])
    (when (*verbosep*) (printip "Import ~s~n" f))
    (with-handlers 
        ([exn:fail? (lambda (exn)
                      (eprintf (make-errormessage (exn-message exn) fil)))])
      (read-grammar (string->path f) lang)
      (set-language-choices! lang (make-hasheq))))
  (mt "null"))

(define *symtable* (make-hash))

; initial procedure patterns
(define *init-patterns* (list
                         (cons 'error (list (cons (mt "error" (mt "varlist" "strs")) 
                                                  (match-lambda*
                                                    [(list read-lang lang trm (cons _ strs))
                                                     (let ([errstr (string-concatenate (map any->string strs))])
                                                       (eprintf "Error while pattern matching: ~a~nTraceback: ~n" errstr)
                                                       (raise (make-exn:fail:errorpattern errstr (current-continuation-marks))))]))))  
                         (cons 'message_ (list (cons (mt "message_" (mt "var" "loc") (mt "varlist" "strs")) 
                                                     (match-lambda*
                                                       [(list read-lang lang trm (cons _ strs) (cons _ loc))
                                                        (let ([errstr (string-concatenate (map any->string strs))])
                                                          (eprintf (make-errormessage errstr loc))
                                                          (mt "null"))]))))  
                         (cons 'pattern (list (cons (mt "pattern" (mt "var" "pat") (mt "var" "repl")) 
                                                    (match-lambda*
                                                      [(list read-lang lang trm (cons _ repl) (cons _ pat)) 
                                                       (printip "add Pattern ~a => ~a~n" pat repl)
                                                       (let ([name (term-name pat)])
                                                         (if (term? name)
                                                             (pattern-put! '|| (cons (cons pat repl) (pattern-ref '|| lang)) lang)
                                                             (pattern-put! name (cons (cons pat repl) (pattern-ref name lang)) lang))                                                         
                                                         (mt "name" name))]))))
                         (cons 'rule (list (cons (mt "rule" (mt "name" (mt "varlist" "names")) (mt "var" "rule")) 
                                                 (match-lambda*
                                                   [(list read-lang lang trm (cons _ rule) (cons _ names)) 
                                                    (let ([name (string-append* (map any->string names))]) (rule-put! name rule lang) (mt "name" name))])))) 
                         (cons 'newname (list (cons (mt "newname") 
                                                    (match-lambda*
                                                      [(list read-lang lang trm) (mt "name" (symbol->string (gensym)))]))))
                         (cons 'extend (list (cons (mt "extend" (mt "var" "fil")) 
                                                   (match-lambda*
                                                     [(list read-lang lang trm (cons _ fil)) 
                                                      (do-import fil read-lang)]))))
                         (cons 'include (list (cons (mt "include" (mt "var" "fil")) 
                                                    (match-lambda*
                                                      [(list read-lang lang trm (cons _ fil)) 
                                                       (do-import fil lang)]))))
                         (cons 'insert (list (cons (mt "insert" (mt "name" (mt "varlist" "names")) (mt "var" "rule"))  
                                                   (match-lambda*
                                                     [(list read-lang lang trm (cons _ rule) (cons _ names)) 
                                                      (let ([name (string-append* (map any->string names))])
                                                        (if (rule-exists? name lang) 
                                                            (let ([term (rule-get name lang)])
                                                              (when (not (and (term? term) (token-equal? (term-name term) "//")))
                                                                (error 'Insert "insertion in something other than // : ~s -- ~s" name rule))
                                                              (when (*verbosep*) (printip "Insert ~s : ~a == ~a~n" name rule term))
                                                              (rule-put! name (apply mt (term-name term) (lset-union! equal? (list rule) (term-vals term))) lang)
                                                              rule)
                                                            (begin (rule-put! name (mt "//" rule) lang) rule)))]))))
                         (cons 'term (list (cons (mt "term" (mt "var" "var") (mt "varlist" "vals")) 
                                                 (match-lambda*
                                                   [(list read-lang lang trm (cons _ vals) (cons _ var)) 
                                                    (let*-values ([(file start end) (cond [(term? var) (values (term-file var) (term-start-pos var) (term-end-pos var))]
                                                                                          [(token? var) (values (token-file var) (token-start-pos var) (token-end-pos var))]
                                                                                          [else (values #f #f #f)])]
                                                                  [(new-term) (term file start end var vals)]
                                                                  [(unexpanded) (mt "unexpanded" new-term)])
                                                      (when (*verbosep*) (printip "Creating variable term ~a~n" new-term))
                                                      unexpanded)]))
                                           (cons (mt "term" (mt "name" (mt "varlist" "names")) (mt "varlist" "vals")) 
                                                 (match-lambda*
                                                   [(list read-lang lang trm (cons _ vals) (cons _ names)) 
                                                    (let* ([name (string-append* (map any->string names))]
                                                           [new-term (term (term-file trm) (term-start-pos trm) (term-end-pos trm) (if (token? name) (token-chars name) name) vals)]
                                                           [unexpanded (mt "unexpanded" new-term)])
                                                      (when (*verbosep*) (printip "Creating term ~a~n" new-term))
                                                      unexpanded)]))))
                         (cons 'token (list (cons (mt "token" (mt "varlist" "strings")) 
                                                  (match-lambda*
                                                    [(list read-lang lang trm (cons _ strings))
                                                     (for-each (lambda (chars) 
                                                                 (unless (token? chars) (raise-user-error 'Token "Content of Token term must be a token, found ~a" chars))) 
                                                               strings)
                                                     (let ([chars (if (= (length strings) 1) (car strings) (mtk (apply string-append {map token-chars strings})))])
                                                       (when (*verbosep*) (printip "Creating token ~a~n" chars))
                                                       chars)]))))
                         (cons 'symtable-put (list (cons (mt "symtable-put" (mt "var" "key") (mt "var" "val")) 
                                                         (match-lambda*
                                                           [(list read-lang lang trm (cons _ val) (cons _ key))
                                                            (printf "symtable-put ~a: ~a~n" key val) 
                                                            (hash-set! *symtable* key val)
                                                            val]))))
                         (cons 'symtable-get (list (cons (mt "symtable-get" (mt "var" "key")) 
                                                         (match-lambda*
                                                           [(list read-lang lang trm (cons _ key))
                                                            (printf "symtable-get ~a~n" key)
                                                            (let ([val (hash-ref *symtable* key)])
                                                              (printf "symtable-get ~a: ~a~n" key val)                                                   
                                                              val)]))))))

(define (expand-term trm lang modify-lang)
  ; print traceback for error pattern
  (with-handlers ([exn:fail:errorpattern? (lambda (exn) (unless (token-equal? (term-name trm) "error") (eprintf (make-errormessage "" trm))) 
                                            (raise exn))])
    (cond [(or (token? trm) (string? trm)) trm]
          [(not (term? trm)) (raise-user-error 'expand-term "called with something other than a term or token: ~s" trm)]
          [(token-equal? (term-name trm) "unexpanded") (car (term-vals trm))] ;; TODO: change this
          [else 
           (parameterize ([*verbosep* (if (member (any->string (term-name trm)) (*verbosepats*)) #t (*verbosep*))])
             (printip-up "(expand-term ~a)~n" trm)
             (begin0 
               (let* ([post-name (expand-term (term-name trm) lang modify-lang)]
                      [post-vals (map (lambda (t) (expand-term t lang modify-lang)) (term-vals trm))]
                      [post-term (make-term (term-file trm) (term-start-pos trm) (term-end-pos trm) post-name post-vals)])
                 (let pat-loop ([pats (reverse (pattern-ref (if (term? (term-name trm)) '|| (term-name trm)) lang))])
                   (if (null? pats) post-term
                       (let ([bnd (let match-loop ([bindings null]
                                                   [pattern (car (car pats))]
                                                   [trm post-term])
                                    ;(printip-up "Matching ~a on ~a...~n" pattern trm)
                                    (let ([res (match pattern 
                                                 [(term _ _ _ "var" (list v)) (let ([existing-var (assoc v bindings equal?)])
                                                                                ; check if binding same variable is to same value
                                                                                (if existing-var (if (equal? (cdr existing-var) trm) bindings #f) 
                                                                                    (cons (cons v trm) bindings)))]
                                                 [(term _ _ _ n (list-rest r ... (list (term _ _ _ "varlist" (list v))))) 
                                                  (if (and (term? trm) (>= (length (term-vals trm)) (length r)))                                                
                                                      (let* ([b (match n 
                                                                  [(? term?) (match-loop bindings (term-name pattern) (term-name trm))]
                                                                  [_ (if (token-equal? (term-name trm) n) bindings #f)])]
                                                             [bnd (foldl (lambda (pat val b) (if b (match-loop b pat val) #f)) b r (take (term-vals trm) (length r)))])
                                                        (if bnd (cons (cons v (drop (term-vals trm) (length r))) bnd) #f)) #f)]
                                                 [(term _ _ _ n r) (if (and (term? trm) (eq? (length r) (length (term-vals trm))))
                                                                       (let ([b (match n 
                                                                                  [(term _ _ _ "var" (list v)) (match-loop bindings (term-name pattern) (term-name trm))]
                                                                                  [_ (if (token-equal? (term-name trm) n) bindings #f)])])
                                                                         (foldl (lambda (pat val b) (if b (match-loop b pat val) #f)) b r (term-vals trm))) #f)]
                                                 [(token _ _ _ c) (if (equal? pattern trm) bindings #f)]
                                                 [(? string? s) (printf "got a string ~s!!~n" s) (if (eq? s trm) bindings #f)]
                                                 [v (printf "got into notypeland with ~s, ~a!!~n" v (string? v)) (if (eq? v trm) bindings #f)])])
                                      ;(printip-dn "Matched ~a : ~a on ~a...~n" res pattern trm)
                                      res))])
                         (if bnd (begin (printip-up "Matched ~a to pattern ~a using ~a~n" post-term (car (car pats)) bnd)
                                        (let* ([res (let replace-loop ([repl (cdr [car pats])])
                                                      (match repl 
                                                        [(? procedure?) (apply repl lang modify-lang trm bnd)]
                                                        [(term _ _ _ "unreplaced" (list v)) v]
                                                        [(term _ _ _ "var" (list (or (? token? v) (? string? v)))) 
                                                         (let ([val (assoc v bnd token-equal?)])
                                                           (if (not val) (eprintf (make-errormessage (format "Variable ~a not found in pattern definition '~a'~n" 
                                                                                                             v (term-name post-term)) v)) 
                                                               (cdr val)))]
                                                        [(term l1 l2 l3 n r) 
                                                         (term l1 l2 l3 (replace-loop n) 
                                                               (foldr (match-lambda** 
                                                                       [((term _ _ _ "varlist" (list (or (? token? v) (? string? v)))) r)
                                                                        (let ([val (assoc v bnd token-equal?)])
                                                                          (if (not val) (eprintf (make-errormessage (format "List variable ~a not found in pattern definition '~a'~n" 
                                                                                                                            v (term-name post-term)) v)) 
                                                                              (append (cdr val) r)))]
                                                                       [(v r) (cons (replace-loop v) r)]) '() r))]
                                                        [v v]))])
                                          (printip-dn "Replaced ~a with ~a~n" trm res)
                                          (expand-term res lang modify-lang))) 
                             (pat-loop (cdr pats)))))))
               (when (*verbosep*) (pat-indent-cnt-dn))
               ))])))





(define (make-lang files patterns rules)
  (let ((lang (make-language files '() (make-hash) (make-hash) (make-hasheq))))
    
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

(define *forest-home* (path->directory-path (or (getenv "FOREST_HOME") (let-values ([(b n m) (split-path (simplify-path (path->complete-path (find-system-path 'run-file) (find-system-path 'orig-dir))))]) b))))
(define *ext-dirs* (list "ext" "bootstrap" ))
(define *ext-dir-list* (append-map (lambda (dir) (directory-list #:build? #t (build-path *forest-home* dir))) *ext-dirs*))
(define (find-grammar-for-ext infile)
  (let* ([ext (bytes->string/latin-1 (filename-extension infile))]
         [lst (filter (lambda (fil) (equal? ext (let-values ([(b n m) (split-path fil)]) 
                                                  (string-take (path-element->string n) (or (string-index-right (path-element->string n) #\.) 0))))) 
                      *ext-dir-list*)])
    (if (member infile lst) infile
        (let ((grammars (filter find-grammar-for-ext lst)))
          (if (pair? grammars) (car grammars) #f)))))

(define (prepare-language-choices exp lang) 
  (define (charset-to-try exp syms)
    (match exp
      [(? char?) (char-set exp)]
      [(? char-set?) exp]
      [(token _ _ _ chars) (char-set (string-ref (any->string chars) 0))]
      
      [(term _ _ _ "null" (list)) char-set:full]
      [(term _ _ _ "name" (list names ...)) 
       (let ([exp (string-append* (map any->string names))]) 
         (if (member exp syms) char-set:full (charset-to-try (rule-get exp lang) (cons exp syms))))]
      
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
  
  (charset-to-try exp null))

(define (file-more-recent fil1 fil2)
  (and (file-exists? fil1) (file-exists? fil2) 
       (> (file-or-directory-modify-seconds fil1) (file-or-directory-modify-seconds fil2))))

;;;;;;;;;;;; write cached languages

(define (write-language t port depfiles . prefs)
  (define (list-body ls)
    (cond ((pair? ls) (apply write-language (car ls) port depfiles (cons "  " prefs)) 
                      (when (pair? (cdr ls)) (newline port) (display (apply string-append prefs) port))
                      (list-body (cdr ls)))
          (else ls)))
  (define (term-body ls)
    (cond ((pair? ls) (apply write-language (car ls) port depfiles prefs) 
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
                                     (if (and ikv (eq? (cdr ikv) v)) #f (cons k v)))))) port depfiles "  ")
     (newline port)
     (write-language
      (filter identity (hash-map (language-rules t) 
                                 (lambda (k v)
                                   (let ((ikv (assoc k *init-rules*)))
                                     (if (and ikv (eq? (cdr ikv) v)) #f (cons k v)))))) port depfiles "  ")
     (display ")" port))
    ((term? t)
     (if (term-start-pos t) 
         (fprintf port "(lmt ~s ~s ~s ~s " (list-index (lambda (s) (and (term-file t) (equal? s (path->string (term-file t))))) depfiles) 
                  (term-start-pos t) (term-end-pos t) (term-name t))
         (fprintf port "(mt ~s "  (term-name t)))
     (term-body (term-vals t))
     (display ")" port))
    ((token? t)
     (if (token-start-pos t)
         (fprintf port "(lmtk ~s ~s ~s ~s)" (list-index (lambda (s) (and (token-file t) (equal? s (path->string (token-file t))))) depfiles) 
                  (token-start-pos t) (token-end-pos t) (token-chars t))
         (fprintf port "(mtk ~s)" (token-chars t))))
    ((string? t) (write t port))
    ((symbol? t) (display "'" port) (write t port))
    ((char-set? t) (display "{" port) (char-set-for-each (lambda (c) (write c port) (display " " port)) t) (display "}" port))
    ((proper-list? t)   
     (display "(list " port)
     (list-body t)
     (display ")" port))
    ((pair? t) 
     (display "(cons " port)
     (write-language (car t) port depfiles)  
     (display " " port)
     (write-language (cdr t) port depfiles)
     (display ")" port))
    (else (display t port))))



(define *lang-cache* (make-hash))

(define *file-cache* (make-hash))

(define *cache-dir* (build-path *forest-home* "cache"))

(define (read-grammar infile modify-lang)
  (when (or (*verbose*) (*verbosep*)) (printf "reading grammar ~a~n" infile))
  
  (let* ([files (cons (path->string (search-file infile)) (language-files modify-lang))]
         [short-files (map (lambda (f) (if (string-prefix? (path->string *forest-home*) f) 
                                           (string-drop f (string-length (path->string *forest-home*)))
                                           f)) (cons (path->string (search-file infile)) (language-files modify-lang)))]
         [cache-file-name (build-path *cache-dir* 
                                      (string-append
                                       (string-join (map (lambda (f) (regexp-replace 
                                                                      "/" (regexp-replace* "\\\\" f "_") "_")) short-files) ",") 
                                       ".cache"))]
         [deps-file-name (path-replace-suffix cache-file-name ".deps")]
         [depfiles (or (and (file-exists? deps-file-name) (call-with-input-file deps-file-name (lambda (port) (string-split (port->string port) #px"[\n\r]")))) files)])
    (if (and *cache* (every (lambda (f) (file-more-recent cache-file-name f)) depfiles))
        (let ((cached-lang (call-with-input-file cache-file-name (lambda (port) (set! *depfiles* depfiles) (eval (read port) *ns*))))) 
          (set-language-files! modify-lang files)
          (set-language-patterns! modify-lang (language-patterns cached-lang))
          (set-language-rules! modify-lang (language-rules cached-lang))) 
        (parameterize ((*verbose* #f)
                       (*verbosep* #f))
          (let* ([read-lang (parse-file infile (lambda (read-lang terms) (for-each (lambda (trm) (expand-term trm read-lang modify-lang)) terms)))]
                 [depfiles (lset-difference equal? (lset-union equal? files (language-files read-lang)) (list "core"))])
            (call-with-output-file deps-file-name (lambda (port) (for-each (lambda (f) (displayln f port)) depfiles)) #:exists 'truncate)
            (set-language-files! modify-lang files)
            (call-with-output-file cache-file-name (lambda (port) (write-language modify-lang port depfiles)) #:exists 'truncate))))
    
    (when (or (*verbose*) (*verbosep*)) (printf "Done reading grammar ~a: ~a ~n" infile (language-files modify-lang)))))


(define *grammar-paths* (list (build-path *forest-home* "grammars")))

(define (search-file infile)
  (if (path-string? infile)
      (if (file-exists? infile) infile  
          (let ([p (find (lambda (p) (file-exists? (build-path p infile))) *grammar-paths*)])
            (if p (build-path p infile)
                (raise-user-error 'search-file "File not found: ~a, current dir: ~a~n" infile (current-directory)))))
      false))


(define (make-errormessage message term-or-token)
  (define (find-line lines nline nchar)
    (if (> nchar (string-length (car lines))) 
        (find-line (cdr lines) (+ nline 1) (- nchar (string-length (car lines)) 1))
        (values (car lines) nline nchar)))
  (let*-values ([(file pos new-pos) (if (term? term-or-token) (values (term-file term-or-token) (term-start-pos term-or-token) (term-end-pos term-or-token))
                                        (values (token-file term-or-token) (token-start-pos term-or-token) (token-end-pos term-or-token)))]
                [(infile) (search-file file)])
    (if (not infile) 
        ; infile not known
        (format "<unknown location>: ~a (~a)~n" message term-or-token)
        (let*-values ([(str) (call-with-input-file infile (lambda (in) (bytes->string/latin-1 (read-bytes (file-size infile) in))))]
                      [(line nline nchar) (find-line (regexp-split #rx"\n" str) 1 pos)]
                      [(new-line new-nline new-nchar) (find-line (regexp-split #rx"\n" str) 1 (if new-pos (- new-pos 1) (string-length str)))])
          (if (= nline new-nline) 
              (format "~a:~a: ~a~n~a~n~a~n" infile nline message line (apply string-append (string-take (regexp-replace* "[^\t ]" line " ") nchar) "^" 
                                                                             (if (> (- new-nchar 1) nchar) (list (make-string (- new-nchar nchar 1) #\-) "^") (list))))
              (format "~a:~a-~a: ~a~n~a~n~a~n~a~n~a~n" infile nline new-nline message line (string-append (string-take (regexp-replace* "[^\t ]" line " ") nchar) "^ ...")
                      new-line (string-append (string-take (regexp-replace* "[^\t ]" new-line " ") new-nchar) "^")))))))


(struct exn:fail:errorpattern exn:fail ()
  #:extra-constructor-name make-exn:fail:errorpattern
  #:transparent)


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
        [(token _ _ _ chars)
         (let string-loop ([exp (string->list (any->string chars))] [chls pos])
           (if (null? exp) (values chls null) 
               (let-values ([(new-pos taken) (parse (car exp) chls null)])
                 (if new-pos (string-loop (cdr exp) new-pos) (values #f null)))))]
        
        [(term _ _ _ "null" (list)) (values pos null)]
        [(term _ _ _ "name" (list names ...)) 
         (let* ([exp (string->symbol (string-append* (map any->string names)))]
                [rule (rule-get exp lang)]
                [memo (hash-ref (rule-memo exp) pos #f)])
           (if memo
               (begin 
                 (when (and (memo-left-rec memo) (member exp path-ls)) 
                   (set-memo-left-rec! memo (append (memo-left-rec memo) (take-while (lambda (n) (not (eq? n exp))) path-ls))))
                 (when (*verbose*) (printi "cached ~a ~n" exp))
                 (values (memo-post memo) (memo-taken memo)))
               (if (member exp path-ls)
                   (begin
                     (when (*verbose*) (printi "left-recursion ~a start ~a ~a~n" pos exp (take-while (lambda (n) (not (eq? n exp))) path-ls)))
                     (hash-set! (rule-memo exp) pos (make-memo #f null (take-while (lambda (n) (not (eq? n exp))) path-ls)))
                     (values #f null))
                   (let*-values ([(post taken) (parse rule pos (cons exp path-ls))]
                                 [(memo) (hash-ref (rule-memo exp) pos #f)])
                     (if memo ; base case of recursive call
                         (let left-rec-loop ([old-post pos] [post post] [taken taken])
                           (for-each (lambda (n) (hash-remove! (rule-memo n) pos)) (memo-left-rec memo))
                           (if (and post (> post old-post))
                               (begin 
                                 (when (*verbose*) (printi "left-recursion ~a retry ~a ~a ~a ~a ~a~n" pos exp old-post post taken (memo-left-rec memo)))
                                 (set-memo-post! memo post) (set-memo-taken! memo taken)
                                 (let-values ([(new-post new-taken) (parse rule pos (cons exp path-ls))])
                                   (left-rec-loop post new-post new-taken)))
                               (if post 
                                   (let ([memo (hash-ref (rule-memo exp) pos #f)])   ;; did not make advance. return old values
                                     (when (*verbose*) (printi " left-recursion ~a finished ~a ~a ~a ~a~n" pos exp (memo-post memo) (memo-taken memo) path-ls))
                                     (values (memo-post memo) (memo-taken memo)))
                                   (begin   ;; could not match
                                     (when (*verbose*) (printi " left-recursion ~a failed ~a ~a ~a~n" pos exp (memo-post memo) (memo-taken memo)))
                                     (values (memo-post memo) (memo-taken memo))))))
                         (begin ; normal case of non-recursive call
                           (hash-set! (rule-memo exp) pos (make-memo post taken #f))
                           (values post taken)))))))]
        [(term _ _ _ "+" args) ; concatenation
         (let concat-loop ([args args] [curr-ls pos] [term null])
           (if (null? args) (values curr-ls (reverse! term))
               (let-values ([(new-pos taken) (parse (car args) curr-ls (if (eq? curr-ls pos) path-ls null))])
                 (if new-pos 
                     (concat-loop (cdr args) new-pos (append-reverse taken term))
                     (values #f null)))))]
        [(or (term _ _ _ "/" args) (term _ _ _ "//" args)) ; choice or parallel choice
         (let* ([kvls (hash-ref (language-choices lang) exp (lambda () 
                                                              (let ([kvls (map (lambda (exp) (cons exp (prepare-language-choices exp lang))) args)])
                                                                (hash-set! (language-choices lang) exp kvls)
                                                                kvls)))]
                [args (if (< pos len) (filter-map (lambda (kv) (and (char-set-contains? (cdr kv) (string-ref str pos)) (car kv))) kvls) args)])
           (let choice-loop ([args args])
             (if (null? args) (values #f null)
                 (let-values ([(new-pos taken) (parse (car args) pos path-ls)])
                   (if new-pos (values new-pos taken) 
                       #;(if (token-equal? (term-name exp) "/") (values new-pos taken)
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
        [(term _ _ _ "@" (list name arg)) 
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
           (unless new-pos (eprintf (make-errormessage (string-append* (map (match-lambda [(token _ _ _ exp) (any->string exp)]) mess)) (make-token infile pos new-pos ""))))
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
        
        [_ (raise-user-error 'parse "Unknown or illegal parser action \"~s\"~n" exp)]))
    (with-handlers ([exn:fail:errorpattern? (lambda (exn) #f)]
                    [exn:fail:filesystem? 
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
          (if (and (number? res) (= res len))
              lang
              (let ([pos (foldl max 0 (hash-map memo-hash (lambda (k v) (foldl (lambda (kv s) (if (cdr kv) (max (cdr kv) s) s))
                                                                               0 (hash-map v (lambda (k v) (cons k (memo-post v))))))))])
                (fprintf (current-error-port) (make-errormessage "Could not parse. First char not parsed:" (make-token infile pos pos "")))
                #f)))))))


;;;; more globals
(define *current-directory* (make-parameter "."))

;;;;;;;; define the namespace to do eval in
(define *ns* (make-base-namespace))
(namespace-set-variable-value! 'make-lang make-lang #t *ns*)
(namespace-set-variable-value! 'mt mt #t *ns*)
(namespace-set-variable-value! 'mtk mtk #t *ns*)
(namespace-set-variable-value! 'lmt lmt #t *ns*)
(namespace-set-variable-value! 'lmtk lmtk #t *ns*)

;;;;;;;;; main program: command line parsing
(define *verbose* (make-parameter #f))

(define *verbosep* (make-parameter #f))
(define *verbosepats* (make-parameter (list)))

(define *cache* #t)

(define (main argv)  
  (let ([infiles (command-line "forest" argv
                               (once-each
                                [("-v" "--verbose") "Print verbose messages"
                                                    (*verbose* #t)]
                                [("-p" "--pattern") patnames "Print pattern expansion messages"
                                                    (*verbosepats* (string-split patnames ","))]
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
