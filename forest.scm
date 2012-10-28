
(import (srfi :1)
        (except (srfi :13) string-hash)
        (srfi :14)
        (srfi :69)
        (gnu kawa slib pregexp))

		
;;;;;;;;;; helper functions not in Kawa

(define (printf fmt . args)
  (display (apply format fmt args)))

(define (fprintf port fmt . args)
  (display (apply format fmt args)) port)

(define (eprintf fmt . args)
  (fprintf (current-error-port) fmt args))

(define (directory-list (dir ::string))
  (let ((arr (java.io.File:list (java.io.File dir))))
    (unfold (lambda (n) (= n 0)) (lambda (n) (arr (- n 1))) (lambda (n) (- n 1)) arr:length)))

(define-syntax begin0
  (syntax-rules ()
    ((_ arg0 args ...) (let ((res arg0)) args ... res)))) 

(define *sym-num* 0)
(define (gensym)
  (set! *sym-num* (+ *sym-num* 1))
  (symbol (format "fs~d" *sym-num*) #f))

(define (read-lines in) 
  (let do-read ((line (read-line in 'concat))) 
  (if (eof-object? line) () (cons line (do-read (read-line in 'concat))))))


;;;;;;;;;;;;;;; globals

(define *verbose* #f)
(define (verbose-mode v) (set! *verbose* v))

(define *verbosep* #f)
(define (verbosep-mode v) (set! *verbosep* v))

(define *current-directory* ".")

(define *prepare-char-set* char-set:ascii)

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
  (printf "~a~a" (syn-indent-cnt-up) (apply format fmt args)))
(define (printi-dn fmt . args)
  (printf "~a~a" (syn-indent-cnt-dn) (apply format fmt args)))

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
  (when *verbosep* (printf "~a~a" (pat-indent-cnt-up) (apply format fmt args))))
(define (printip-dn fmt . args)
  (when *verbosep* (printf "~a~a" (pat-indent-cnt-dn) (apply format fmt args))))

;;;;;;;; forest functions

(define *ext-dir* "ext")
(define *ext-dir-list* (directory-list *ext-dir*))
(define (find-grammar-for-ext infile)
  (let* ((ext (path-extension infile))
         (lst (filter (lambda (fil) (let ((idx (or (string-index-right fil #\.) 0))) 
                                      (equal? ext (substring fil (+ (or (string-index-right fil #\. 0 idx) -1) 1) idx)))) 
                      *ext-dir-list*)))
    (if (equal? ext "forest") #t 
        (let ((grammars (filter find-grammar-for-ext lst)))
          (if (pair? grammars) (string-append *ext-dir* "/" (car grammars)) #f)))))


;;;;;;;;;;;;;; data structure definitions 

(define-record-type term (make-term file start-pos end-pos name vals) term? 
  (file term-file set-term-file!)
  (start-pos term-start-pos set-term-start-pos!)
  (end-pos term-end-pos set-term-end-pos!)
  (name term-name set-term-name!)
  (vals term-vals set-term-vals!))

(define-record-type token (make-token file start-pos end-pos chars) token? 
  (file token-file set-token-file!)
  (start-pos token-start-pos set-token-start-pos!)
  (end-pos token-end-pos set-token-end-pos!)
  (chars token-chars set-token-chars!))

(define-record-type memo (make-memo post taken left-rec) memo? 
  (post memo-post set-memo-post!)
  (taken memo-taken set-memo-taken!)
  (left-rec memo-left-rec set-memo-left-rec!))

(define-record-type language (make-language name files patterns rules choices) language? 
  (name language-name set-language-name!)
  (files language-files set-language-files!)
  (patterns language-patterns set-language-patterns!)
  (rules language-rules set-language-rules!)
  (choices language-choices set-language-choices!))


(define (mt name . vals)
  (make-term '() '() '() name vals))

(define (token-equal? fst snd)
  (let ((fst-str (if (token? fst) (token-chars fst) fst))
        (snd-str (if (token? snd) (token-chars snd) snd)))
    (equal? fst-str snd-str)))

(define (term-equal? fst snd)
  (or (and (term? fst) (term? snd) 
           (token-equal? (term-name fst) (term-name snd))
           (eq? (length (term-vals fst)) (length (term-vals snd)))
           (every term-equal? (term-vals fst) (term-vals snd))) 
      (token-equal? fst snd)))


(define (format-record t)
  (define (display-record t port)
    (cond 
      ((term? t)
        (display "[" port)
        (display (term-name t) port)
        (let ((l (term-vals t))) 
          (unless (null? l)
            (display " : " port)
            (display-record (car l) port)
            (for-each (lambda (e)
              (display " " port)
              (display-record e port)) (cdr l))))
        (display "]" port))
      ((token? t)
        (display "#\"" port)
        (display (token-chars t) port)
        (display "\"" port))
      ((string? t) (write t port))
      ((char-set? t) (display "{" port) (char-set-for-each (lambda (c) (write c port) (display " " port)) t) (display "}" port))
      ((pair? t)
        (define (tr-next ls)
          (if (pair? ls) (cons (format-record (car ls)) (tr-next (cdr ls)))
            (if (null? ls) ls (format-record ls))))
        (display (tr-next t) port))
      (else (display t port))))
  (call-with-output-string (lambda (port) (display-record t port))))


;;;;;;;;;;;;;;;;;

(define (grown-rec? l1 l2)
  (if (not l1) #f
      (if (not l2) #t
          (> l1 l2))))

(define (any->string sym)
  (cond  
    ((string? sym) sym)
    ((token? sym) (token-chars sym))
    ((and (term? sym) (string=? (term-name sym) "string") (pair? (term-vals sym))) 
      (any->string (car (term-vals sym))))
    (else sym)))

(define (any->symbol sym)
  (if (symbol? sym) sym (string->symbol (any->string sym))))


;;;;;;;;; grammar hash table ;;;;;;;;;;;;;;
(define (rule-exists? sym lang)
  (hash-table-exists? (language-rules lang) (any->symbol sym)))

(define (rule-get sym lang)
  (hash-table-ref (language-rules lang) (any->symbol sym) (lambda () (throw 'user-error 'forest "nonterminal \"~s\" not found~%" sym))))

(define (rule-put! sym expr lang)
  (hash-table-set! (language-rules lang) (any->symbol sym) expr))

;;;;;;;;; pattern hash table ;;;;;;;;;;;;;;
(define (pattern-ref name lang)
  (hash-table-ref/default (language-patterns lang) (any->symbol name) '()))

(define (pattern-put! name lst lang)
  (hash-table-set! (language-patterns lang) (any->symbol name) lst))

;;;;;;;;;;;;;; main parsing functions

(define *grammar-paths* (list "." (string-append "." "/" "grammars")))

(define (search-file infile)
  (if (file-exists? infile) infile  
      (let ((p (find (lambda (p) (file-exists? (string-append p "/" infile))) *grammar-paths*)))
        (if p (string-append p "/" infile)
            (throw 'user-error 'search-file "File not found: ~a~%" infile)))))

(define (make-errormessage message file pos new-pos)
  (define (find-line lines nline nchar)
    (if (> nchar (string-length (car lines))) 
        (find-line (cdr lines) (+ nline 1) (- nchar (string-length (car lines))))
        (values (car lines) nline nchar)))
  (let*-values (((infile) (search-file file))
                ((lines) (call-with-input-file infile (lambda (in) (read-lines in))))
                ((line nline nchar) (find-line lines 1 pos)))
    (format "~a:~a: ~a~%~a~a~%" infile nline message line (string-append (pregexp-replace "\\S" (string-take line nchar) " ") "^"))))

(define (expand-term trm lang modify-lang)
  (cond ((or (token? trm) (string? trm)) trm)
        ((not (term? trm)) (throw 'user-error 'expand-term "called with something other than a term or token: ~s" trm))
        ((term? (term-name trm)) (when *verbosep* (printip "var-term ~a~%" (format-record trm))) trm)
        ((equal? (term-name trm) "unexpanded") (car (term-vals trm)))
        (else 
         (let pat-loop ((pats (reverse (pattern-ref (term-name trm) lang))))
           (if (null? pats) 
               (make-term (term-file trm) (term-start-pos trm) (term-end-pos trm) (term-name trm) 
                          (map (lambda (t) (expand-term t lang modify-lang)) (term-vals trm)))
               (let ((bnd (let match-loop ((bindings ())
                                           (pattern (car (car pats)))
                                           (trm trm))
                            (define (match-name n)
                              (if (and (term? n) (string=? (term-name n) "var")) 
                                  (match-loop bindings (term-name pattern) (term-name trm))
                                  (if (token-equal? (term-name trm) n) bindings #f)))
                            (when *verbosep* (printip-up "Matching ~a on ~a...~%" (format-record pattern) (format-record trm)))
                            (let ((res (if (term? pattern)
                                           (cond 
                                             ((and (string=? (term-name pattern) "var") (pair? (term-vals pattern)) (null? (cdr (term-vals pattern)))) 
                                                  (cons (cons (car (term-vals pattern)) trm) bindings))
                                             ((and (pair? (term-vals pattern))
                                                  (let ((tl (last (term-vals pattern))))
                                                    (and (term? tl) (string=? (term-name tl) "varlist") (pair? (term-vals tl)) (null? (cdr (term-vals tl)))))) 
                                              (let ((n (term-name pattern))
                                                    (r (drop-right (term-vals pattern) 1))
                                                    (v (car (term-vals (last (term-vals pattern))))))
                                                (if (and (term? trm) (>= (length (term-vals trm)) (length r)))
                                                    (let* ((b (match-name n))
                                                           (bnd (fold (lambda (pat val b) (if b (match-loop b pat val) #f)) b r (take (term-vals trm) (length r)))))
                                                      (if bnd (cons (cons v (drop (term-vals trm) (length r))) bnd) #f)) #f)))
                                             (else (if (and (term? trm) (eq? (length (term-vals pattern)) (length (term-vals trm))))
                                                         (let ((b (match-name (term-name pattern))))
                                                           (fold (lambda (pat val b) (if b (match-loop b pat val) #f)) b (term-vals pattern) (term-vals trm))) #f)))
                                           (if (token? pattern) 
                                               (begin (when *verbosep* (printip "Matching token ~a with ~a~%" (format-record pattern) (format-record trm)))
                                                      (if (token-equal? pattern trm) bindings #f))
                                               (eprintf "Found illegal pattern while matching: ~s~%" (format-record pattern))))))
                              (when *verbosep* (printip-dn "Matched ~a : ~a on ~a...~%" (format-record res) (format-record pattern) (format-record trm)))
                              res))))
                 (if (not bnd) 
                     (pat-loop (cdr pats))
                     (let* ((__ (when *verbosep* (printip-up "Repl before ~a on ~a using ~a~%" (format-record (car (car pats))) (format-record trm) (format-record bnd))))
                            (bnd (map (lambda (kv) (if (or (pair? (cdr kv)) (null? (cdr kv)))
                                                        (cons (car kv) (map (lambda (t) (expand-term t lang modify-lang)) (cdr kv)))
                                                        (cons (car kv) (expand-term (cdr kv) lang modify-lang)))) bnd))
                             (_ (when *verbosep* (printip "Repl after ~a on ~a using ~a~%" (format-record (car (car pats))) (format-record trm) (format-record bnd))))
                             (res
                               (expand-term (let replace-loop ((repl (cdr (car pats))))
                                              (cond
                                                ((procedure? repl) (apply repl lang modify-lang bnd))
                                                ((and (term? repl) (string=? (term-name repl) "var"))
                                                   (let* ((v (car (term-vals repl)))
                                                          (val (assoc v bnd token-equal?))) 
                                                      (if val (cdr val)
                                                              (eprintf (make-errormessage (format "Variable ~a not found in pattern definition '~a'~%" 
                                                                (token-chars v) (term-name trm)) (token-file v) (token-start-pos v) (token-end-pos v))))))
                                                ((term? repl) (let ((l1 (term-file repl))
                                                                    (l2 (term-start-pos repl))
                                                                    (l3 (term-end-pos repl))
                                                                    (n (term-name repl))
                                                                    (r (term-vals repl)))
                                                                 (make-term l1 l2 l3 (replace-loop n) 
                                                                       (fold-right (lambda (v r) (if (and (term? v) (string=? (term-name v) "varlist")) 
                                                                                                     (append (cdr (assoc (car (term-vals v)) bnd token-equal?)) r)
                                                                                                     (cons (replace-loop v) r)))
                                                                                   '() r))))
                                                (else repl))) lang modify-lang)))
                          (when *verbosep* (printip-dn "Replaced ~a with ~a~%" (format-record trm) (format-record res)))
                          res))))))))


(define (prepare-language-choices exp lang) 
  (define (charset-to-try exp syms)
   (if (not (term? exp))
     (cond ((char? exp) (char-set exp))
         ((char-set? exp) exp)
         (else (throw 'user-error 'parse "Unknown or illegal parser action \"~a\"~% This error should never be reported. (You found a bug!)~%" exp)))
     (case (string->symbol (term-name exp))
      ((name) 
      (let* ((names (term-vals exp)) 
           (exp (apply string-append (map any->string names))))
        (if (member exp syms) *prepare-char-set* (charset-to-try (rule-get exp lang) (cons exp syms)))))      
      ((string)
      (let ((str (car (term-vals exp))))
        (char-set (string-ref (any->string str) 0))))
      ((+)  ; concatenation
       (let skip-not ((args (term-vals exp)))
       (if (and (term? (car args)) (string=? (term-name (car args)) "!"))
         (skip-not (cdr args))
         (if (null? args) *prepare-char-set* (charset-to-try (car args) syms))))) ; go through all args so we will find all choice terms
      ((/ //) ; choice or parallel choice
      (let* ((args (term-vals exp))
             (sets (map (lambda (exp) (charset-to-try exp syms)) args)))
        (apply char-set-union sets)))
      ((@)  ; gather named term
      (let ((name (car (term-vals exp)))
          (arg (cadr (term-vals exp))))
       (charset-to-try arg syms)))
      ((< > ^> ^=) ; capture literal, output, indent, same indentation
       (charset-to-try (car (term-vals exp)) syms))
      ((null ! $ ^<)  ; null, not, parsing error, dedent
       *prepare-char-set*)
      
      (else (let ((name (term-name exp)) (arg (term-vals exp))) ; anything else
          (throw 'user-error 'parse "Term \"~a\" is not a valid parser action. Perhaps a Pattern of that name is not defined correctly in imported grammars.~%Expression was \"~a\"~%" name exp)))
      )))
  
  (define (prepare-loop exp syms)
    (if (term? exp)
     (case (string->symbol (term-name exp))
      ((name) 
       (let* ((names (term-vals exp))
              (name (apply string-append (map any->string names)))) 
         (if (member name syms) syms (prepare-loop (rule-get name lang) (cons name syms)))))      
      ((+) ; concatenation
       (let ((args (term-vals exp)))
         (fold-right (lambda (exp syms) (prepare-loop exp syms)) syms args))) ; go through all args so we will find all choice terms
      ((/ //) ; choice or parallel choice
     (let ((args (term-vals exp)))
         (fold-right (lambda (exp syms) 
                (hash-table-set! (language-choices lang) exp (charset-to-try exp '()))
                (prepare-loop exp syms)) syms args)))
      ((! < > $ ^> ^=)
        (let ((arg (car (term-vals exp))))
          (prepare-loop arg syms)))          
      ((@)
        (let ((arg (cadr (term-vals exp))))
          (prepare-loop arg syms)))
      (else syms)) 
      syms))
  
  (prepare-loop exp '()))


(define *lang-cache* (make-hash-table))

(define lang-num 0)

(define (read-grammar infile modify-lang)
  (when (or *verbose* *verbosep*) (printf "reading grammar ~a~%" infile))          
  (let* ((files (cons infile (language-files modify-lang)))
         (cached-lang (hash-table-ref/default *lang-cache* files #f)))
    (printf "cached-lang: ~a (~a)~%" cached-lang files)
    (if cached-lang (begin (set-language-files! modify-lang files)
                           (set-language-patterns! modify-lang (hash-table-copy (language-patterns cached-lang)))
                           (set-language-rules! modify-lang (hash-table-copy (language-rules cached-lang)))
                           (set-language-choices! modify-lang (hash-table-copy (language-choices cached-lang)))) 
        (let ((verbose *verbose*)
              (verbosep *verbosep*))
          (set! *verbose* #f) (set! *verbosep* #f)
          (let* ((res (parse-file infile (lambda (read-lang terms) (expand-term (car terms) read-lang modify-lang)))))
        (set! *verbose* verbose) (set! *verbosep* verbosep)
            (unless res
              (throw 'user-error "Error reading file" infile))
            (set-language-files! modify-lang files)
            (set! lang-num (+ lang-num 1))
            (prepare-language-choices (mt "name" "start") modify-lang)
            (let ((cached-lang (make-language lang-num files 
                                                           (hash-table-copy (language-patterns modify-lang)) 
                                                           (hash-table-copy (language-rules modify-lang)) 
                                                           (hash-table-copy (language-choices modify-lang)))))
              (hash-table-set! *lang-cache* files cached-lang)))))
    (when (or *verbose* *verbosep*) (printf "Done reading grammar ~a: ~a ~%" infile (language-files modify-lang)))))

; init
(define (make-lang)
  (set! lang-num (+ lang-num 1))
  (let ((lang (make-language lang-num (list) (make-hash-table) (make-hash-table) (make-hash-table))))
    
    ; initial character and charset bindings
    (rule-put! 'lowercasechar   (char-set-intersection *prepare-char-set* char-set:lower-case)  lang)
    (rule-put! 'uppercasechar   (char-set-intersection *prepare-char-set* char-set:upper-case)  lang)
    (rule-put! 'letterchar      (char-set-intersection *prepare-char-set* char-set:letter)      lang)
    (rule-put! 'digitchar       (char-set-intersection *prepare-char-set* char-set:digit)       lang)
    (rule-put! 'whitespacechar  (char-set-intersection *prepare-char-set* char-set:whitespace)  lang)
    (rule-put! 'hexdigitchar    (char-set-intersection *prepare-char-set* char-set:hex-digit)   lang)
    (rule-put! 'blankchar       (char-set-intersection *prepare-char-set* char-set:blank)       lang)
    (rule-put! 'asciichar       (char-set-intersection *prepare-char-set* char-set:ascii)       lang)
    (rule-put! 'anychar         (char-set-intersection *prepare-char-set* char-set:full)        lang)
    
    (rule-put! 'tabchar         #\tab      lang)
    (rule-put! 'linefeedchar    #\linefeed lang)
    (rule-put! 'returnchar      #\return   lang)
    (rule-put! 'backslashchar   #\\        lang)
    (rule-put! 'singlequotechar #\'        lang)
    (rule-put! 'doublequotechar #\"        lang)
    
    (pattern-put! "pattern" (list (cons (mt "pattern" (mt "var" "pat") (mt "var" "repl")) 
                                        (lambda (real-lang lang repl-arg pat-arg) 
                                           (let* ((repl (cdr repl-arg))
                              (pat (cdr pat-arg))
                          (name (term-name pat)))
                                             (when *verbosep* (printip "add Pattern to (lang ~a) ~a : ~a => ~a~%"  (language-name lang) name 
                                               (format-record pat) (format-record repl)))
                                             (pattern-put! name (cons (cons pat repl) (pattern-ref name lang)) lang)
                                             (mt "name" name))))) lang)
    (pattern-put! "rule" (list (cons (mt "rule" (mt "name" (mt "varlist" "names")) (mt "var" "rule")) 
                                     (lambda (read-lang lang rule-arg names-arg)
                                       (let* ((rule (cdr rule-arg))
                            (names (cdr names-arg))
                        (name (apply string-append (map any->string names)))) 
                     (rule-put! name rule lang) (mt "name" name))))) lang) 
    (pattern-put! "newname" (list (cons (mt "newname") 
                                        (lambda (read-lang lang) 
                      (mt "name" (symbol->string (gensym)))))) lang)  
    (pattern-put! "import" (list (cons (mt "import" (mt "var" "fil")) 
                                       (lambda (read-lang lang fil-arg) 
                                          (let* ((fil (cdr fil-arg))
                             (f (any->string fil)))
                                            (when *verbosep* (printip "Import ~s, ~a ~a~%" f (language-name read-lang) (language-name lang)))
                                            (catch 'user-error (lambda () (read-grammar f read-lang)) 
                                              (lambda (exn . args) (eprintf (make-errormessage (format-record args) (token-file fil) (token-start-pos fil) (token-end-pos fil)))))
                                            (mt "null"))))) lang)
    (pattern-put! "insert" (list (cons (mt "insert" (mt "name" (mt "varlist" "names")) (mt "var" "rule"))  
                                       (lambda (read-lang lang rule-arg names-arg) 
                                          (let* ((rule (cdr rule-arg))
                                                 (names (cdr names-arg))
                                                 (name (apply string-append (map any->string names))))
                                            (if (rule-exists? name lang) 
                                                (let ((term (rule-get name lang)))
                                                  (when (not (and (term? term) (token-equal? (term-name term) "//")))
                                                    (error 'Insert "insertion in something other than // : ~s -- ~s" name rule))
                                                  (when *verbosep* (printip "Insert into (lang ~a) ~s : ~a == ~a~%"  (language-name lang) name (format-record rule) (format-record term)))
                                                  (rule-put! name (apply mt (term-name term) (lset-union term-equal? (list rule) (term-vals term))) lang))
                                                (rule-put! name (mt "//" rule) lang))
                                            rule)))) lang)
    (pattern-put! "term" (list (cons (mt "term" (mt "var" "var") (mt "varlist" "vals")) 
                                     (lambda (read-lang lang vals-arg var-arg) 
                                        (let* ((vals (cdr vals-arg))
                                               (var (cdr var-arg))
                                               (new-term (make-term '() '() '() var vals)))
                                          (when *verbosep* (printip "Creating variable term ~a~%" (format-record new-term)))
                                          new-term)))
                               (cons (mt "term" (mt "name" (mt "varlist" "names")) (mt "varlist" "vals")) 
                                     (lambda (read-lang lang vals-arg names-arg) 
                                        (let* ((vals (cdr vals-arg))
                                               (names (cdr names-arg))
                                               (name (apply string-append (map any->string names)))
                                               (new-term (make-term '() '() '() (if (token? name) (token-chars name) name) vals)))
                                          (when *verbosep* (printip "Creating term ~a~%" (format-record new-term)))
                                          new-term)))) lang)
    (pattern-put! "token" (list (cons (mt "token" (mt "var" "chars")) 
                                      (lambda (read-lang lang tok-arg) 
                      (let ((tok (cdr tok-arg)))
                                         (unless (token? tok) (throw 'user-error 'token "Content of token term must be a token, found ~a" tok))
                                         (when *verbosep* (printip "Creating token ~a~%" (format-record tok)))
                                         tok)))) lang)
    lang))

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

(define (get-lang-for-reading infile)
  (let ((lang (make-lang))
        (grammar-file (find-grammar-for-ext infile)))
    (if grammar-file 
        (if (not (eq? #t grammar-file)) (read-grammar grammar-file lang) (load-core-lang lang))
        (eprintf "No grammar definition found for file ~a~%" infile))
    lang))

(define (parse-file infile process-term)
  
  (when *verbose* (printf " parsing ~s ...~%" infile))
  (let* ((lang (get-lang-for-reading infile))
         (infile (search-file infile))
         (str (apply string-append (call-with-input-file infile (lambda (in) (read-lines in)))))
         (len (string-length str))
         (memo-hash (make-hash-table))
         (indent-ls ()))

    (define (invalidate-caches)
      (set! memo-hash (make-hash-table)))
    
    (define (rule-memo sym)
      (hash-table-ref memo-hash sym (lambda () (let ((h (make-hash-table))) (hash-table-set! memo-hash sym h) h))))
    
    (define (parse exp pos path-ls)
      (if *verbose* 
          (let* ((exp-str (format-record exp))
                 (formatted-exp (if (> (string-length exp-str) 60) (string-append (string-take exp-str 60) " ...") exp-str)))
            (printf "'~a'@~a~a~a~%" (and (< pos len) (string-ref str pos)) pos (syn-indent-cnt-up) formatted-exp)
            (let-values (((new-pos taken) (parse-inner exp pos path-ls)))
              (printf "-> @~a~a~a : ~a~%" new-pos (syn-indent-cnt-dn) (format-record taken) formatted-exp)
              (values new-pos taken)))
          (parse-inner exp pos path-ls)))
    
    (define (parse-inner exp pos path-ls)
      (if (not (term? exp))
       (cond
          ((char? exp) (values (if (and (< pos len) (char=? exp (string-ref str pos))) (+ pos 1) #f) ()))
          ((char-set? exp) (values (if (and (< pos len) (char-set-contains? exp (string-ref str pos))) (+ pos 1) #f) ()))
          (else (throw 'user-error 'parse "unknown or illegal parser action \"~s\"~%" exp)))
       (case (string->symbol (term-name exp))
        ((null) (values pos ()))
        ((name) 
         (let* ((names (term-vals exp))
                (exp (string->symbol (apply string-append (map any->string names))))
                (rule (rule-get exp lang))
                (memo (hash-table-ref/default (rule-memo exp) pos #f)))
           (if memo
               (if (and (memo-left-rec memo) (not (eq? (memo-left-rec memo) exp))) 
                   (parse rule pos (cons exp path-ls)) 
                   (begin        
                     (if (member exp path-ls) ; left recursion
                         (begin (when *verbose* (printi "     start of left recursion ~s~%" exp))
                                (set-memo-left-rec! memo exp)
                                (let path-loop ((path path-ls))
                                  (when (not (eq? (car path) exp))
                                    (set-memo-left-rec! (hash-table-ref/default (rule-memo (car path)) pos #f) exp) ; TODO: check this line
                                    (path-loop (cdr path)))))
                         (when *verbose* (printi "     cached ~s~%" exp)))
                     (values (memo-post memo) (memo-taken memo))))
               (let ((memo (make-memo #f () #f)))
                 (hash-table-set! (rule-memo exp) pos memo)
                 (let recurse-loop ()
                   (let*-values (((post taken) (parse rule pos (cons exp path-ls))))
                     (if (memo-left-rec memo) ; now we're back down in left recursion
                         (begin (when *verbose* (printi "   --> ~s Left recursion : ~a, ~a~%" exp post taken))
                                (if (grown-rec? post (memo-post memo))
                                    ; we made progress parsing the left recursion. Save result and try again
                                    (begin (when *verbose* (printi "       go again (~a ~a) : ~a~%" post (memo-post memo) (grown-rec? post (memo-post memo))))
                                           (set-memo-post! memo post)
                                           (set-memo-taken! memo taken)
                                           (if (eq? (memo-left-rec memo) exp)
                                               (recurse-loop)
                                               (values post taken)))
                                    ; one recursion too many. return previous values
                                    (begin (when *verbose* (printi "       last time (~a ~a) : ~a~%" post (memo-post memo) (grown-rec? post (memo-post memo))))
                                           (set-memo-left-rec! memo #f)
                                           (values (memo-post memo) (memo-taken memo)))))
                         (begin
                           (set-memo-post! memo post)
                           (set-memo-taken! memo taken)
                           (values post taken)))))))))
        ((string) 
         (let string-loop ((exp (string->list (any->string (car (term-vals exp))))) (chls pos))
           (if (null? exp) (values chls ()) 
               (let-values (((new-pos taken) (parse (car exp) chls ())))
                 (if new-pos (string-loop (cdr exp) new-pos) (values #f ()))))))
        ((+) ; concatenation
         (let concat-loop ((args (term-vals exp)) (curr-ls pos) (term ()))
           (if (null? args) (values curr-ls (reverse! term))
               (let-values (((new-pos taken) (parse (car args) curr-ls (if (eq? curr-ls pos) path-ls ()))))
                 (if new-pos 
                     (concat-loop (cdr args) new-pos (append-reverse taken term))
                     (values #f ()))))))
        ((/ //) ; choice or parallel choice
         (let* ((all-args (term-vals exp))
                (args (if (< pos len) (filter (lambda exp (char-set-contains? (hash-table-ref/default (language-choices lang) exp char-set:full) (string-ref str pos))) 
                                              all-args) all-args)))
           (let choice-loop ((args args))
             (if (null? args) (values #f ())
                 (let-values (((new-pos taken) (parse (car args) pos path-ls)))
                   (if new-pos (values new-pos taken) 
                       #;(if (equal? (term-name exp) "/") (values new-pos taken)
                             (let ((success-args (filter (lambda (arg) (let-values (((new-pos taken) (parse arg pos path-ls)))
                                                                         new-pos)) (cdr args))))
                               (unless (null? success-args)
                                 (throw 'user-error 'parse "parallel choice matches more than one alternative: \"~a\"~%" 
                                                   (apply string-append (map (lambda (a) (format "~%    ~a" a)) (cons (car args) success-args)))))
                               (values new-pos taken))) (choice-loop (cdr args))))))))
        ((!) ; not
         (let*-values (((arg) (car (term-vals exp)))
                       ((new-pos taken) (parse arg pos path-ls)))
           (values (if new-pos #f pos) ())))
        ((<) ; capture literal
         (let*-values (((arg) (car (term-vals exp)))
                       ((new-pos taken) (parse arg pos path-ls)))
           (let ((res (list (if new-pos (make-token infile pos new-pos (substring str pos new-pos)) #f))))
             (values new-pos res))))
        ((@) ; gather named term
         (let*-values (((name-arg) (car (term-vals exp)))
                       ((name) (if (and (term? name-arg) (string=? (term-name name-arg) "string")) (car (term-vals name-arg)) name-arg))
                       ((arg) (cadr (term-vals exp)))
                       ((new-pos taken) (parse arg pos path-ls)))
           (if new-pos
               (let ((new-term (make-term infile pos new-pos (any->string name) taken)))
                 (values new-pos (list new-term)))
               (values new-pos taken))))
        ((>) ; output
         (let*-values (((arg) (car (term-vals exp)))
                       ((new-pos taken) (parse arg pos path-ls)))
           (when new-pos
             (process-term lang taken))
           (values new-pos taken)))
        (($ (list-rest arg mess)) ; parsing error : print mess when arg fails
         (let*-values (((arg mess) (values (car (term-vals exp)) (cdr (term-vals exp))))
                       ((new-pos taken) (parse arg pos path-ls)))
           (unless new-pos (eprintf (make-errormessage (apply string-append (map (lambda (exp) (any->string exp)) mess)) infile pos new-pos)))
           (values new-pos taken)))
        ((^>) ; take indentation whitespace
         (let*-values (((arg) (car (term-vals exp)))
                       ((new-pos taken) (parse arg pos path-ls)))
           (when new-pos
             (when *verbose* (printf "^> ~s : ~s ~s~%" (- new-pos pos) (substring str pos new-pos) (map car indent-ls)))
             (set! indent-ls (cons (list (- new-pos pos) pos) indent-ls)))
           (values new-pos ())))
        ((^=) ; match indentation token
         (let*-values (((arg) (car (term-vals exp)))
                       ((new-pos taken) (parse arg pos path-ls)))
           (when *verbose* (printf "^= ~s : ~s ~s~%" (- new-pos pos) (substring str pos new-pos) (map car indent-ls)))
           (values (if (and new-pos (eq? (- new-pos pos) (caar indent-ls)))
                       new-pos #f) ())))
        ((^<) ; match indentation token
         (when *verbose* (printf "^< ~s -> ~s~%" (caar indent-ls) (caadr indent-ls)))
         (set! indent-ls (cdr indent-ls))
         (invalidate-caches)
         (values pos ())))))
    #;(with-handlers ((exn:fail:filesystem? 
                     (lambda (exn)
                       (fprintf (current-error-port) "~a~%" (exn-message exn)) #f))
                    (exn:fail:user? 
                     (lambda (exn)
                       (fprintf (current-error-port) "error parsing ~s: ~a~%" infile (exn-message exn)) #f))
                    #;(exn:fail? 
                       (lambda (exn)
                         (fprintf (current-error-port) "error parsing ~s: ~a~%" infile (exn-message exn)) #f))))
	  
    (fluid-let ((*current-directory* (path-directory infile)))
      (let-values (((res taken) (parse (mt "name" "start") 0 ())))
        (unless (and (number? res) (= res len))
          (let ((pos (hash-table-fold memo-hash (lambda (k v a) (max a (hash-table-fold v (lambda (k v a) (if (memo-post v) (max (memo-post v) a) a)) 0))) 0)))
            (eprintf (make-errormessage "Could not parse. First char not parsed:" infile pos pos)))          
          #f)))))


;;;;;;;;; main program: command line parsing
(define (process-args argv)
  (cond 
    ((string=? (car argv) "-v") (verbose-mode #t) (process-args (cdr argv)))
    ((string=? (car argv) "-p") (verbosep-mode #t) (process-args (cdr argv)))
    (else argv)))

(define (main argv)
  (printf "argv: ~a~%" argv)
  (let ((infiles (process-args (cdr argv))))
    (exit (if (every (lambda (infile)
                        (let ((modify-lang (make-lang)))
                          (parse-file infile (lambda (read-lang terms) (printf "> ~a~%" (format-record (expand-term (car terms) read-lang modify-lang)))))))
                      infiles) 0 1))))


; interactive script
(main (command-line))
