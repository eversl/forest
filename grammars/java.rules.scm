; Definitions for syntax extensions needed to read the patterns 
(Import "forest.patterns.scm")
(Import "forest.rules.scm")

;(Pattern (oper 'c) (+ 'c WhiteSpace))
(Insert Sexpr (@ "oper" (+ (parens (+ (sym "oper") Sexpr)))))

;(Pattern (seq 'i 's) (+ 'i (* (+ 's 'i))))
(Insert Sexpr (@ "seq" (+ (parens (+ (sym "seq") Sexpr Sexpr)))))

;(Pattern (tr-seq 'i 's) (+ (? (seq 'i 's)) (? 's)))
(Insert Sexpr (@ "tr-seq" (+ (parens (+ (sym "tr-seq") Sexpr Sexpr)))))

;(Pattern (list 'c) (seq 'c comma))
(Insert Sexpr (@ "list" (+ (parens (+ (sym "list") Sexpr)))))

;(Pattern (tr-list 'c) (tr-seq 'c comma))
(Insert Sexpr (@ "tr-list" (+ (parens (+ (sym "tr-list") Sexpr)))))

;(Pattern (parens 'c) (+ (tok "(") 'c (tok ")")))

;(Pattern (brackets 'c) (+ (tok "[") 'c (tok "]")))
(Insert Sexpr (@ "brackets" (parens (+ (sym "brackets") Sexpr))))
;(Pattern (braces 'c) (+ (tok "{") 'c (tok "}")))
(Insert Sexpr (@ "braces" (+ (parens (+ (sym "braces") Sexpr)))))
;(Pattern (chevrons 'c) (+ (tok "<") 'c (tok ">")))
(Insert Sexpr (@ "chevrons" (+ (parens (+ (sym "chevrons") Sexpr)))))

;(Pattern (parens$ 'c) (/ (+ (tok "(") 'c (tok ")")) ($ "could not find closing ')'" (tok "("))))
(Insert Sexpr (@ "parens$" (parens (+ (sym "parens$") Sexpr))))
;(Pattern (brackets$ 'c) (/ (+ (tok "[") 'c (tok "]")) ($ "could not find closing ']'" (tok "["))))
(Insert Sexpr (@ "brackets$" (parens (+ (sym "brackets$") Sexpr))))
;(Pattern (braces$ 'c) (/ (+ (tok "{") 'c (tok "}")) ($ "could not find closing '}'" (tok "{"))))
(Insert Sexpr (@ "braces$" (parens (+ (sym "braces$") Sexpr))))
;(Pattern (chevrons$ 'c) (/ (+ (tok "<") 'c (tok ">")) ($ "could not find closing '>'" (tok "<"))))
(Insert Sexpr (@ "chevrons$" (parens (+ (sym "chevrons$") Sexpr))))

;(Pattern ($$ 'a1 'a2) (/ (+ 'a1 'a2) (+ (& 'a1) ($ 'a1 'a1 "expression not completed"))))
(Insert Sexpr (@ "$$" (parens (+ (sym "$$") Sexpr Sexpr))))

;(Pattern (@: 'n ,r) (@ 'n (+ (kw 'n) ,r)))
(Insert Sexpr (@ "@:" (parens (+ (sym "@:") Sexpr Sexprs))))
;(Pattern (@$ 'n ,r) (@ 'n ($ 'n (+ ,r))))
(Insert Sexpr (@ "@$" (parens (+ (sym "@$") Sexpr Sexprs))))
;(Pattern (@$: 'n ,r) (@ 'n ($ (kw 'n) (+ ,r))))
(Insert Sexpr (@ "@$:" (parens (+ (sym "@$:") Sexpr Sexprs))))
;(Pattern (@.: 'f 'n ,r) (@ 'n (+ 'f (kw 'n) ,r)))
(Insert Sexpr (@ "@.:" (parens (+ (sym "@.:") Sexpr Sexpr Sexprs))))
;(Pattern (@.$: 'f 'n ,r) (@ 'n (+ 'f ($ (kw 'n) (+ ,r)))))
(Insert Sexpr (@ "@.$:" (parens (+ (sym "@.$:") Sexpr Sexpr Sexprs))))

;(Pattern (kw 'c) (+ (choice-append! Keyword (+ 'c (! LetterOrDigit))) WhiteSpace))
(Insert Sexpr (@ "kw" (parens (+ (sym "kw") Sexpr))))
