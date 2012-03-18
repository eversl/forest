; Definitions for patterns needed to read the java grammar definitions 

(Import "forest.patterns.scm")
(Import "forest.rules.scm")

(Import "java.rules.scm")

(Pattern (oper 'c) (+ 'c WhiteSpace))
(Pattern (seq 'i 's) (+ 'i (* (+ 's 'i))))
(Pattern (tr-seq 'i 's) (+ (? (seq 'i 's)) (? 's)))
(Pattern (list 'c) (seq 'c comma))
(Pattern (tr-list 'c) (tr-seq 'c comma))

(Pattern (parens 'c) (+ (tok "(") 'c (tok ")")))
(Pattern (brackets 'c) (+ (tok "[") 'c (tok "]")))
(Pattern (braces 'c) (+ (tok "{") 'c (tok "}")))
(Pattern (chevrons 'c) (+ (tok "<") 'c (tok ">")))

(Pattern (parens$ 'c) (+ (tok "(") ($ (+ 'c (tok ")")) "could not find closing ')'")))
(Pattern (brackets$ 'c) (+ (tok "[") ($ (+ 'c (tok "]")) "could not find closing ']'")))
(Pattern (braces$ 'c) (+ (tok "{") ($ (+ 'c (tok "}")) "could not find closing '}'")))
(Pattern (chevrons$ 'c) (+ (tok "<") ($ (+ 'c (tok ">")) "could not find closing '>'")))

(Pattern (@: 'n ,r) (@ 'n (+ (kw 'n) ,r)))
(Pattern (@$: 'n ,r) (@ 'n (+ (kw 'n) ($ (+ ,r) 'n ": statement not completed"))))
(Pattern (@.: 'f 'n ,r) (@ 'n (+ 'f (kw 'n) ,r)))
(Pattern (@.$: 'f 'n ,r) (@ 'n (+ 'f (kw 'n) ($ (+ ,r) 'n ": statement not completed"))))

(Pattern (kw 'c) (+ (Insert Keyword (+ 'c (! LetterOrDigit))) WhiteSpace))


