
;Defines the patterns needed to read forest.peg.scm

(Import "forest.rules.scm")

(Pattern (* 'c) (%*% (NewName) 'c))
(Pattern (%*% 'n 'c) (Rule 'n (/ (+ 'c 'n) ())))
(Pattern (tok 'c) (+ 'c WhiteSpace))
(Pattern (parens 'c) (+ (tok "(") 'c (tok ")")))
(Pattern (sym 'c) (tok (+ 'c (! NameChar))))
             
(Pattern (? 'c) (/ 'c ()))
(Pattern (& 'c) (! (! 'c)))
