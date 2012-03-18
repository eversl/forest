
;Defines the patterns needed to read forest.peg.scm

(Insert Sexpr (@ "%*%" (+ "(" WhiteSpace "%*%" (! NameChar) WhiteSpace Sexpr Sexpr ")" WhiteSpace)))
(Insert Sexpr (@ "*" (+ "(" WhiteSpace "*" (! NameChar) WhiteSpace Sexpr ")" WhiteSpace)))
(Insert Sexpr (@ "tok" (+ "(" WhiteSpace "tok" (! NameChar) WhiteSpace Sexpr ")" WhiteSpace)))
(Insert Sexpr (@ "parens" (+ "(" WhiteSpace "parens" (! NameChar) WhiteSpace Sexpr ")" WhiteSpace)))
(Insert Sexpr (@ "sym" (+ "(" WhiteSpace "sym" (! NameChar) WhiteSpace Sexpr ")" WhiteSpace)))

(Insert Sexpr (@ "?" (+ "(" WhiteSpace "?" WhiteSpace Sexpr ")" WhiteSpace)))
(Insert Sexpr (@ "&" (+ "(" WhiteSpace "&" WhiteSpace Sexpr ")" WhiteSpace)))

