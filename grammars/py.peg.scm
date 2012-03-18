; This needs to be at the top for the keyword macro to work
(def-rule Keyword (/))


(def-rule start (+ (^ ()) (? NEWLINE) (* (seq Statement SAME)) eof))


(def-rule Suite (/ (+ StatementList NEWLINE SAME) (+ NEWLINE INDENT (seq Statement SAME) DEDENT)))

(def-rule Statement (/ (+ StatementList NEWLINE) CompoundStatement))

(def-rule StatementList (tr-seq SimpleStatement semicolon))

(def-rule SimpleStatement 
  (/ (@$: "assert" Expression (?+ comma Expression))
     (@$: "pass")
     (@$: "del" (tr-list Target))
     (@$: "print" (/ (tr-list Expression) 
                     (+ ">>" Expression (tr-list Expression))))
     (@$: "return" (tr-list Expression))
     (@$: "yield" (tr-list Expression))
     (@$: "raise" (?+ Expression (?+ comma Expression (?+ comma Expression))))
     (@$: "break")
     (@$: "continue")
     (@$: "import" (list (+ DottedName (? (@$: "as" Identifier)))))
     (@$: "from" (/ (+ (* dot) DottedName) (+ dot (* dot))) 
          (@$: "import" (/ (list (+ Identifier (? (@$: "as" Identifier))))
                         (parens (tr-list (+ Identifier (? (@$: "as" Identifier)))))
                         all)))
     (@$: "global" (list Identifier))
     (@$: "exec" Expression (?+ (@$: "in" Expression (@ "locals" (?+ comma Expression)))))
     (@ "assignment" (+ (*1 (+ (tr-list Target) equals)) (tr-list Expression)))
     (@ "augmented-assignment" (+ Target AugOp (tr-list Expression)))
     (@ "expression-statement" (list Expression))))

(def-rule CompoundStatement 
  (/ (@$: "class" Identifier (? (@ "inheritance" (parens (tr-list Expression)))) colon Suite)
     (@.$: (* Decorator) "def" Identifier (? (parens ParameterList)) colon Suite)
     (@$: "with" Expression (? (@$: "as" Target)) colon Suite)
     (@$: "try" colon Suite 
          (* (@$: "except" Expression (?+ "," Target) colon Suite))
          (? (@$: "else" colon Suite))
          (? (@$: "finally" colon Suite)))
     (@$: "for" (list Target) (@$: "in" (list Expression)) colon Suite
          (? (@.$: SAME "else" colon Suite)))
     (@$: "while" Expression colon Suite
          (? (@.$: SAME "else" colon Suite)))
     (@$: "if" Expression colon Suite 
          (* (@.$: SAME "elif" Expression colon Suite))
          (? (@.$: SAME "else" colon Suite)))
     ))

(def-rule ParameterList (+ (list DefParameter) 
                          (?+ comma (? (/ (+ kwargs Identifier (? (+ comma restargs Identifier))) 
                                          (+ restargs Identifier))))))

(def-rule DefParameter (@ "param" (+ Parameter (? (@ "init" (+ equals Expression))))))

(def-rule Parameter (/ Identifier (@ "tuple" (parens (tr-list Parameter)))))

(def-rule AugOp (/ (@ "add-assign" (tok "+="))
                  (@ "sub-assign" (tok "-="))
                  (@ "mul-assign" (tok "*="))
                  (@ "div-assign" (tok "/="))
                  (@ "mod-assign" (tok "%="))
                  (@ "pwr-assign" (tok "**="))
                  (@ "lsh-assign" (tok "<<="))
                  (@ "rsh-assign" (tok ">>="))
                  (@ "and-assign" (tok "&="))
                  (@ "ior-assign" (tok "|="))
                  (@ "xor-assign" (tok "^="))))

(def-rule Target (/ (@ "sequence" (parens (tr-list Target)))
                   (@ "sequence" (brackets (tr-list Target)))
                   Primary))

(def-rule Decorator (+ (tok "@") DottedName (? (parens (? (+ ArgumentList (? comma))))) NEWLINE))


(def-rule Atom (/ Identifier Literal Enclosure)) 

(def-rule Primary (+ Atom (* (/ (brackets (/ (@ "subscription" (list Expression)) 
                                            (@: "short-slice" (? Expression) colon (? Expression)) 
                                            (@ "extended-slicing" (tr-list SliceItem))))
                               (@ "call" (parens (? (/ (+ ArgumentList (? comma)) (+ Expression GenexprFor)))))
                               (@ "attribute-ref" (+ dot Identifier)))))) 

(def-rule SliceItem (/ Expression ProperSlice ellipsis))

(def-rule ProperSlice (+ (? Expression) colon (? Expression) (?+ colon (? Expression))))

(def-rule Enclosure (/ (parens (/ (@ "generator" (+ Expression GenexprFor))
                                 (@ "tuple" (? (tr-list Expression)))
                                 YieldExpression)) 
                      (brackets (/ (@ "list-for" (+ Expression ListFor))
                                   (@ "list-display" (? (tr-list Expression))))) 
                      (braces (@ "dictionary" (? (tr-list (@ "dict-item" (+ Expression colon Expression)))))) 
                      (@ "string-conversion" (+ (tok "`") (tr-list Expression) (tok "`")))))

(def-rule GenexprFor (@$: "for" (tr-list Target) (@$: "in" Expression) (? (/ GenexprFor GenexprIf)))) 

(def-rule GenexprIf (@$: "if" Expression (? (/ GenexprFor GenexprIf)))) 

(def-rule ListFor (@$: "for" (tr-list Target) (@$: "in" (tr-list Expression)) 
                      (? (/ ListFor ListIf))))

(def-rule ListIf (@$: "if" Expression (? (/ ListFor ListIf))))


(def-rule ArgumentList (/ (+ PosArgs (?+ comma KwArgs) (?+ comma RestArgs) (?+ comma KwRestArgs))
                         (+ KwArgs (?+ comma RestArgs) (?+ comma KwRestArgs))
                         (+ RestArgs (?+ comma KwRestArgs))
                         (+ KwRestArgs)))

(def-rule PosArgs (list Expression))
(def-rule KwArgs (list (+ Identifier equals Expression)))
(def-rule RestArgs (+ kwargs Expression))
(def-rule KwRestArgs (+ restargs Expression))

(def-rule YieldExpression (@$: "yield" (tr-list Expression)))


(def-rule LambdaForm (@$: "lambda" (? ParameterList) colon Expression))

(def-rule Expression (/ (@ "infix" (seq UnaryExpression InfixOp))
                       LambdaForm))

(def-rule UnaryExpression (@ "unary" (+ (* PrefixOp) Primary)))

(def-rule InfixOp (/ (@ "is" (+ (kw "is") (? (kw "not"))))
                    (@ "in" (+ (? (kw "not")) (kw "in")))
                    (@ "eq" (tok "=="))
                    (@ "neq" (tok "!="))
                    (@ "uneq" (tok "<>"))
                    (@ "leq" (tok "<="))
                    (@ "geq" (tok ">="))
                    (@ "rsh" (tok ">>"))
                    (@ "lsh" (tok "<<"))
                    (@ "lt" (tok "<"))
                    (@ "gr" (tok ">"))
                    (@: "and")
                    (@: "or")
                    (@ "bit-and" (tok "&"))
                    (@ "bit-xor" (tok "^"))
                    (@ "bit-or" (tok "|"))
                    (@ "power" (tok "**"))
                    (@ "add" (tok "+"))
                    (@ "sub" (tok "-"))
                    (@ "mul" (tok "*"))
                    (@ "idiv" (tok "//"))
                    (@ "div" (tok "/"))
                    (@ "mod" (tok "%"))))

(def-rule PrefixOp (/ (@: "not")
                     (@ "inv" (tok "~"))
                     (@ "pos" (tok "+"))
                     (@ "neg" (tok "-"))))

;;;;;;;;;;;;;; Whitespace and Comments

(def-rule WhiteSpace (* (/ (+ (! EndOfLine) char-set:whitespace) (+ #\\ EndOfLine))))

(def-rule EndOfLine (/ (+ #\return #\newline) #\return #\newline))

(def-rule LineComment (+ #\# (* (+ (! EndOfLine) char-set:full))))

(def-rule NEWLINE (+ (? LineComment) EndOfLine (*+ WhiteSpace (? LineComment) EndOfLine) (^ WhiteSpace)))

(def-rule SAME (^= SAME))

(def-rule INDENT (^= INDENT))

(def-rule DEDENT (^= DEDENT))

;;;;;;;;;;;;;; Identifiers and Terminals

(def-rule Identifier (tok (< (+ (! (+ Keyword (! LetterOrDigit))) (/ char-set:letter #\_) (* LetterOrDigit)))))

(def-rule DottedName (seq Identifier dot))

(def-rule LetterOrDigit (/ char-set:letter+digit #\_))

(def-rule dot (tok #\.))

(def-rule semicolon (tok #\;))

(def-rule comma (tok #\,))

(def-rule colon (tok #\:))

(def-rule eof (! char-set:full))

; some tokens specially defined for Python

(def-rule ellipsis (kw "...")) 

(def-rule equals (tok #\=))

(def-rule kwargs (tok #\*))

(def-rule all (tok #\*))

(def-rule restargs (tok "**"))

;;;;;;;;;;;;;; Literals

(def-rule Literal (/ StringLiteral
                    ImagLiteral
                    FloatingPointLiteral
                    IntegerLiteral))

(def-rule StringLiteral (@ "string" (tok (< (+ (? StringPrefix) (/ LongString ShortString)))))) 
(def-rule StringPrefix (/ "r" "u" "ur" "R" "U" "UR" "Ur" "uR"))
(def-rule ShortString (/ (+ #\' (* (+ (! #\') ShortStringChar)) #\') (+ #\" (* (+ (! #\") ShortStringChar)) #\")))
(def-rule LongString (/ (+ "'''"    (* (+ (! "'''")    LongStringChar)) "'''") 
                       (+ "\"\"\"" (* (+ (! "\"\"\"") LongStringChar)) "\"\"\""))) 
(def-rule ShortStringChar (/ (+ (! #\\) (! EndOfLine) char-set:printing) EscapeSeq)) 
(def-rule LongStringChar (/ (+ (! #\\) char-set:printing) EscapeSeq)) 
(def-rule EscapeSeq (+ #\\ char-set:ascii)) 

(def-rule IntegerLiteral (@ "integer" (tok (< (+ (/ HexInteger OctalInteger DecimalInteger) (? (/ "l" "L"))))))) 
(def-rule DecimalInteger (*1 char-set:digit)) 
(def-rule OctalInteger (+ "0" (*1 OctalDigit))) 
(def-rule HexInteger (+ (/"0x"  "0X") (*1 char-set:hex-digit))) 
(def-rule OctalDigit (/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

(def-rule FloatingPointLiteral (@ "float" (tok (< (/ PointFloat ExponentFloat))))) 
(def-rule PointFloat (/ (+ (? IntegerPart) Fraction) (+ IntegerPart "."))) 
(def-rule ExponentFloat (+ (/ IntegerPart PointFloat) Exponent)) 
(def-rule IntegerPart (*1 char-set:digit)) 
(def-rule Fraction (+ "." (*1 char-set:digit))) 
(def-rule Exponent (+ (/ "e" "E") (? (/ "+" "-")) (*1 char-set:digit))) 

(def-rule ImagLiteral (@ "imaginary" (tok (< (+ (/ FloatingPointLiteral IntegerPart) (/ "j" "J")))))) 
