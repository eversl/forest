
; Syntax definition for java lang spec 1.0

(Import "forest.patterns.scm")
(Import "forest.rules.scm")

(Import "java.patterns.scm")
(Import "java.rules.scm")

(Rule Keyword (//))

; start parsing here
(Rule start (+ WhiteSpace CompilationUnit EndOfFile))
; start = WhiteSpace CompilationUnit EndOfFile

(Rule CompilationUnit (+ (? (> PackageDeclaration)) (* (> LanguageDeclaration)) (* (> ImportDeclaration)) (* (> TypeDeclaration))))

(Rule LanguageDeclaration (@$: "language" StringLiteral semicolon$))
(TermPattern [language : [string : 'v]] (Unexpanded [Import : 'v]))

(Rule PackageDeclaration (@$: "package" QualifiedIdentifier semicolon$))

(Rule ImportDeclaration (@$: "import" (? (@: "static")) 
                             (+ QualifiedIdentifier (? (+ dot (@ "all" (tok "*")))))
                             semicolon$))

(Rule TypeDeclaration (// ClassDeclaration InterfaceDeclaration semicolon))


;;;;;;;;;;;;;; Class Declaration

(Rule ClassDeclaration (@.$: Modifiers "class" Identifier 
                             (? (@$: "extends" Type))
                             (? (@$: "implements" (list Type)))
                             ClassBody))

(Rule ClassBody (body-braces$ (* ClassBodyDeclaration)))

(Rule ClassBodyDeclaration (// (@ "initializer" (+ (? (@: "static")) Block))
                               (@ "method" (+ Modifiers MethodDeclarator))
                               (@ "constructor" (+ Modifiers ConstructorDeclarator))
                               (@ "field" (+ Modifiers Type VariableDeclarators semicolon))
                               semicolon))

(Rule MethodDeclarator (+ Type Identifier FormalParameters (* Dim) (? ThrowsPart) (/ Block semicolon)))

(Rule ConstructorDeclarator (+ Identifier FormalParameters
                               (? ThrowsPart) Block))

(Rule ThrowsPart (@$: "throws" (list Type)))


;;;;;;;;;;;;;; Interface Declaration

(Rule InterfaceDeclaration (@.$: Modifiers "interface" Identifier (? (@$: "extends" (list Type))) (body-braces$ (* InterfaceBodyDeclaration))))

(Rule InterfaceBodyDeclaration (// (@ "constant" (+ Modifiers Type (list ConstantDeclarator) semicolon$))
                                   (@ "method" (+ Modifiers InterfaceMethodDeclarator))
                                   semicolon))

(Rule InterfaceMethodDeclarator (+ Type Identifier FormalParameters (* Dim) (? ThrowsPart) semicolon$))

(Rule ConstantDeclarator (+ Identifier (* Dim) (tok "=") VariableInitializer))

;;;;;;;;;;;;;; Variable Declaration

(Rule LocalVariableDeclarationStatement (@ "local-var" (+ VariableModifiers Type VariableDeclarators semicolon)))

(Rule VariableDeclarators (list VariableDeclarator))

(Rule VariableDeclarator (@ "var-declarator" (+ Identifier (* Dim) 
                                                (? (@ "var-init" (+ (tok "=") VariableInitializer))))))

(Rule VariableInitializer (/ ArrayInitializer Expression))

(Rule ArrayInitializer (@ "array-init" (braces$ (? (tr-list VariableInitializer)))))


;;;;;;;;;;;;;; Formal Parameter

(Rule FormalParameters (@ "formal-params" (parens$ (? (/ (+ (list FormalParameter) (? (+ comma VarArityParameter)))
                                                         VarArityParameter)))))

(Rule FormalParameter (@ "param" (+ VariableModifiers Type VariableDeclaratorId)))

(Rule VarArityParameter (@ "var-param" (+ VariableModifiers Type (tok "...") VariableDeclaratorId)))

(Rule VariableModifiers (* (@: "final")))

(Rule VariableDeclaratorId (@ "name" (+ Identifier (* Dim))))


;;;;;;;;;;;;;; Statements

(Rule Block (@ "block" (braces$ (* BlockStatement))))

(Rule BlockStatement  (// LocalVariableDeclarationStatement
                          Statement))

(Rule Statement (// Block
                    (@$: "if" ParExpression Statement (? (@$: "else" Statement)))
                    (@: "for" (parens (+ (? ForInit) semicolon (? Expression) semicolon (? Expressions))) Statement)
                    (@$: "do" Statement (@$: "while" ParExpression semicolon$))
                    (@$: "while" ParExpression Statement)
                    (@$: "switch" ParExpression (braces$ (* SwitchBlockStatementGroup)))
                    (@$: "synchronized" ParExpression Block)
                    (@$: "return" (? Expression) semicolon$)
                    (@$: "throw" Expression semicolon$)
                    (@$: "break" (? Identifier) semicolon$)
                    (@$: "continue" (? Identifier) semicolon$)
                    (@ "label" (+ Identifier colon Statement))
                    (@$: "try" Block (/ (+ (+ CatchClause (* CatchClause)) (? Finally)) Finally))
                    (@ "statement-expr" (+ Expression semicolon))
                    semicolon))


(Rule ForInit (/ (+ VariableModifiers Type VariableDeclarators) 
                 Expressions))

(Rule CatchClause (@$: "catch" (parens$ FormalParameter) Block))

(Rule Finally (@$: "finally" Block))

(Rule SwitchBlockStatementGroup (+ SwitchLabel colon (* BlockStatement)))

(Rule SwitchLabel (/ (@$: "case" Expression)
                     (@: "default")))

;;;;;;;;;;;;;; Expressions

(Rule Expressions (list Expression))

(Rule Expression (/ (@ "add-assign-expr" (+ Expression1 (oper "+=") Expression))
                    (@ "sub-assign-expr" (+ Expression1 (oper "-=") Expression))
                    (@ "mul-assign-expr" (+ Expression1 (oper "*=") Expression))
                    (@ "div-assign-expr" (+ Expression1 (oper "/=") Expression))
                    (@ "and-assign-expr" (+ Expression1 (oper "&=") Expression))
                    (@ "ior-assign-expr" (+ Expression1 (oper "|=") Expression))
                    (@ "xor-assign-expr" (+ Expression1 (oper "^=") Expression))
                    (@ "mod-assign-expr" (+ Expression1 (oper "%=") Expression))
                    (@ "lsh-assign-expr" (+ Expression1 (oper "<<=") Expression))
                    (@ "rsh-assign-expr" (+ Expression1 (oper ">>=") Expression))
                    (@ "rot-assign-expr" (+ Expression1 (oper ">>>=") Expression))
                    (@ "assign-expr" (+ Expression1 (oper "=") Expression))
                    Expression1))

(Rule Expression1 (/ (@ "cond-expr" (+ Expression2 (oper "?") Expression (oper ":") Expression1))
                     Expression2))

(Rule Expression2 (/ (@ "cond-or-expr" (+ Expression2 (oper "||") Expression3))
                     Expression3))

(Rule Expression3 (/ (@ "cond-and-expr" (+ Expression3 (oper "&&") Expression4))
                     Expression4))

(Rule Expression4 (/ (@ "ior-expr" (+ Expression4 (oper "|") Expression5))
                     Expression5))

(Rule Expression5 (/ (@ "xor-expr" (+ Expression5 (oper "^") Expression6))
                     Expression6))

(Rule Expression6 (/ (@ "and-expr" (+ Expression6 (oper "&") Expression7))
                     Expression7))

(Rule Expression7 (/ (@ "eq-expr" (+ Expression7 (oper "==") Expression8))
                     (@ "neq-expr" (+ Expression7 (oper "!=") Expression8))
                     Expression8))

(Rule Expression8 (/ (@.$: Expression8 "instanceof" Type)
                     (@ "less-eq-expr" (+ Expression9 (oper "<=") Expression9))
                     (@ "less-expr" (+ Expression9 (oper "<") Expression9))
                     (@ "greater-eq-expr" (+ Expression9 (oper ">=") Expression9))
                     (@ "greater-expr" (+ Expression9 (oper ">") Expression9))
                     Expression9))

(Rule Expression9 (/ (@ "rrot-expr" (+ Expression9 (oper ">>>") Expression10))
                     (@ "rsh-expr" (+ Expression9 (oper ">>") Expression10))
                     (@ "lsh-expr" (+ Expression9 (oper "<<") Expression10))
                     Expression10))

(Rule Expression10 (/ (@ "add-expr" (+ Expression10 (oper "+") Expression11))
                      (@ "sub-expr" (+ Expression10 (oper "-") Expression11))
                      Expression11))

(Rule Expression11 (/ (@ "mul-expr" (+ Expression11 (oper "*") UnaryExpression))
                      (@ "div-expr" (+ Expression11 (oper "/") UnaryExpression))
                      (@ "mod-expr" (+ Expression11 (oper "%") UnaryExpression))
                      UnaryExpression))

;(def-pattern right-assoc-expr (self next (opname op) ...)
;    (Rule self (/ (@ opname (+ next op self)) ...
;                            next))

(Rule UnaryExpression (+ SelectorExpression (* PostfixOp)))

(Rule SelectorExpression (+ PrefixExpression (* (/ DimExpr
                                                   (+ dot Selector)))))

(Rule Selector (// (+ (kw "this"))
                   (+ SuperInvocation)
                   InnerCreator
                   (+ Identifier (? Arguments))))

(Rule PrefixExpression (+ (* PrefixOp) CastExpression))

(Rule CastExpression (/ (@ "cast" (+ (parens Type) UnaryExpression)) Primary))

(Rule Primary (// Literal
                  ParExpression
                  (@$: "this")
                  SuperInvocation                  
                  Creator
                  (+ QualifiedIdentifier (? (/ (brackets Expression)
                                               Arguments
                                               (+ (* Dim) dot (kw "class")) 
                                               (+ dot (/ (kw "this") 
                                                         (+ (kw "super") Arguments) 
                                                         InnerCreator)))))))

(Rule PrefixOp (/ (@ "inc" (oper "++"))
                  (@ "dec" (oper "--"))
                  (@ "not" (oper "!"))
                  (@ "inv" (oper "~"))
                  (@ "pos" (oper "+"))
                  (@ "neg" (oper "-"))))

(Rule PostfixOp (/ (@ "inc" (oper "++"))
                   (@ "dec" (oper "--"))))


(Rule ParExpression (@ "par-expr" (parens$ Expression)))  

(Rule Arguments (@ "arguments" (parens$ (? Expressions))))

(Rule SuperInvocation (@$: "super" (/ Arguments (+ dot Identifier (? Arguments)))))

(Rule Creator (@$: "new" (/ BasicType CreatedName) (/ ArrayCreatorRest
                                                      ClassCreatorRest)))

(Rule ArrayCreatorRest (/ (+ Dim (* Dim) ArrayInitializer)
                          (+ DimExpr (* DimExpr) (* Dim))))

(Rule InnerCreator (+ (kw "new") Identifier ClassCreatorRest))

(Rule ClassCreatorRest (+ Arguments (? ClassBody)))

(Rule Dim (@ "dim" (brackets ())))

(Rule DimExpr (@ "dim-expr" (brackets Expression)))

;;;;;;;;;;;;;; Types and Modifiers

(Rule Type (@ "type" (+ (/ BasicType CreatedName) 
                        (* Dim))))

(Rule CreatedName (seq (+ Identifier ) dot))

(Rule BasicType (// (@: "void")
                    (@: "byte")
                    (@: "short")
                    (@: "char")
                    (@: "int")
                    (@: "long")
                    (@: "float")
                    (@: "double")
                    (@: "boolean")))

(Rule Modifier (// (@: "public")
                   (@: "protected")
                   (@: "private")
                   (@: "static")
                   (@: "abstract")
                   (@: "final")
                   (@: "native")
                   (@: "synchronized")
                   (@: "transient")
                   (@: "volatile")
                   (@: "strictfp")))

(Rule Modifiers (@ "modifiers" (* Modifier)))


;;;;;;;;;;;;;; Whitespace and Comments

(Rule WhiteSpace (/ (+ (/ WhitespaceChar LineComment BlockComment) WhiteSpace) ()))

(Rule LineComment (+ "//" (* (+ (! Newline) AnyChar)) Newline))

(Rule BlockComment (/ (+ "/*" ($ (+ (* (+ (! "*/") AnyChar)) "*/") "unclosed comment"))))

;;;;;;;;;;;;;; Identifiers and Terminals

; TODO: exclude the keywords from identifiers
(Rule Identifier (tok (< (+ (! Keyword) (/ LetterChar "_" "$") (* LetterOrDigit)))))

(Rule QualifiedIdentifier (+ Identifier (* (+ dot Identifier))))

(Rule LetterOrDigit (/ LetterChar DigitChar "_" "$"))

(Rule OperatorChar (/ "!" "%" "^" "&" "*" "-" "+" "=" "|" "?" "<" ">" ":" "/"))

(Rule dot (tok "."))

(Rule semicolon (tok ";"))

(Rule semicolon$ ($ semicolon "';' expected"))

(Rule comma (tok ","))

(Rule colon (tok ":"))

(Rule EndOfFile (! AnyChar))

;;;;;;;;;;;;;; Literals

(Rule Literal (/ FloatingPointLiteral
                 IntegerLiteral
                 CharacterLiteral
                 StringLiteral
                 BooleanLiteral
                 NullLiteral))

(Rule FloatingPointLiteral (@ "floating-point" (tok (< (/ HexadecimalFloatingPointLiteral DecimalFloatingPointLiteral)))))

(Rule HexadecimalFloatingPointLiteral (+ HexSignificand BinaryExponent (? FloatTypeSuffix)))

(Rule HexSignificand (+ HexNumeral (? ".") (? HexDigits)))

(Rule BinaryExponent (+ (/ "p" "P") SignedInteger))

(Rule FloatTypeSuffix (/ "f" "F" "d" "D"))

(Rule DecimalFloatingPointLiteral (/ (+ "." Digits (? ExponentPart) (? FloatTypeSuffix))
                                     (+ Digits "." (? Digits) (? ExponentPart) (? FloatTypeSuffix))
                                     (+ Digits (? ExponentPart) FloatTypeSuffix)
                                     (+ Digits ExponentPart (? FloatTypeSuffix))))

(Rule ExponentPart (+ (/ "e" "E") SignedInteger))

(Rule SignedInteger (+ (? (/ "+" "-")) Digits))

(Rule IntegerLiteral (@ "integer" (tok (< (/ (+ HexNumeral (? IntegerTypeSuffix))
                                             (+ OctalNumeral (? IntegerTypeSuffix))
                                             (+ DecimalNumeral (? IntegerTypeSuffix)))))))

(Rule HexNumeral (+ (/ "0x" "0X") HexDigits))

(Rule OctalNumeral (+ "0" (* OctalDigit)))

(Rule DecimalNumeral Digits)

(Rule IntegerTypeSuffix (/ "l" "L"))

(Rule OctalDigit (/ "0" "1" "2" "3" "4" "5" "6" "7"))

(Rule HexDigits (+ HexDigitChar (* HexDigitChar)))

(Rule Digits (+ DigitChar (* DigitChar)))

(Rule CharacterLiteral (@ "char" (tok (+ SingleQuoteChar (! SingleQuoteChar) (< Character) SingleQuoteChar))))

(Rule Character (/ (+ BackslashChar (/ "b" "t" "n" "f" "r" DoubleQuoteChar SingleQuoteChar 'BackslashChar)) 
                   (+ BackslashChar "u" ($ (+ HexDigitChar HexDigitChar HexDigitChar HexDigitChar) "Illegal unicode escape"))
                   (+ BackslashChar ($ (/ (+ (/ "0" "1" "2" "3") OctalDigit OctalDigit) (+ OctalDigit OctalDigit) OctalDigit) "Illegal octal escape")) 
                   (+ (! ReturnChar) (! LinefeedChar) AsciiChar)))

(Rule StringLiteral (@ "string" (tok (+ DoubleQuoteChar (< (* (+ (! DoubleQuoteChar) Character))) DoubleQuoteChar))))

(Rule BooleanLiteral (/ (@: "true") (@: "false")))

(Rule NullLiteral (@: "null"))

