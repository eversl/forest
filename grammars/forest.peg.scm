(Import "forest.patterns.scm")
(Import "forest.rules.scm")

;;;;;;;;;;;;;;;;;;;;;;;;

; start parsing here
(Rule start (+ WhiteSpace Declarations EndOfFile))

(Rule EndOfFile (! AnyChar))

(Rule Declaration (> Sexpr))
(Rule Declarations (/ (+ Declaration Declarations) ()))

;;;;;;;;;;;;;;;;;;;;; Names

(Rule Name (// (@ "Var" (+ (tok "'") NameLiteral)) 
               (@ "VarList" (+ (tok ",") NameLiteral))
               (@ "SpecialVar" (+ (tok "`") NameLiteral))
               (@ "Name" NameLiteral)))
(Rule NameChar (/ LetterChar DigitChar "$" "+" "<" "=" ">" "^" "`" "|" "~" "_" "!" "%" "&" "*" "-" "+" "?" ":" "/" "@" "."))
(Rule NameChars (/ (+ NameChar NameChars) ()))
(Rule NameLiteral (tok (< (+ NameChar NameChars))))

;;;;;;;;;;;;;;;;;;;;; String and Null

(Rule StringChars (/ (+ (! DoubleQuoteChar) AnyChar StringChars) ()))
(Rule StringLiteral (tok (+ DoubleQuoteChar (< StringChars) DoubleQuoteChar))) 

(Rule Null (+ (tok "(") (tok ")")))

;;;;;;;;;;;;;; Whitespace

(Rule WhiteSpace (/ (+ (/ WhitespaceChar LineComment) WhiteSpace) ()))
(Rule CommentChars (/ (+ (! Newline) AnyChar CommentChars) ()))
(Rule LineComment (+ ";" CommentChars Newline))
(Rule Newline (/ (+ ReturnChar LinefeedChar) ReturnChar LinefeedChar))

;;;;;;;;;;;;;;;;;;;;;;;; Sexpr

(Rule Sexpr (// 
             Name
             (@ "String" StringLiteral)
             (@ "Pattern" (parens (+ (sym "Pattern") Sexpr Sexpr)))
             (@ "Rule" (parens (+ (sym "Rule") Name Sexpr)))
             (@ "Insert" (parens (+ (sym "Insert") Name Sexpr)))
             (@ "NewName" (parens (+ (sym "NewName"))))
             (@ "Import" (parens (+ (sym "Import") StringLiteral)))
             
             (@ "+" (parens (+ (sym "+") Sexprs)))
             (@ "//" (parens (+ (sym "//") Sexprs)))
             (@ "/" (parens (+ (sym "/") Sexprs)))
             (@ "!" (parens (+ (sym "!") Sexpr)))
             (@ "<" (parens (+ (sym "<") Sexpr)))
             (@ "@" (parens (+ (sym "@") Sexpr Sexprs)))
             (@ ">" (parens (+ (sym ">") Sexpr)))
             (@ "$" (parens (+ (sym "$") Sexpr Sexprs)))
             (@ "Null" Null)
             ))

(Rule Sexprs (/ (+ Sexpr Sexprs) ()))