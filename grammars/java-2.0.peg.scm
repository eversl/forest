
; Syntax definition for java lang spec 2.0

(import "java-1.0.peg.scm")

; the actual content as updates to existing rules


(choice-append! Statement (@$: "assert" Expression (?+ colon Expression) semicolon))
(choice-append! Statement (@$: "for" (parens$ (+ FormalParameter colon Expression)) Statement))

;(def-rule Primary (/ ParExpression
;                     Literal
;                     (@$: "this")
;                     SuperInvocation                  
;                     Creator
;                     (+ QualifiedIdentifier (? IdentifierSuffix))))
(def-rule Primary (// ParExpression
                     Literal
                     (@$: "this" (? Arguments))
                     SuperInvocation                  
                     Creator
                     (+ QualifiedIdentifier (? IdentifierSuffix))))



;;;;;;;;;;;;;; Inner classes

(choice-append! InterfaceBodyDeclaration ClassDeclaration)
(choice-append! InterfaceBodyDeclaration InterfaceDeclaration)

(choice-append! BlockStatement ClassDeclaration)

(choice-append! Primary (+ BasicType (* Dim) dot (kw "class")))

(choice-append! ClassBodyDeclaration ClassDeclaration)
(choice-append! ClassBodyDeclaration InterfaceDeclaration)

;;;;;;;;;;;;;; Enum Declaration
;; see http://jcp.org/aboutJava/communityprocess/jsr/tiger/enum.html

(choice-append! TypeDeclaration EnumDeclaration)
(choice-append! ClassBodyDeclaration EnumDeclaration)
(choice-append! InterfaceBodyDeclaration EnumDeclaration)
(choice-append! BlockStatement EnumDeclaration)

(def-rule EnumDeclaration (@.$: Modifiers "enum" Identifier (? (@$: "implements" (list Type))) EnumBody))

(def-rule EnumBody (braces (+ (tr-list EnumConstant) (? EnumBodyDeclarations))))

(def-rule EnumConstant (@ "constant" (+ (* Annotation) Identifier (? Arguments) (? ClassBody))))

(def-rule EnumBodyDeclarations (+ semicolon (* ClassBodyDeclaration)))

(def-rule EnumConstantName Identifier)

;;;;;;;;;;;;;; Annotations

; (def-rule PackageDeclaration (@$: "package" QualifiedIdentifier semicolon))
(def-rule PackageDeclaration (@.$: (* Annotation) "package" QualifiedIdentifier semicolon))

; (def-rule InterfaceDeclaration (@.$: Modifiers "interface" Identifier 
;                                     (? (@$: "extends" (list Type))) (braces (* InterfaceBodyDeclaration))))
(def-rule InterfaceDeclaration (@.$: Modifiers "interface" Identifier (? TypeParameters) 
                                     (? (@$: "extends" (list Type))) (braces (* InterfaceBodyDeclaration))))

(choice-append! InterfaceBodyDeclaration (@ "generic-method" (+ Modifiers TypeParameters InterfaceMethodDeclarator)))

(choice-append! TypeDeclaration AnnotationTypeDeclaration)
(choice-append! ClassBodyDeclaration AnnotationTypeDeclaration)
(choice-append! InterfaceBodyDeclaration AnnotationTypeDeclaration)

(choice-append! Modifier Annotation)
  
; (def-rule VariableModifiers (* (@: "final")))
(def-rule VariableModifiers (* (/ Annotation (@: "final"))))

(def-rule AnnotationTypeDeclaration (+ Modifiers (tok "@") (kw "interface") Identifier AnnotationTypeBody))

(def-rule AnnotationTypeBody (braces (* AnnotationTypeElementDeclaration)))

(def-rule AnnotationTypeElementDeclaration (+ Modifiers AnnotationTypeElementRest))

(def-rule AnnotationTypeElementRest (/ ClassDeclaration 
                                       InterfaceDeclaration 
                                       EnumDeclaration
                                       AnnotationTypeDeclaration
                                       (+ Type Identifier (/ AnnotationMethodRest AnnotationConstantRest) semicolon)))

(def-rule AnnotationMethodRest (+ (parens$ ()) (? DefaultValue)))

(def-rule AnnotationConstantRest VariableDeclarators)

(def-rule DefaultValue (@$: "default" ElementValue))

(def-rule Annotation (@ "annotation" (+ (tok "@") QualifiedIdentifier 
                                        (? (parens$ (? (list (+ (?+ Identifier (tok "=")) ElementValue))))))))

(def-rule ElementValue (/ Expression
                          Annotation
                          ElementValueArrayInitializer))

(def-rule ElementValueArrayInitializer (braces (tr-list ElementValue)))

;;;;;;;;;;;;; Generics

;(def-rule ClassDeclaration (@.$: Modifiers "class" Identifier 
;                                 (? (@$: "extends" Type))
;                                 (? (@$: "implements" (list Type)))
;                                 ClassBody))
(def-rule ClassDeclaration (@.$: Modifiers "class" (+ Identifier (? TypeParameters)) 
                                 (? (@$: "extends" Type))
                                 (? (@$: "implements" (list Type)))
                                 ClassBody))


; (def-rule CreatedName (seq (+ Identifier ) dot))
(def-rule CreatedName (seq (+ Identifier (? (@ "type-args" (chevrons (list TypeArgument))))) dot))


;(def-rule IdentifierSuffix (/ (brackets Expression)
;                              Arguments
;                              (+ (* Dim) dot (kw "class")) 
;                              (+ dot (/ (kw "this") 
;                                        (+ (kw "super") Arguments) 
;                                        InnerCreator))))
(def-rule IdentifierSuffix (/ (brackets Expression)
                              Arguments
                              (+ (* Dim) dot (kw "class")) 
                              (+ dot (/ ExplicitGenericInvocation 
                                        (kw "this") 
                                        (+ (kw "super") Arguments) 
                                        InnerCreator))))


(choice-append! Primary (@ "generic" (+ NonWildcardTypeArguments (/ ExplicitGenericInvocationSuffix (@$: "this" Arguments)))))

(choice-append! ClassBodyDeclaration (@ "generic-method" (+ Modifiers TypeParameters MethodDeclarator)))
(choice-append! ClassBodyDeclaration (@ "generic-constructor" (+ Modifiers TypeParameters ConstructorDeclarator)))

(choice-append! Selector ExplicitGenericInvocation)

(def-rule TypeParameters (@ "type-param" (chevrons$ (list (+ Identifier (?+ (kw "extends") (seq Type (tok "&"))))))))

(def-rule NonWildcardTypeArguments (@ "type-args" (chevrons (list Type))))

(def-rule ExplicitGenericInvocation (+ NonWildcardTypeArguments ExplicitGenericInvocationSuffix))

(def-rule ExplicitGenericInvocationSuffix (/ SuperInvocation
                                             (+ Identifier Arguments)))

(def-rule TypeArgument (/ (@ "any" (+ (tok "?") (? (/ (@$: "extends" Type) (@$: "super" Type)))))
                          Type))


