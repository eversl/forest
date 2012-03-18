
; Syntax definition for java 5 annotations

(def-rule PackageDeclaration (@.$: (* Annotation) "package" QualifiedIdentifier semicolon))

(choice-append! TypeDeclaration AnnotationTypeDeclaration)

;;;;;;;;;;;;;; Class Declaration

(choice-append! ClassBodyDeclaration AnnotationTypeDeclaration)

;;;;;;;;;;;;;; Interface Declaration

(choice-append! InterfaceBodyDeclaration AnnotationTypeDeclaration)

;;;;;;;;;;;;;; Enum Declaration

(def-rule EnumConstant (@ "constant" (+ (* Annotation) Identifier (? Arguments) (? ClassBody))))

;;;;;;;;;;;;;; Annotations

(def-rule AnnotationTypeDeclaration (+ Modifiers (tok "@") (kw "interface") Identifier AnnotationTypeBody))

(def-rule AnnotationTypeBody (braces (* AnnotationTypeElementDeclaration)))

(def-rule AnnotationTypeElementDeclaration (+ Modifiers AnnotationTypeElementRest))

(def-rule AnnotationTypeElementRest (/ ClassDeclaration 
                                       InterfaceDeclaration 
                                       EnumDeclaration
                                       AnnotationTypeDeclaration
                                       (+ Type Identifier (/ AnnotationMethodRest AnnotationConstantRest) semicolon)))

(def-rule AnnotationMethodRest (+ (parens ()) (? DefaultValue)))

(def-rule AnnotationConstantRest VariableDeclarators)

(def-rule DefaultValue (@$: "default" ElementValue))

(def-rule Annotation (@ "annotation" (+ (tok "@") QualifiedIdentifier 
                                        (? (parens (+ (?+ Identifier (tok "=")) ElementValue))))))

(def-rule ElementValue (/ Expression
                          Annotation
                          (braces (tr-list ElementValue))))

;;;;;;;;;;;;;; Variable Declaration

(def-rule VariableModifiers (* (/ Annotation (@: "final"))))

(choice-append! Modifier Annotation)
