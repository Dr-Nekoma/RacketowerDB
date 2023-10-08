#lang racket

(module lex racket
  (require parser-tools/lex)
  (require (prefix-in : parser-tools/lex-sre))
  
  (define (tokenizer ip)
    (letrec ([one-line
              (lambda ()
		(let ([result (mappings/tokens ip)])
                  (unless (equal? (position-token-token result) 'EOF)
                    (printf "~a\n" result)
                    (one-line)
                    )))])
      (one-line)))

  (define-lex-abbrev keywords (:or "BEGIN" "END" "INSERT" "CREATE" "RELATION" "PROJECT" "SELECT"))
  (define-lex-abbrev types (:or "STRING" "INTEGER"))
  (define-empty-tokens keyword-tokens
    (STRING INTEGER))
  (define-empty-tokens operation-tokens (LESS GREATER EQUAL))
  (define-empty-tokens empty-keyword-tokens
    (PROJECT CREATE BEGIN ON SELECT END RELATION))
  (define-empty-tokens punctuation-tokens (LPAREN RPAREN COMMA EOF))
  (define-tokens basic-tokens
    (IDENTIFIER NUMBER))

  (define mappings/tokens
    (lexer-src-pos
       [(eof) (token-EOF)]
       ["(" (token-LPAREN)]
       [")" (token-RPAREN)]
       ["," (token-COMMA) ]
       [">" (token-GREATER) ]
       ["<" (token-LESS) ]
       ["=" (token-EQUAL) ]
       ["PROJECT" (token-PROJECT) ]
       ["CREATE" (token-CREATE) ]
       ["BEGIN" (token-BEGIN) ]
       ["ON" (token-ON) ]
       ["SELECT" (token-SELECT) ]
       ["END" (token-END) ]
       ["RELATION" (token-RELATION) ]
       ["STRING" (token-STRING) ]
       ["INTEGER" (token-INTEGER) ]
       [keywords (string->symbol lexeme)]
       [types (string->symbol lexeme)]
       [(:+ numeric) (token-NUMBER (string->number lexeme))]
       [(:: (:or alphabetic #\_) (:* (:or alphabetic numeric #\_))) (token-IDENTIFIER lexeme)]
       ;; [(repetition 1 +inf.0 numeric) (string->number lexeme)]
       [whitespace (return-without-pos (mappings/tokens input-port))]))

  (provide keyword-tokens)
  (provide empty-keyword-tokens)
  (provide punctuation-tokens)
  (provide basic-tokens)
  (provide operation-tokens)
  (provide tokenizer)
  (provide mappings/tokens))

(module+ parser
  (require (submod ".." lex))
  (require parser-tools/yacc)
  (require RacketowerDB/ast)
  (require threading)

  (define (create-relation-fields fields-to-convert)
    (define (create-relation-field field-to-convert index)
      (let* ((name (car field-to-convert))
             (anamed-data (cdr field-to-convert))
             (type-name (car anamed-data))
             (size (cdr anamed-data)))
        (cons name (new field%
                        [position index]
                        [type (new type%
                                   [name type-name]
                                   [byte-size size])]))))
    (let ((indexes (range (length fields-to-convert))))
      (map create-relation-field fields-to-convert indexes)))

  (define parse
    (parser
     [start expr]
     [end EOF]
     [error (lambda (a b c d e) (begin (printf "tok-ok = ~a\ntok-name = ~a\ntok-value = ~a\nstart-pos = ~a\nend-pos = ~a\n" a b c d e) (void)))]
     [tokens keyword-tokens punctuation-tokens operation-tokens basic-tokens empty-keyword-tokens]
     [src-pos]

     ;; CREATE RELATION Car (A STRING(5) B INTEGER C INTEGER)

     ;; PROJECT a, b ON table_name SELECT a > 123
     ;; [grammar
     ;;   [expr [(LPAREN exprs RPAREN) $2]
     ;;         [(NUMBER) $1]
     ;;         [(IDENTIFIER) $1]]
     ;;   [exprs [() '()]
     ;;          [(expr exprs) (cons $1 $2)]]]
     
     [grammar
      [expr [() null]
            [(LPAREN exprs RPAREN) $2]
            [(IDENTIFIER) $1]
            [(NUMBER) $1]
            [(PROJECT IDENTIFIER) $2]
            [(SELECT exprs) $2]
            [(expr operator expr) (list $1 $2 $3)]
            [(CREATE RELATION IDENTIFIER LPAREN attrs RPAREN)
             (cons $3 (new table% [fields (make-hash (create-relation-fields $5))]))]
            [(PROJECT identifiers ON IDENTIFIER) (list $2 $4)]
            [(PROJECT identifiers ON IDENTIFIER exprs) (list $2 $4 $5)]
            ]
      ;; [entities [(RELATION)]]
      [identifiers
       [() '()]
       [(IDENTIFIER identifiers) (cons $1 $2)]]      
      [type [(STRING LPAREN NUMBER RPAREN) (cons 'VARCHAR $3)]
            [(INTEGER) (cons 'INTEGER 4)]]
      [operator [(LESS) "<"]
                [(GREATER) ">"]
                [(EQUAL) "="]]
      [attr [(IDENTIFIER type) (cons $1 $2)]]
      [attrs [() '()]
             [(attr attrs) (cons $1 $2)]]
      [exprs [() '()]
             [(expr exprs) (cons $1 $2)]]]
     ))
        
  (provide parse))

