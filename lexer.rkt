#lang racket

(module lexer racket
  (require parser-tools/lex)

  (define sample-input "BEGIN CREATE RELATION person() END")

  (define (tokenizer lexer)
    (define p (open-input-string sample-input))
    (list (lexer p)
	  (lexer p)
	  (lexer p)
	  (lexer p)
	  (lexer p)))

  (define keywords
    (list "BEGIN"
	  "END"
	  "INSERT"
	  "CREATE"
	  "RELATION"
	  "PROJECT"
	  "SELECT"))

  (define mappings
    (eval `(lexer
       [(eof) eof]
       ["(" 'LEFT-PAREN]
       [")" 'RIGHT-PAREN]
       ,@(map (lambda (k) `(,k (string->symbol ,k))) keywords) ;;[(keywords) lexeme]
       [(repetition 1 +inf.0 numeric) (string->number lexeme)]
       [(concatenation (union alphabetic #\_)
                       (repetition 0 +inf.0 (union alphabetic numeric #\_)))
	lexeme]
					; invoke the lexer again to skip the current token
       [whitespace (the-lexer/primitive input-port)])))

  (provide tokenizer)
  (provide mappings))

