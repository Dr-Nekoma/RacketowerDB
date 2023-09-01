#lang racket

(module+ lex
  (require parser-tools/lex)
  (require (prefix-in : parser-tools/lex-sre))
  
  (define sample-input "BEGIN CREATE RELATION END")
  ;; (define sample-input "BEGIN CREATE RELATION person() END")

  (define (tokenizer lexer)
    (define p (open-input-string sample-input))
    (list (lexer p)
	  (lexer p)
	  (lexer p)
	  (lexer p)
	  (lexer p)))

  (define-lex-abbrev keywords (:or "BEGIN" "END" "INSERT" "CREATE" "RELATION" "PROJECT" "SELECT"))

  (define mappings
    (lexer
       [(eof) eof]
       ["(" 'left-paren]
       [")" 'right-paren]
       [keywords (string->symbol lexeme)]
       [(repetition 1 +inf.0 numeric) (string->number lexeme)]
       [whitespace (mappings input-port)]))

  (provide tokenizer)
  (provide mappings))

