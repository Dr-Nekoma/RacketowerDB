#lang racket


(module+ lex
  (require parser-tools/lex)
  (require racket/match/gen-match)

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
    (let ((ns (make-base-empty-namespace)))
      (namespace-attach-module (current-namespace)
			       'parser-tools/lex
			       ns)
      (namespace-attach-module (current-namespace)
			       'racket/match/gen-match
			       ns)
      (namespace-attach-module (current-namespace)
			       'racket/base
			       ns)
      (parameterize ([current-namespace ns])
	(namespace-require 'parser-tools/lex)
	(eval `(lexer
		[(eof) eof]
		["(" 'LEFT-PAREN]
		[")" 'RIGHT-PAREN]
		,@(map (lambda (k) `(,k (string->symbol ,k))) keywords)
		[(repetition 1 +inf.0 numeric) (string->number lexeme)]
		[(concatenation (union alphabetic #\_)
				(repetition 0 +inf.0 (union alphabetic numeric #\_))) lexeme]
		[whitespace (mappings input-port)])))))

  (provide tokenizer)
  (provide mappings))

