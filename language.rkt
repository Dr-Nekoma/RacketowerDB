#lang racket

(module lex racket
  (require parser-tools/lex)
  (require (prefix-in : parser-tools/lex-sre))
  
  (define (tokenizer ip)
    (letrec ([one-line
              (lambda ()
		(let ([result (mappings ip)])
                  (unless (equal? (position-token-token result) 'EOF)
                    (printf "~a\n" result)
                    (one-line)
                    )))])
      (one-line)))

  (define-lex-abbrev keywords (:or "BEGIN" "END" "INSERT" "CREATE" "RELATION" "PROJECT" "SELECT"))
  (define-lex-abbrev types (:or "STRING" "INTEGER"))
  (define-tokens keyword-tokens
    (BEGIN END INSERT CREATE RELATION PROJECT SELECT STRING INTEGER))
  (define-empty-tokens punctuation-tokens (LEFT_PAREN RIGHT_PAREN EOF))
  (define-tokens basic-tokens
    (IDENTIFIER NUMBER))

  (define mappings
    (lexer-src-pos
       [(eof) (token-EOF)]
       ["(" (token-LEFT_PAREN)]
       [")" (token-RIGHT_PAREN)]
       [keywords (string->symbol lexeme)]
       [types (string->symbol lexeme)]
       [(:+ numeric) (token-NUMBER (string->number lexeme))]
       [(:: (:or alphabetic #\_) (:* (:or alphabetic numeric #\_))) (token-IDENTIFIER lexeme)]
       ;; [(repetition 1 +inf.0 numeric) (string->number lexeme)]
       [whitespace (mappings input-port)]))

  (provide keyword-tokens)
  (provide punctuation-tokens)
  (provide basic-tokens)
  (provide tokenizer)
  (provide mappings))

(module+ parser
  (require (submod ".." lex))
  (require parser-tools/yacc)

  (define parse
    (parser
     (start expr)
     (end EOF)
     (error (lambda (a b c d e) (begin (printf "tok-ok = ~a\ntok-name = ~a\ntok-value = ~a\nstart-pos = ~a\nend-pos = ~a\n" a b c d e) (void))))
     (tokens keyword-tokens punctuation-tokens basic-tokens)
     (src-pos)

     (grammar
      (expr [(CREATE) "Duh"]))))

  (provide parse))

