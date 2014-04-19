; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2014                              *
; *  Author: Liu Liu                          *
; *          Ulrich Kremer                    *
; *  April 5, 2014                            *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ctv", "vtc",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")
;(load "dictionary.ss") ;; the real thing with 45,000 words

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; boolean: #t if "a" is a member of the list "l"
(define is-member?
  (lambda (a l)
    (cond ((null? l) #f)
          ((equal? a (car l)) #t)
          (else (is-member? a (cdr l))))))

;; boolean: #t for '(w o r d), #f for '((w o r d))
(define is-word?
  (lambda (w)
    (and (list? w) (not (list? (car w))))))

;; apply spell-checker to a paragraph
;input  : paragraph
;output : list of booleans for each word in paragraph if spelled correctly
(define spell-checker-p
  (lambda (p)
    (cond ((null? p) '())
          ((spell-checker (car p)) (append (list 1) (spell-checker-p (cdr p))))
          (else (append (list 0) (spell-checker-p (cdr p)))))))

;; reduces a boolean list to just the number of #t in the list
(define num-valid-words
  (lambda (p)
    (reduce + (spell-checker-p p) 0)))

;; returns position of element in list (must be in list) 
(define get-position
  (lambda (a lst)
    (cond ((equal? a (car lst)) 0)
          (else (+ (get-position a (cdr lst)) 1)))))

;; removes an item from a list
(define remove
  (lambda (a lst)
    (cond ((null? lst) '())
          ((equal? a (car lst)) (remove a (cdr lst)))
          (else (cons (car lst) (remove a (cdr lst)))))))

;; flatten a list (from hw6)
(define flatten 
  (lambda (l)
    (cond ((null? l) '())
          ((not (list? l)) (list l))
          (else  (append (flatten (car l)) (flatten (cdr l)))))))

;; counts the number of times 'item' appears in the list 'lst'
(define count-occurences 
  (lambda (item lst)
    (cond ((null? lst) 0)
          ((equal? item (car lst)) (+ 1 (count-occurences item (cdr lst))))
          (else (count-occurences item (cdr lst))))))

;; get frequency distribution of letters, 5 a's, 3 b's, and 7 d's: (5 3 0 7 0 ...)
;input  : a word, paragraph, or document
;output : returns a histogram of letters represented as a list
(define letter-histogram
  (lambda (p)
    (map (lambda (letter) (count-occurences letter (flatten p))) 
         abc-list)))

;; -----------------------------------------------------
;; GLOBAL VARIABLES

(define abc-list '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define abc-nums (map ctv abc-list))
(define abc-freqs '(e t a o n r i s h d l f c m u g y p w b v k j x q z))

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
    (is-member? w dictionary)))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input=a word, output=encoded word
(define encode-n
  (lambda (n)  ;;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w)  ;;"w" is the word to be encoded
      (map vtc 
           (map (lambda (x) (modulo (+ n x) 26)) ;; (x + n) % 26
                (map ctv 
                     w))))))

;;encode a paragraph
;;INPUT: a paragraph "p" and an "encoder"
;;OUTPUT: an encoded paragraph using a provided encoder
(define encode-p
  (lambda (p encoder)
    (cond ((null? p) '())
          ((is-word? p) (list (encoder p)))
          (else (append (encode-p (car p) encoder) 
                        (encode-p (cdr p) encoder))))))

;;encode a document
;;INPUT: a document "d" and an "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    (cond ((null? d) '())
          ((is-word? d) (encoder d))
          (else (append (list (encode-d (car d) encoder)) 
                        (encode-d (cdr d) encoder))))))
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
    (define check-decode
      (lambda (n) 
        (num-valid-words (encode-p p (encode-n n))))
      )
    (let ((decode-compare (map check-decode abc-nums)))
      (encode-n (get-position (apply max decode-compare) decode-compare)))
    )
  )

;(define encoded (encode-d test-document (encode-n 11)))
;(define decoderA (Gen-Decoder-A encoded))
;(display "Encoded Document: " )
;encoded
;(display "Decoded Document: ")
;(encode-d encoded decoderA)

;;generate a decoder using frequency analysis
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-B
  (lambda (p)
    (vtc (get-position 
          (apply max (letter-histogram p)) 
          (letter-histogram p)))
    ))

; (vtc (get-position 
;        (apply max (letter-histogram p)) 
;        (letter-histogram p)))


;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
    (encode-d d decoder)))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;(define add5 (encode-n 5))
;(define encoded-document (encode-d document add5))
;(define decoderSP1 (Gen-Decoder-A (car (cdr encoded-document))))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;(Code-Breaker encoded-document decoderSP1)
