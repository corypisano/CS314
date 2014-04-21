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
;(load "include.ss")

;; contains a test document consisting of three paragraphs. 
;(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
;(load "test-dictionary.ss")
;(load "dictionary.ss") ;; the real thing with 45,000 words

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; boolean: #t if "a" is a member of the list "l"
(define is-member?
  (lambda (a l)
    (cond ((null? l) #f)
          ((equal? a (car l)) #t)
          (else (is-member? a (cdr l))))))

;; boolean: #t if w is a list of characters, but not a list of words
(define is-word?
  (lambda (w)
    (and (list? w) (not (list? (car w))))))

;; apply spell-checker to a paragraph
;input  : paragraph
;output : list of 1's and 0's for each word in paragraph spelled correctly
(define spell-checker-p
  (lambda (p)
    (cond ((null? p) '())
          ((spell-checker (car p)) (append (list 1) (spell-checker-p (cdr p))))
          (else (append (list 0) (spell-checker-p (cdr p)))))))

;; returns number of correctly spelled words in a paragraph
(define num-valid-words
  (lambda (p)
    (reduce + (spell-checker-p p) 0)))

;; counts number of words in a paragraph
(define num-words
  (lambda (p)
    (cond ((null? p) 0)
          (else (+ 1 (num-words (cdr p)))))))

;; returns position of element in list (must be in list) 
(define get-position
  (lambda (a lst)
    (cond ((equal? a (car lst)) 0)
          (else (+ (get-position a (cdr lst)) 1)))))

;; returns the position in the list of the max element in the list
(define arg-max
  (lambda (lst)
    (get-position (apply max lst) lst)))

;; replaces a number in list with a 0
; used for recursive calls in Gen-Decoder-B
(define replace-n-0
  (lambda (n lst)
    (cond ((null? lst) '())
          ((equal? n (car lst)) (append (list 0) (cdr lst)))
          (else (append (list (car lst)) (replace-n-0 n (cdr lst)))))))

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
;; switch to arg-max
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

;;generate a decoder using frequency analysis
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-B
  (lambda (p)
    (define check-decode?
      (lambda (n)
        (if (number? n)
            (equal? (num-words p) (num-valid-words (encode-p p (encode-n n))))
            (display "error"))))
    (define decoder-iter
      (lambda (freqs)
        (cond ((equal? 0 (apply + freqs)) 0)
              ((check-decode? (- 4 (arg-max freqs))) (- 4 (arg-max freqs)))
              (else (decoder-iter (replace-n-0 (apply max freqs) freqs))))))  
    (encode-n (decoder-iter (letter-histogram p)))))

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
;(define encoded-p (car encoded-document))
;(define decoderFA1 (Gen-Decoder-B encoded-p))
;(Code-Breaker encoded-document decoderSP1)

