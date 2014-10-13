#lang racket

(module+ test
; test suite
; test case - function that takes a text and calls various methods, including insert
;  second piece is the bitmap that's the correct answer for the results of the function
; create a text, call the function, do-a-little-work until up-to-date?, compare bitmaps
; test-txtbmp : (-> text void) bitmap -> bool
; random testing
(require (for-syntax racket/base))
(define-syntax (test-txtbmp stx)
  (syntax-case stx ()
    [(_ . args)
     (with-syntax ([line (syntax-line stx)])
       #'(test-txtbmp/proc line . args))]))

(define (test-txtbmp/proc line fn2list ls)
  (define nt (new state%))
  (for ([f fn2list])
    (f nt)
    (let loop()
      (send nt do-a-little-work)
      (unless (send nt up-to-date?) (loop))))
  (print (send nt get-bitmap))
  (newline)
  (define actual (bitmap->strings (send nt get-bitmap)))
  (unless (equal? actual ls)
    (eprintf "YOU FAILED test on line ~a\n ~s\n ~s\n" line actual ls)))

(define (bitmap->strings bmp)
  (define bdc (make-object bitmap-dc% bmp))
  (define color (make-object color%))
  (for/list ([i (in-range (send bmp get-height))])
    (apply 
     string
    (for/list ([j (in-range (send bmp get-width))])
      (send bdc get-pixel j i color)
      (if (and (= 255 (send color red))
               (= 255 (send color green))
               (= 255 (send color blue)))
          #\space
          #\x)))))
      

; turn into bytes and use equal?
; empty bytes will be 4x width and height
; set-argb-pixels
(define (compare-bitmap bmp1 bmp2)
  (define b1 (make-bytes 40000))
  (define c1 (make-bytes 40000))
  (send bmp1 get-argb-pixels 0 0 100 100 b1) 
  (send bmp2 get-argb-pixels 0 0 100 100 c1)
  (equal? b1 c1))

; passed

(define (mini1 txt)
  (send txt insert "hello\n")
  (send txt insert "hello\n"))
(define (mini2 txt)
  (send txt insert "hello\n")
  (send txt insert "hi"))
(define (mini3 txt)
  (send txt delete 4 7))
(define (mini4 txt)
  (send txt insert "h        o"))
(define (mini5 txt)
  (send txt insert "1"))
(define (mini5d txt)
  (send txt delete 0 0))
(define (mini6 txt)
  (send txt insert "1"))
(define (mini6d txt)
  (send txt delete 0 1))
(define (mini7 txt)
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n")
  (send txt insert "hello\n"))
(define (mini8 txt)
  (send txt insert "hi\n" 6))
(define (mini9 txt)
  (send txt insert " " 12))
(define (mini10 txt)
  (send txt delete 6 12))
(define (mini11 txt)
  (send txt insert "aaaaaa\n")
  (send txt insert "      \n")
  (send txt insert "aaaaaa\n"))
(define (mini12 txt) 
  (send txt delete 3 10))
(define (mini13 txt)
  (send txt delete 4 5))
(define (mini14 txt)
  (send txt insert "aaaa\n")
  (send txt insert "    \n")
  (send txt insert "aaaa\n"))
(define (mini15 txt)
  (send txt insert "\n"))


(test-txtbmp (list mini1) '("xxxxx     "
                            "xxxxx     "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "))
(test-txtbmp (list mini2) '("xxxxx     "
                            "xx        "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "
                            "          "))
(test-txtbmp (list mini1 mini3) '("xxxxxxxx  "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "))
(test-txtbmp (list mini4 mini3) `("x     x   "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "
                                  "          "))
(test-txtbmp (list mini5 mini5d) `("x         "
                                   "          " 
                                   "          " 
                                   "          "
                                   "          " 
                                   "          " 
                                   "          " 
                                   "          " 
                                   "          "
                                   "          "))
(test-txtbmp (list mini6 mini6d) `("          " 
                                   "          "                                                 
                                   "          " 
                                   "          " 
                                   "          " 
                                   "          "
                                   "          " 
                                   "          " 
                                   "          " 
                                   "          "))
(test-txtbmp (list mini7) `("xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "xxxxx     " 
                            "          " 
                            "          " 
                            "          " 
                            "          " 
                            "          " 
                            "          " 
                            "          "
                            "          " 
                            "          " 
                            "          "))
(test-txtbmp (list mini7 mini8) `("xxxxx     "                                   
                                  "xx        "
                                  "xxxxx     " 
                                  "xxxxx     " 
                                  "xxxxx     " 
                                  "xxxxx     " 
                                  "xxxxx     " 
                                  "xxxxx     " 
                                  "xxxxx     " 
                                  "xxxxx     " 
                                  "xxxxx     "  
                                  "          " 
                                  "          " 
                                  "          " 
                                  "          " 
                                  "          " 
                                  "          "
                                  "          " 
                                  "          " 
                                  "          "))
(test-txtbmp (list mini7 mini9 mini8) `("xxxxx     "
                                        "xx        "
                                        "xxxxx     " 
                                        " xxxxx    "
                                        "xxxxx     "
                                        "xxxxx     "
                                        "xxxxx     "
                                        "xxxxx     " 
                                        "xxxxx     "
                                        "xxxxx     "
                                        "xxxxx     "
                                        "          "
                                        "          " 
                                        "          " 
                                        "          "
                                        "          "
                                        "          "
                                        "          "
                                        "          "
                                        "          "))
(test-txtbmp (list mini7 mini10) `("xxxxx     " 
                                   "xxxxx     "
                                   "xxxxx     "
                                   "xxxxx     "
                                   "xxxxx     " 
                                   "xxxxx     "
                                   "xxxxx     "
                                   "xxxxx     "
                                   "xxxxx     "
                                   "          "
                                   "          "
                                   "          " 
                                   "          " 
                                   "          " 
                                   "          " 
                                   "          " 
                                   "          "
                                   "          " 
                                   "          "
                                   "          "))
(test-txtbmp (list mini11 mini12) `("xxx       "
                                    "xxxxxx    " 
                                    "          " 
                                    "          " 
                                    "          " 
                                    "          "
                                    "          " 
                                    "          " 
                                    "          " 
                                    "          "))
(test-txtbmp (list mini14 mini13) `("xxxx      "
                                    "xxxx      " 
                                    "          "
                                    "          "
                                    "          "
                                    "          " 
                                    "          " 
                                    "          "
                                    "          " 
                                    "          "))
(test-txtbmp (list mini14) `("xxxx      "
                             "          "
                             "xxxx      "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "))
(test-txtbmp (list mini15) `("          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          " 
                             "          " 
                             "          " 
                             "          " 
                             "          "))


(define (mini16 txt)
  (send txt insert ";hello"))
(define (mini17 txt)
  (send txt insert "\"hello\""))
(define (mini18 txt)
  (send txt insert "#|hello"))
(test-txtbmp (list mini16) `("xxxxxx    "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "))
(test-txtbmp (list mini17) `("xxxxxxx   "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "
                             "          "))

(define (test-changestyle t start end c)
  (define sd (make-object style-delta%))
  (send sd set-delta-foreground c)
  (send t change-style sd start end))
  )