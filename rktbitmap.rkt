#lang racket/base

(require racket/gui/base
         racket/class
         racket/list
         racket/match)

(define state%
  (class text%
    (define invalid-start #f)
    (define invalid-end #f)
    (define primary-bmp (make-bitmap 10 10))
    (define secondary-bmp (make-bitmap 10 10))
    (super-new)
    
    ; situations to consider
    ; for insert
    ;; remember to consider if insert is before or after
    ;; one/multiple characters in one line
    ;; one/multiple characters in multiple lines
    ;;; copy bitmap down
    ;;; and then update the invalid region (inserted stuff)
    ;;; invalid region is the inserted region
    
    ; eventually just updates invalid region
    ; either update the bitmap if the work is sufficiently small
    ; or just update start/end
    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      ; check size of invalid region
      ; if it's one line, update invalid region to include the line
      (define ps (position-paragraph start))
      (define pe (position-paragraph (+ start len)))
      (cond 
        ; one line
        [(= ps pe)
         (union-invalid ps pe)]
        ; insert at end of bitmap, no space left (i.e. no line shifting)
        ; remove this section because this case doesn't test properly
        ; only two cases, the one above and the one below
        ; insert in the middle, line shifting
        [else
         (define invalid-region-size (- (last-paragraph) ps))        
         (cond 
           [(>= (last-paragraph) (send primary-bmp get-height))     
            (define h (send primary-bmp get-height))
            (define w (send primary-bmp get-width))
            (define new-primary-bmp (make-bitmap w (* 2 h)))
            (define new-secondary-bmp (make-bitmap w (* 2 h)))
            (define bdc (new bitmap-dc% [bitmap new-primary-bmp]))
            (send bdc draw-bitmap-section 
                  primary-bmp 0 0 0 0 w ps)
            (send bdc draw-bitmap-section
                  primary-bmp 0 pe 0 ps w (- h ps))            
            (set! primary-bmp new-primary-bmp)
            (set! secondary-bmp new-secondary-bmp)]
           [else 
            (define bdc (new bitmap-dc% [bitmap secondary-bmp]))
            (define h (send primary-bmp get-height))
            (define w (send primary-bmp get-width))
            (send bdc draw-bitmap-section
                  primary-bmp 0 0 0 0 w ps)
            (send bdc draw-bitmap-section
                  primary-bmp 0 pe 0 ps w invalid-region-size)
            (swap-bitmaps)])
         (union-invalid ps pe)])
      
      #|
      (printf "insert istart:~a iend:~a\n" invalid-start invalid-end)
      (print primary-bmp)
      (newline)
|#)
    
    
    ; how to handle multiple non-consective regions?
    ; for delete - invalid region should only be one line? except when things are pending
    ;; one/multiple characters in one line
    ;;; fix invalid line
    ;; one line
    ;;; copy stuff up
    ;; multiple lines
    ;;; copy stuff up
    ;;; check first line of deleted region
    ;;; check last line of deleted region in case of overlap
    (define/augment (on-delete start len)
      (inner (void) on-delete start len)
      ; check size of invalid region
      (define ps (position-paragraph start))
      (define pe (position-paragraph (+ start len)))
      (cond
        [(equal? ps pe) 
         (union-invalid ps pe)]
        [else
         ; copy stuff up
         (define invalid-region-size (- (last-paragraph) pe))
         (define bdc (new bitmap-dc% [bitmap secondary-bmp]))
         (define h (send secondary-bmp get-height))
         (define w (send secondary-bmp get-width))
         (send bdc draw-bitmap-section primary-bmp 0 0 0 0 w ps)
         (send bdc draw-bitmap-section primary-bmp 0 ps 0 pe w invalid-region-size)
         (send bdc set-pen "blue" 1 'transparent)
         (send bdc set-brush "white" 'solid)
         (send bdc draw-rectangle 0 (- (last-paragraph) 1) w (- h ps (- invalid-region-size pe)))
         (swap-bitmaps)
         (union-invalid ps ps)])
      
      #|
      (printf "delete istart:~a iend:~a\n" invalid-start invalid-end)
      (print primary-bmp)
      (newline)
|#)

      ; if start and end are in the same line
      ; update invalid region to include line
      ; if they are on different lines, 
      ; copy the bitmap up
      ; and update invalid region
    
    ; how to get colors of words
    (define/augment (after-change-style start len) 
      (inner (void) after-change-style start len))
    
    (inherit paragraph-start-position
             paragraph-end-position
             position-paragraph
             last-paragraph
             get-character
             insert
             delete
             get-text)
    
    (define/private (update-bitmap)
      (define bdc (new bitmap-dc% [bitmap primary-bmp]))
      (send bdc erase)
      ;; iterate over the characters in t
      ;; update the bitmap based on them      
      (for ([y (+ 1 (last-paragraph))])
        (update-one-line y bdc))
      (send bdc set-bitmap #f))
    
    (define/public (get-bitmap)
      primary-bmp)       
    (define/private (swap-bitmaps)
      (define temp primary-bmp)
      (set! primary-bmp secondary-bmp)
      (set! secondary-bmp temp))
    (define/private (update-invalid-start nstart)
      (set! invalid-start nstart))
    (define/private (union-invalid start end)
      (set! invalid-start 
            (if invalid-start
                (min start invalid-start)
                start))
      (set! invalid-end 
            (if invalid-end
                (max end invalid-end)
                end)))
    (define/private (clear-invalid)
      (set! invalid-start #f)
      (set! invalid-end #f))
    
    ; do-a-little-work : void
    ; three iterations of (update-bitmap)
    ; for now, iterate by lines
    ; check to see if there's work to do
    ; size of bitmap
    
    ; check timer (later)
    ; data structure for invalid region?
    ; assume that bitmap has the right size
    (define/public (do-a-little-work)
      (cond 
        [(up-to-date?) (void)]
        [else 
         (define bdc (new bitmap-dc% [bitmap primary-bmp]))
         (let ([y invalid-start])
           (cond 
             [(= invalid-start invalid-end)
              (update-one-line y bdc)
              (clear-invalid)]
             [else 
              (update-one-line y bdc)
              (update-invalid-start (+ 1 y))]))
         (send bdc set-bitmap #f)]))
    
    ; one problem, if the paragraph is shorter after a delete
    ; then there will be remnants from the previous paragraph 
    ; after the last position in the paragraph
    ; that don't get redrawn
    (define/private (update-one-line y bdc)
      (send bdc set-pen "blue" 1 'transparent)
      (send bdc set-brush "white" 'solid)
      (define w (send secondary-bmp get-width))
      (send bdc draw-rectangle 0 y w 1)
      (for ([i (in-range (paragraph-start-position y) 
                         (paragraph-end-position y))])
        (define x (- i (paragraph-start-position y)))
        (let ([ch (get-character i)])
          (cond 
            [(char-whitespace? ch) 
             (send bdc set-pixel x y (make-object color% "red"))]
            [else (send bdc set-pixel x y (make-object color% "black"))]))))
    
    (define/public (up-to-date?)
      (and (not invalid-start) (not invalid-end)))
    ))


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
      (if (= 0 (send color red))
          #\x
          #\space)))))

; turn into bytes and use equal?
; empty bytes will be 4x width and height
; set-argb-pixels
(define (compare-bitmap bmp1 bmp2)
  (define b1 (make-bytes 40000))
  (define c1 (make-bytes 40000))
  (send bmp1 get-argb-pixels 0 0 100 100 b1) 
  (send bmp2 get-argb-pixels 0 0 100 100 c1)
  (equal? b1 c1))

(define (insert-test0 txt)
  (send txt insert "hello")
  (send txt delete 0 5)
  (send txt insert "hello"))

(define (insert-test3 txt)
  (send txt insert "hello")
  (send txt insert "hello")
  (send txt delete 3 8))

(define (insert-test1 txt)
  (send txt insert "hello" 0)
  (send txt insert "hello" 99))

(define (insert-newlines txt)
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n"))

(define (delete-test1 txt)
  (send txt insert "\n\n\n\n\n\n")
  (send txt insert "hellohellohellohellohello")
  (send txt insert "\n\n\n\n\n\n")
  (send txt delete 0 6)
  (send txt delete 0 12))

(define (insert-test4 txt)
  (for ([i (in-range 101)])
    (send txt insert "aaaaaaaaaaaaaaaaaaaa")
    (send txt insert "\n")))

(define (delete-test4 txt)
  (send txt delete 10 40)
  (send txt delete 50 75)
  (send txt delete 100 136)
  (send txt delete 308 367)
  (send txt delete 320 336)
  (send txt delete 400 578))

(define (delete-test2 txt)
  (send txt insert "hellohellohello\n")
  (send txt insert "hellohellohello\n")
  (send txt insert "hellohellohello\n")
  (send txt delete 5 10))

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
(test-txtbmp (list mini4 mini3) (make-bitmap 10 10))
(test-txtbmp (list mini5 mini5d) (make-bitmap 10 10))
(test-txtbmp (list mini6 mini6d) (make-bitmap 10 10))
(test-txtbmp (list mini7) (make-bitmap 10 10))
(test-txtbmp (list mini7 mini8) (make-bitmap 10 10))
(test-txtbmp (list mini7 mini9 mini8) (make-bitmap 10 10))
(test-txtbmp (list mini7 mini10) (make-bitmap 10 10))
(test-txtbmp (list mini11 mini12) (make-bitmap 10 10))
(test-txtbmp (list mini14 mini13) (make-bitmap 10 10))
(test-txtbmp (list mini14) (make-bitmap 10 10))
(test-txtbmp (list mini15) (make-bitmap 10 10))
;(test-txtbmp insert-test3 (make-bitmap 100 100))
;(test-txtbmp insert-test (make-bitmap 100 100))
;(test-txtbmp insert-newlines (make-bitmap 100 100))
;(test-txtbmp insert-test2 (make-bitmap 100 100))
;(test-txtbmp insert-test1 (make-bitmap 100 100))
;(test-txtbmp delete-test1 (make-bitmap 100 100))
;(test-txtbmp insert-test4 (list delete-test4 insert-newlines) (make-bitmap 100 100))
;(test-txtbmp delete-test2 empty (make-bitmap 100 100))
;(test-txtbmp insert-test4 empty (make-bitmap 100 100))
;(test-txtbmp insert-test4 (list delete-test4) (make-bitmap 100 100))

#| 
(define f (new frame% [label ""] 
               [width 200]
               [height 200]))
(define t (new state%))
(define hp (new horizontal-panel% [parent f]))
(define ec (new editor-canvas% 
                [parent hp]
                [editor t]))
(define c (new canvas%
               [parent hp]
               [paint-callback
                (λ (c dc)
                  (send dc draw-bitmap (send t get-bitmap) 0 0))]))
(send f show #t)
|#