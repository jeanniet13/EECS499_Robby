#lang racket/base

(require racket/gui/base
         racket/class
         racket/list
         racket/match)

(define state%
  (class text%
    (define invalid-start +inf.0)
    (define invalid-end -inf.0)
    (define primary-bmp (make-bitmap 100 100))
    (define secondary-bmp (make-bitmap 100 100))
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
         (update-invalid-start (min ps invalid-start))
         (update-invalid-end (max pe invalid-end))]
        ; insert at end of bitmap, no space left (i.e. no line shifting)
        ; remove this section because this case doesn't test properly
        ; only two cases, the one above and the one below
        ; insert in the middle, line shifting
        [else
         (define invalid-region-size (+ (- (last-paragraph) ps) 1))
         ;(define b (make-bytes (* 4 100 (- (last-paragraph) invalid-region-size))))
         ;(send primary-bmp get-argb-pixels 0 start 100 invalid-region-size b)         
         (cond 
           [(>= (last-paragraph) (send primary-bmp get-height))            
            (define bdc (new bitmap-dc% [bitmap secondary-bmp]))
            (define h (send primary-bmp get-height))
            (define w (send primary-bmp get-width))
            (define temp (make-bitmap w (* 2 h)))
            (set! secondary-bmp temp)
            (send bdc draw-bitmap-section 
                  primary-bmp 0 0 0 0 w h)
            (set! primary-bmp secondary-bmp)
            (set! secondary-bmp (make-bitmap w (* 2 h)))]
           [else 
            (define bdc (new bitmap-dc% [bitmap secondary-bmp]))
            (define h (send primary-bmp get-height))
            (define w (send primary-bmp get-width))
            (send bdc draw-bitmap-section
                  primary-bmp 0 0 0 0 w h)
            (send bdc draw-bitmap-section
                  primary-bmp 0 (+ 1 pe) 0 ps 100 invalid-region-size)])
         (update-invalid-start (inexact->exact (min ps invalid-start)))
         (update-invalid-end (inexact->exact (max pe invalid-end)))])
      
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
         (update-invalid-start (inexact->exact (min ps invalid-start)))
         (update-invalid-end (inexact->exact (max pe invalid-end)))]
        [else
         ; copy stuff up
         (define invalid-region-size (+ 1 (- (last-paragraph) pe)))
         (define bdc (new bitmap-dc% [bitmap primary-bmp]))
         (send bdc draw-bitmap-section primary-bmp 0 ps 0 pe 100 invalid-region-size)
         (send bdc set-pen "white" 1 'transparent)
         (send bdc set-brush (make-object color% "white") 'transparent)
         (send bdc draw-rectangle 0 (+ 1 pe) 100 invalid-region-size)
         (update-invalid-start (inexact->exact (min ps invalid-start)))
         (update-invalid-end (inexact->exact (max ps invalid-end)))])
      
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
    (define/private (update-invalid-end nend)
      (set! invalid-end nend))
    
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
              (update-invalid-start +inf.0)
              (update-invalid-end -inf.0)]
             [else 
              (update-one-line y bdc)
              (update-invalid-start (+ 1 y))]))
         (send bdc set-bitmap #f)]))
    
    (define/private (update-one-line y bdc)
      (for ([i (in-range (paragraph-start-position y) 
                         (+ 1 (paragraph-end-position y)))])
        (define x (- i (paragraph-start-position y)))
        (let ([ch (get-character i)])
          (cond 
            [(char-whitespace? ch) 
             (send bdc set-pixel x y (make-object color% "red"))]
            [else (send bdc set-pixel x y (make-object color% "black"))]))))
    
    (define/public (up-to-date?)
      (and (= +inf.0 invalid-start) (= -inf.0 invalid-end)))
    ))

(define (color->bytes colorobj numpixels)
  (define r (send colorobj red))
  (define g (send colorobj green))
  (define b (send colorobj blue))
  (define a (send colorobj alpha))
  (define ret (make-bytes (* 4 numpixels)))
  (for ([i (in-range 0 (* 4 numpixels) 4)]
        [j (in-range 1 (* 4 numpixels) 4)]
        [k (in-range 2 (* 4 numpixels) 4)]
        [l (in-range 3 (* 4 numpixels) 4)])
    (bytes-set! ret i (inexact->exact (* 255 a)))
    (bytes-set! ret j r)
    (bytes-set! ret k g)
    (bytes-set! ret l b))
  ret)

; test suite
; test case - function that takes a text and calls various methods, including insert
;  second piece is the bitmap that's the correct answer for the results of the function
; create a text, call the function, do-a-little-work until up-to-date?, compare bitmaps
; test-txtbmp : (-> text void) bitmap -> bool
; random testing
(define (test-txtbmp fn fn2list bmp)
  (define nt (new state%))
  (fn nt)
  (let loop ()
    (send nt do-a-little-work)
    (unless (send nt up-to-date?) (loop)))
  (for ([f fn2list])
    (f nt)
    (let loop()
      (send nt do-a-little-work)
      (unless (send nt up-to-date?) (loop))))
  (print (send nt get-bitmap))
  (print bmp)
  (newline)
  (compare-bitmap (send nt get-bitmap) bmp))

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
    (send txt insert "a")
    (send txt insert "\n")))

(define (delete-test4 txt)
  (send txt delete 10 20)
  (send txt delete 40 50))

(define (delete-test2 txt)
  (send txt insert "hellohellohello\n")
  (send txt insert "hellohellohello\n")
  (send txt insert "hellohellohello\n")
  (send txt delete 5 10))

;(test-txtbmp insert-test3 (make-bitmap 100 100))
;(test-txtbmp insert-test (make-bitmap 100 100))
;(test-txtbmp insert-newlines (make-bitmap 100 100))
;(test-txtbmp insert-test2 (make-bitmap 100 100))
;(test-txtbmp insert-test1 (make-bitmap 100 100))
;(test-txtbmp delete-test1 (make-bitmap 100 100))
;(test-txtbmp insert-test4 (list delete-test4 insert-newlines) (make-bitmap 100 100))
;(test-txtbmp delete-test2 empty (make-bitmap 100 100))
(test-txtbmp insert-test4 empty (make-bitmap 100 100))

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