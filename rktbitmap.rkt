#lang racket/base

(require racket/gui/base
         racket/class
         racket/list
         racket/match)

(define state%
  (class text%
    (define invalid-start #f)
    (define invalid-end #f)
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
        [(= pe (send primary-bmp get-height))
         (update-invalid-start (min ps invalid-start))
         (update-invalid-end (max pe invalid-end))
         (when (>= (last-paragraph) (send primary-bmp get-height))           
           (define h (send primary-bmp get-height))
           (define w (send primary-bmp get-width))
           (define temp (make-bitmap w (* 2 h)))
           (set! secondary-bmp temp)
           ;(define b (make-bytes (* 4 w h)))
           ;(send primary-bmp get-argb-pixels 0 0 w h b)
           ;(send temp set-argb-pixels 0 0 w h b)
           (send secondary-bmp draw-bitmap-section 
                 primary-bmp 0 0 0 0 w h)
           ;(set! primary-bmp temp)
           (set! primary-bmp secondary-bmp)
           (set! secondary-bmp (make-bitmap w (* 2 h))))]
        ; insert in the middle, line shifting
        [else
         (define invalid-region-size (+ (- pe ps) 1))
         (define b (make-bytes (* 4 100 (- (last-paragraph) invalid-region-size))))
         (send primary-bmp get-argb-pixels 0 start 100 invalid-region-size b)
         (if (>= (last-paragraph) (send primary-bmp get-height))
             0 ; double size of bitmap
             (send primary-bmp set-argb-pixels 0 (+ 1 pe) 100 invalid-region-size b))
         (update-invalid-start (min ps invalid-start))
         (update-invalid-end (max pe invalid-end))
         0])
      
      #;(printf "istart:~a iend:~a\n" invalid-start invalid-end))
    
    
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
         (update-invalid-start (min ps invalid-start))
         (update-invalid-end (max pe invalid-end))]
        [(equal? 1 (- pe ps))
         ; copy stuff up
         (define invalid-region-size (+ 1 (- (last-paragraph) pe)))
         ;(define b (make-bytes (* 4 100 invalid-region-size)))
         ;(send primary-bmp get-argb-pixels 0 pe 100 invalid-region-size b)
         ;(send primary-bmp set-argb-pixels 0 ps 100 invalid-region-size b)
         ;(update-invalid-start (min ps invalid-start))
         ; set to last-paragraph b/c the lines that we copied aren't cleared
         (update-invalid-end (last-paragraph))]
        [else 0])
      
      #;(printf "istart:~a iend:~a\n" invalid-start invalid-end)
      ; if start and end are in the same line
      ; update invalid region to include line
      ; if they are on different lines, 
      ; copy the bitmap up
      ; and update invalid region
    
    ; how to get colors of words
    (define/augment (after-change-style start len) 
      (inner (void) after-change-style start len)
      #;(update-bitmap start iend))
    
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
      (update-invalid-start nstart))
    (define/private (update-invalid-end nend)
      (update-invalid-end nend))
    
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
        [else (define bdc (new bitmap-dc% [bitmap primary-bmp]))
              ;(printf "istart:~a iend:~a\n" invalid-start invalid-end)
              #;(printf "pnum:~a pstart:~a pend:~a\n" 
                        invalid-start 
                        (paragraph-start-position invalid-start) 
                        (paragraph-end-position invalid-start))
              (let ([y invalid-start])
                (cond 
                  [(equal? invalid-start invalid-end)
                   (update-one-line y bdc)
                   (update-invalid-start #f)
                   (update-invalid-end #f)]
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
      ;(>= invalid-start invalid-end)
      (and (not invalid-start) (not invalid-end)))
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
(define (test-txtbmp fn fn2 bmp)
  (define nt (new state%))
  (fn nt)
  (let loop ()
    (send nt do-a-little-work)
    ;(print (send nt get-bitmap))
    ;(newline)
    (unless (send nt up-to-date?) (loop)))
  (when fn2
    (fn2 nt)
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
  (send txt delete 3 8)
  )

(define (insert-test1 txt)
  (send txt insert "hello" 0)
  (send txt insert "hello" 99))

(define (insert-test txt)
  (send txt insert "hello")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt delete 10 50)
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt delete 100 150))

(define (insert-test2 txt)
  (send txt insert "hello")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "\n")
  (send txt insert "a a a a a a a a a a a a a a a a a a a")
  (send txt insert "a a a a a a a a a a a a a a a a a a a"))

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

;(test-txtbmp insert-test3 (make-bitmap 100 100))
;(test-txtbmp insert-test (make-bitmap 100 100))
;(test-txtbmp insert-newlines (make-bitmap 100 100))
;(test-txtbmp insert-test2 (make-bitmap 100 100))
;(test-txtbmp insert-test1 (make-bitmap 100 100))
;(test-txtbmp delete-test1 (make-bitmap 100 100))
(test-txtbmp insert-test4 (make-bitmap 100 100))

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