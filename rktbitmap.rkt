#lang racket/base

(require racket/gui/base
         racket/class
         racket/list
         racket/match
         racket/file
         framework)

(define state%  
  (class text%
    (init-field [event-queue? #f])
    (define is-do-a-little-work-enqueued?  #f)
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
      (maybe-queue-do-a-little-work?))    
    
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
      (maybe-queue-do-a-little-work?))

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
             get-text
             find-snip)
    
    (define/private (update-bitmap)
      (define bdc (new bitmap-dc% [bitmap primary-bmp]))
      (send bdc erase)
      ;; iterate over the lines in t
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
    
    (define/private (maybe-queue-do-a-little-work?)
      (cond
        [(up-to-date?) (void)]
        [is-do-a-little-work-enqueued? (void)]
        [(not event-queue?) (void)]
        [else
         (set! is-do-a-little-work-enqueued? #t)
         (queue-callback 
          (lambda () (do-a-little-work))
          #f)]))
         
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
         ; change this to loop for x time
         ; current-inexact-milliseconds
         ; guarantee at least one iteration
         ; stops after >10ish milliseconds
         (let ([y invalid-start])
           (cond 
             [(= invalid-start invalid-end)
              (update-one-line y bdc)
              (clear-invalid)]
             [else 
              (update-one-line y bdc)
              (update-invalid-start (+ 1 y))]))
         (send bdc set-bitmap #f)
         (maybe-queue-do-a-little-work?)]))
    
    (define/private (update-one-line y bdc)
      (define (get-color p)
        (define snip (find-snip p 'after-or-none))
        (define style (send snip get-style))
        (send style get-foreground))
      (define (color->rgb c)
        (list (send c red)
              (send c green)
              (send c blue)))
      
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
             (send bdc set-pixel x y (make-object color% "white"))]
            [else 
             (send bdc set-pixel x y (make-object color% (get-color i)))]))))
    
    (define/public (up-to-date?)
      (and (not invalid-start) (not invalid-end)))

    ))


(module+ main
(define f (new frame% [label ""] 
               [width 200]
               [height 200]))
(define t (new state% [event-queue? (lambda () (send c refresh))]))
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
  )

(define (r txt)
  (send txt insert (file->string "C:/Program Files/Racket/collects/compiler/private/xform.rkt")))
(define nt (new state %))
(send nt insert (file->string "C:/Program Files/Racket/collects/compiler/private/xform.rkt"))
(define counter 0)
(let loop ()
  (send nt do-a-little-work)
  


; benchmark time(do-a-little-work) on a large file
; and (send t ) some edits
; do-a-little-work may need to return a boolean indicating whether or not it needs to do more work
; do-a-little-work should then not queue the callback itself
; whoever calls do-a-little-work should queue the callback based on dalw return
; dalw should always take 10ms
; maybe plot numbers

;create a text
;call load-file (collection-file-path "rep.rkt" "drracket" "private")
;call (set-filename #f) which is a method of text
;get the bitmap in sync (call do-a-little-work until it's done)
;do some insertions and time the insert call
;time the do-a-little-work calls (not the loop)
;i.e. do an insertion, do-a-little-work and etc.
;(time exp...)