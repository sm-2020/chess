#lang racket/gui

(require embedded-gui) ; for snip-width and snip-height

; A chess game using racket's editor toolkit

;define an instance of the snip-class% object. This snip class? is used when serializing
;snips from the pasteboard.
(define chess-piece-snip-class
  (make-object
      (class snip-class%
        (super-new)
        (send this set-classname "chess-piece-snip"))))

;snip class object needs to be registered with the Racket editor gui
(send (get-the-snip-class-list) add chess-piece-snip-class)

; Definition of the snip class
; Use the same snip class for all chess pieces, since the only difference between them
; is how they are displayed. To have a minimal working snip, three things need to be present in the derived class
; a call to set-snipclass which associates the snip instance with the snip class that was previously defined
; a get-extent method which the pasteboard% uses to determine the size of the snip
; a draw method, which is used to draw the snip onto the pasteboard canvas.
; Arguments
; a glyph, which is a string representing the Unicode character for the piece,
; a font used to render the glyph
; a size which is the size in pixels of the chess piece (since the piece is a square it will have the same width and height)
(define chess-piece%
  (class snip%
    (init-field glyph font size [location #f]) ; location false means a piece is not on the board

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)
    
    (super-new)
    (send this set-snipclass chess-piece-snip-class)
  
    ; get-extent method is used by the pasteboard% to obtain the dimensions of the snip
    ; it just reports the size as both the width and height, a device context, dc is passed in,
    ; together with the position of the snip on the canvas as the x and y coordinates, in return,
    ; the pasteboard% expects the width, height and some other parameters to be filled in by
    ; our object (the other parameters have to do with snips that are part of a text editor
    ; and represent text, they don?t concern us here, so they are set to 0).
    ; The method uses box-es (single elemet vector)for the output parameters, so we need to use box-set! to set the output value.
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    ; draw method is used to paint the snip contents onto the canvas. It receives the device context,
    ; dc and the snip position on the canvas. 
    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
         (send dc draw-text glyph (+ x ox) (+ y oy))))
))
;; Implementation of the chess board
; To display the chess board pattern on the background, we can create a derived pasteboard% class
; and implement the on-paint method. Since the chess board itself is static and the user does not interact with it,
; this approach is the simplest.
; The on-paint method is invoked twice during each repaint:
;                  once before the snips are painted and once after.
; The before? parameter tells us which invocation it is. The method also receives a device context, dc, plus
; some other parameters which determine the area that needs repainting ? these parameters would help in speeding up
; complex redraw operations, but since our drawing needs are simple, we just redraw the entire board every time. 
(define chess-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)))

    ;The chess pieces are added to the chess board using the insert method,
    ; which allows placing a snip at any coordinate, or at (0, 0) if no coordinates are specified.
    ; To place a snip in its correct position, based on the location stored inside the snip, we can derive
    ; the pasteboard%?s after-insert method, which is invoked after each snip is inserted and call position-piece for the snip.
    
    (define/augment (after-insert chess-piece . rest) ;override pasteboard%'s after-insert method in order to inject position-piece
      (position-piece this chess-piece))

    (define (position-piece board piece)
      (define-values (canvas-width canvas-height)
        (let ((c (send board get-canvas)))
         (send c get-size)))
      (define-values (square-width square-height)
        (values (/ canvas-width 8) (/ canvas-height 8)))
      (define-values (rank file)
        (location->rank-file (send piece get-location)))
      (define-values (square-x square-y)
        (values (* file square-width) (* rank square-height)))
      (define piece-width (snip-width piece))
      (define piece-height (snip-height piece))
      (send board move-to piece
            (+ square-x (/ (- square-width piece-width) 2))
            (+ square-y (/ (- square-height piece-height) 2))))

    ; The location->rank-file function, used by position-piece, converts a chess board location
    ; into the row and column of the corresponding square on the board. It is simpler for our program to use
    ; chess board locations, written as ?d3? or ?f5?, as they can be read as imputs from the user or from a file
    (define (location->rank-file location)
      (unless (and (string? location) (= (string-length location) = 2))
        (raise-argument-error 'location "valid chess position a1 .. h8" location))
      (define file
        (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h) (string-ref location 0)))
      (define rank
        (index-of '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1) (string-ref location 1)))
       (unless (and rank file)
         (raise-argument-error 'location "valid chess position a1 .. a8" location))
        (values rank file))
))

; Drawing resources such as brushes, pens and fonts must be created and set on the device context
; and draw operations such as draw-rectangle, or draw-text use the last set drawing resource.
;
; Creating drawing resources, such as pen%, brush% and font% objects directly is expensive, so Racket
; provides a ?manager? type object which only creates them as needed:the-pen-list, the-brush-list, the-font-list.
; because of this, we don?t need to keep these objects around in the class, we can simply retrieve them every time draw-chess-board is invoked
;  
; The method clears the device context before drawing ? since the method is supposed to cover the entire area of the
; board and we only draw the ?black? squares, this is the simplest method to ensure that no previous image data is left
; over from previous drawing operations.
;  
; The code does not assume that the board is a certain size, or even that it is a rectangle. Instead, it determines the size
; dynamically and calculates the explicit width and height of each square, as well as the positions of the labels.
; This is helpful, this function will not have to change if a chess board of a different size is created, and will also handle dynamic resizes of the canvas.
(define (draw-chess-board dc)
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (define font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width 8))
  (define cell-height (/ dc-height 8))
  (define margin 3)
 
  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)

  (for* ([row (in-range 8)] [col (in-range 8)]
         #:when (or (and (odd? row) (even? col))
                    (and (even? row) (odd? col))))
    (define-values [x y]  (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  (for* ([(rank index) (in-indexed '("8" "7" "6" "5" "3" "2" "1"))])
         (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
         (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
         (send dc draw-text rank margin y))
  
  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h"))])
    (define-values [w h _1 _2] (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
    (send dc draw-text file x (- dc-height h margin)))
)

;;------------------------------------------------------
(define chess-piece-data
      (hash
        "K" #\u2654 "Q" #\u2655 "R" #\u2656 "B" #\u2657 "N" #\u2658 "P" #\u2659
         "k" #\u265A "q" #\u265B "r" #\u265C "b" #\u265D "n" #\u265E "p" #\u265F))

; this function receieves the piece code and creates a snip piece
(define (make-chess-piece id)
   (define glyph (hash-ref chess-piece-data id))
   (define font (send the-font-list find-or-create-font 30 'default 'normal 'normal))
   (new chess-piece% [glyph (string glyph)] [font font] [size 30]))
; Test code to see how this snip works

;Create a pasteboard to hold all the chess peices
(define board (new chess-board%))
;Toplevel window of our app
(define topLevel (new frame% [label "Chess board"] [width (* 50 8)] [height (* 50 8)]))
; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent topLevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))

(send topLevel show #t) ;; show the toplevel frame
;; Insert one of each of the chess pieces onto the board, so we can see them
;; and drag them around.
(for ([id (in-hash-keys chess-piece-data)])
  (define piece (make-chess-piece id))
  (send board insert piece (random (* 50 6)) (random (* 50 6))))