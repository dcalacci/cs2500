;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tetra) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Tetra Game (basic Tetris clone)
;;Dan Calacci and Joshua Kosof
;;Some code courtesy of professors from Northeastern University
;;Last updated: 10/24/11
                           ;;;;;;;;;;;;;;;;;;;;;
                           ;; Version history ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10/21/11 - Wrote main code for program (everything, but collision buggy)
;; 10/22/11 - fixed collision bugs, made the game restart after it ends.
;; 10/24/11 - Fixed remaining collision bugs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------XXXXXXXXXXXXXXXXXXXX-------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;  Wishlist:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;1. Scoring: record time it takes for blocks to reach top of screen.        ;;
;;2. Implement pausing of the game                                           ;;
;;3. Enable rotation when a piece is on the side of the game screen.         ;;
;;4. PRIORITY: fix collision bug when turning into an existing block         ;;
;;5. PRIORITY: fix collision bugs when blocks drop into spawning area        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)

;; Board will be a 10x20 cell grid, with each cell being the size of a
;;  tetra block. 
;; Board-related definitions
(define BOARD-WIDTH 10)
(define BOARD-HEIGHT 20)
(define CELL-SIZE/PIXELS 20)
(define BACKGROUND (empty-scene (* BOARD-WIDTH  CELL-SIZE/PIXELS)
                                (* BOARD-HEIGHT CELL-SIZE/PIXELS)))


;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; A block is a (make-block Number Number Color)
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the the point around which the tetra rotates 
;; when it spins
(define-struct tetra (center blocks))

;; A set of blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.  Repititions are NOT allowed.

;; Board coordinate system is in grid/cell units,
;; with x & y increasing to the right & upward direction, respectively.

;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen
(define-struct world (tetra pile))

;;;;;;;;;;;;;;;;;;;;;;;;
;;  Beginning Tetras  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;"O" Tetra
(define blocko1 (make-block 4 19 'green))
(define blocko2 (make-block 5 19 'green))
(define blocko3 (make-block 4 20 'green))
(define blocko4 (make-block 5 20 'green))
(define bseto (list blocko1 blocko2 blocko3 blocko4))
(define Otetra (make-tetra (make-posn 4.5 19.5) bseto))


;;"I" Tetra
(define blocki1 (make-block 3 20 'blue))
(define blocki2 (make-block 4 20 'blue))
(define blocki3 (make-block 5 20 'blue))
(define blocki4 (make-block 6 20 'blue))
(define bseti (list blocki1 blocki2 blocki3 blocki4))
(define Itetra (make-tetra (make-posn 4 20) bseti))

;;"L" Tetra
(define blockl1 (make-block 4 18 'purple))
(define blockl2 (make-block 5 18 'purple))
(define blockl3 (make-block 4 19 'purple))
(define blockl4 (make-block 4 20 'purple))
(define bsetl (list blockl1 blockl2 blockl3 blockl4))
(define Ltetra (make-tetra (make-posn 4 19) bsetl))

;;"J" Tetra
(define blockj1 (make-block 4 18 'lightblue))
(define blockj2 (make-block 5 18 'lightblue))
(define blockj3 (make-block 5 19 'lightblue))
(define blockj4 (make-block 5 20 'lightblue))
(define bsetj (list blockj1 blockj2 blockj3 blockj4))
(define Jtetra (make-tetra (make-posn 5 19) bsetj))

;;"T" Tetra
(define blockt1 (make-block 4 20 'orange))
(define blockt2 (make-block 4 19 'orange))
(define blockt3 (make-block 5 19 'orange))
(define blockt4 (make-block 3 19 'orange))
(define bsett (list blockt1 blockt2 blockt3 blockt4))
(define Ttetra (make-tetra (make-posn 4 19) bsett))

;;"Z" Tetra
(define blockz1 (make-block 4 19 'pink))
(define blockz2 (make-block 5 19 'pink))
(define blockz3 (make-block 4 20 'pink))
(define blockz4 (make-block 3 20 'pink))
(define bsetz (list blockz1 blockz2 blockz3 blockz4))
(define Ztetra (make-tetra (make-posn 4 19) bsetz))

;;"S" Tetra
(define blocks1 (make-block 4 19 'red))
(define blocks2 (make-block 3 19 'red))
(define blocks3 (make-block 4 20 'red))
(define blocks4 (make-block 5 20 'red))
(define bsets (list blocks1 blocks2 blocks3 blocks4))
(define Stetra (make-tetra (make-posn 4 19) bsets))

;;Other Initial Definitions
(define init-tetra Ttetra)

(define init-world (make-world init-tetra empty))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-image-on-grid : Image, Number, Number, Image -> Image
;; Like place-image, but with "cell" coordinates
(define (place-image-on-grid img x y scene)
  (place-image img (* CELL-SIZE/PIXELS (+ x 1/2))
               (* CELL-SIZE/PIXELS (- BOARD-HEIGHT (+ y 1/2)))
               scene))
;;test
(check-expect (place-image-on-grid (square 19 "solid" "blue") 5 10 BACKGROUND)
              (place-image (square 19 "solid" "Blue") 110 190 BACKGROUND))

;; block-image : block -> image
;; turns a block into an image
(define (block-image block)
  (square
   (- CELL-SIZE/PIXELS 1)
   'solid
   (block-color block)))
;;Test
(check-expect (block-image (make-block 5 5 'red))
              (square 19 'solid 'red))


;; bset+scene : BSet, Image -> Image
;; Render a BSet onto an image.
(define (bset+scene bset scene)
  (cond [(empty? bset) scene]
        [else (place-image-on-grid 
               (block-image 
                (first bset))  
               (block-x (first bset)) 
               (block-y (first bset))
               (bset+scene (rest bset) scene))]))

;; tetra+scene : tetra, image -> image
;; renders a tetra onto an image
(define (tetra+scene tetra scene)
  (bset+scene (tetra-blocks tetra) 
              scene))

;; Test:
;(bset+scene bset2 BACKGROUND)
;(tetra+scene Otetra BACKGROUND)

;; world->image : world -> image
;; renders the world as an image
(define (world->image w)
  (tetra+scene (world-tetra w)
               (bset+scene (world-pile w) BACKGROUND)))
;;Test:
;(world->image init-world)

;;;;;;;;;;;;;;;;;;
;; Tetra Motion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

;;A Dir is a symbol:
;; -'up
;; -'down
;; -'left
;; -'right


;; move-tetra : tetra, dir -> tetra
;; moves t in a direction dir
(define (move-tetra t dir)
  (make-tetra
   (move-center (tetra-center t) dir)
   (move-bset (tetra-blocks t) dir)))

;;; move-center : center, dir -> center
;;; Move the center 1 unit in the given direction.
(define (move-center c dir)
  (cond [(symbol=? dir 'up)   (make-posn (posn-x c) (+ (posn-y c) 1))]
        [(symbol=? dir 'down) (make-posn (posn-x c) (- (posn-y c) 1))]
        [(symbol=? dir 'left) (make-posn (- (posn-x c) 1) (posn-y c))]
        [else                 (make-posn (+ (posn-x c) 1) (posn-y c))]))


;; move-bset : BSet, dir -> bset
;; moves bset in a given dir
(define (move-bset bset dir)
  (cond
    [(empty? bset) empty]
    [else
     (cons
      (move-block (first bset) dir)
      (move-bset (rest bset) dir))]))


;;; move-block : Block, Dir -> Posn
;;; Move b one unit in the given direction.
(define (move-block b dir)
  (cond [(symbol=? dir 'up)   (make-block (block-x b) (+ (block-y b) 1) (block-color b))]
        [(symbol=? dir 'down) (make-block (block-x b) (- (block-y b) 1) (block-color b))]
        [(symbol=? dir 'left) (make-block (- (block-x b) 1) (block-y b) (block-color b))]
        [else                 (make-block (+ (block-x b) 1) (block-y b) (block-color b))]))

;;;;;;;;;;;;;;;;;;;;
;; Tetra Rotation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

;;CCW ROTATION
;; bset-rotate-ccw : posn, bset -> bset
;; rotates b 90 degrees counterclockwise around c
(define (bset-rotate-ccw b c)
  (cond
    [(empty? b) empty]
    [else
     (cons (block-rotate-ccw (first b) c)
           (bset-rotate-ccw (rest b) c))]))

;; block-rotate-ccw : Posn Block -> Block
;; Rotate b 90 counterclockwise around c.
(define (block-rotate-ccw b c)
  (make-block (+ (posn-x c) (- (posn-y c) (block-y b)))
              (+ (posn-y c) (- (block-x b) (posn-x c)))
              (block-color b)))

;; tetra-rotate-ccw : tetra -> tetra
;; rotates t 90 degrees counterclockwise
(define (tetra-rotate-ccw t)
  (make-tetra
   (tetra-center t)
   (bset-rotate-ccw (tetra-blocks t) (tetra-center t))))

;;CW ROTATION
;; block-rotate-cw : Posn Block -> Block
;; Rotate b 90 clockwise around c.
(define (block-rotate-cw b c)
  (make-block (- (posn-x c) (- (posn-y c) (block-y b)))
              (- (posn-y c) (- (block-x b) (posn-x c)))
              (block-color b)))

;; bset-rotate-cw : posn, bset -> bset
;; rotates b 90 degrees clockwise around c
(define (bset-rotate-cw b c)
  (cond
    [(empty? b) empty]
    [else
     (cons (block-rotate-cw (first b) c)
           (bset-rotate-cw (rest b) c))]))

;; tetra-rotate-cw : tetra -> tetra
;; rotates t 90 degrees clockwise
(define (tetra-rotate-cw t)
  (make-tetra
   (tetra-center t)
   (bset-rotate-cw (tetra-blocks t) (tetra-center t))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision Detection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; bsetunderblock? : block, bset -> boolean
;; returns if the space underneath b is being occupied by a block in bset
(define (bsetunderblock? b bset)
  (cond
    [(empty? bset) false]
    [(and
      (=
       (block-x b)
       (block-x (first bset)))
      (=
       (- (block-y b) 1)
       (block-y (first bset)))) true]
    [else
     (bsetunderblock? b (rest bset))]))

#;(check-expect (bsetunderblock? (make-block 5 5 'orange) 
                                 (list (make-block 5 6 'orange) 
                                       (make-block 5 4 'orange))) true)
;; bsetunderbset? : bset, bset -> boolean
;; determines if there is a block in bup that is above a block in bdwn
(define (bsetunderbset? bdwn bup)
  (cond
    [(empty? bup) false]
    [(bsetunderblock? (first bup) bdwn) true]
    [else
     (bsetunderbset? bdwn (rest bup))]))
#;(check-expect (bsetunderbset? (list (make-block 5 5 'orange) (make-block 6 5 'orange))
                                 (list (make-block 5 6 'orange) 
                                       (make-block 5 4 'orange))) true)

;; next-to-wall-right? : bset -> boolean
;; determines if a bset is next to a wall on the right
(define (next-to-wall-right? bset)
  (cond
    [(empty? bset) false]
    [(= (block-x (first bset)) 9) true]
    [else
     (next-to-wall-right? (rest bset))]))
(check-expect (next-to-wall-right? (list (make-block 5 5 'orange) 
                                         (make-block 5 4 'orange))) false)

;; next-to-wall-left? : bset -> boolean
;; determines if a bset is next to a wall on the left
(define (next-to-wall-left? bset)
  (cond
    [(empty? bset) false]
    [(= (block-x (first bset)) 0) true]
    [else
     (next-to-wall-left? (rest bset))]))

;; next-to-bottom? : bset -> boolean
;; determines if a bset is next to the bottom of the frame
(define (next-to-bottom? bset)
  (cond
    [(empty? bset) false]
    [(< (block-y (first bset)) 1) true]
    [else
     (next-to-bottom? (rest bset))]))

;; abouttohitpile? : world, -> boolean
;; returns if the tetra in the world is about to collide with the pile
(define (abouttohitpile? w)
  (bsetunderbset? (world-pile w) (tetra-blocks (world-tetra w))))

;; abouttohitsomething? : world -> boolean
;; returns if the tetra in the world is about to collide with anything
;;  (walls, bottom, pile, etc)
(define (abouttohitsomething? w)
  (or
   (next-to-wall-right? (tetra-blocks (world-tetra w)))
   (next-to-wall-left?  (tetra-blocks (world-tetra w)))
   (next-to-bottom?     (tetra-blocks (world-tetra w)))
   (abouttohitpile? w)))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tetra/World Spawning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; makeNewTetra : number -> tetra
;; Makes a new tetra depending on what number is given
(define (makeNewTetra n)
  (cond
    [(= n 0) Itetra]
    [(= n 1) Otetra]
    [(= n 2) Ltetra]
    [(= n 3) Jtetra]
    [(= n 4) Ttetra]
    [(= n 5) Ztetra]
    [else Stetra]))


;; newWorld : world -> world
;; creates a new world, where the tetra is now part of the pile, 
;;  and a new random tetra has been spawned.
(define (newWorld w)
  (make-world
   (makeNewTetra (random 7))
   (append (tetra-blocks (world-tetra w))
           (world-pile w))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Big-Bang Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; world->world : world -> world
(define (world->world w)
  (cond
    [(and (abouttohitpile? w) (> (posn-y (tetra-center (world-tetra w))) 19))
     (make-world init-tetra empty)]
    [(next-to-bottom? (tetra-blocks (world-tetra w)))
     (newWorld w)]
    [(abouttohitpile? w) (newWorld w)]
    [else
     (make-world
      (move-tetra (world-tetra w) 'down)
      (world-pile w))]))

;; handle-key : world key-event -> world
;; handles key-presses in the game.
(define (handle-key w key)
  (cond [(key=? key "n") init-world]        
        [(not (or (abouttohitpile? w) (next-to-bottom? (tetra-blocks (world-tetra w)))))
         (cond
           [(key=? key "down")
            (make-world (move-tetra (world-tetra w) (string->symbol key))
                        (world-pile w))]
           [(and (key=? key "left")
                 (not (next-to-wall-left?  (tetra-blocks (world-tetra w)))))
            (make-world (move-tetra (world-tetra w) (string->symbol key))
                        (world-pile w))]
           [(and (key=? key "right")
                 (not (next-to-wall-right? (tetra-blocks (world-tetra w)))))
            (make-world (move-tetra (world-tetra w) (string->symbol key))
                        (world-pile w))]
           [(and (key=? key "a") 
                 (not (abouttohitsomething? w)))
            (make-world (tetra-rotate-ccw (world-tetra w)) (world-pile w))]
           [(and (key=? key "s")
                 (not (abouttohitsomething? w)))
            (make-world (tetra-rotate-cw (world-tetra w)) (world-pile w))]
           [(abouttohitpile? w)
            (newWorld w)]
           [else w])]
        [else w]))

(big-bang (make-world init-tetra empty)
          (to-draw   world->image )
          (on-tick   world->world .8)
          (on-key    handle-key))