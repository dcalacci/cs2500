;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab5_exercise11-14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
;; A Ball is:
;;  - (make-ball Number Number Number Number)
(define-struct ball (x y vx vy c))

;; Interpretation: A Ball represents an object at position X/Y with
;;   a velocity VX in the X direction, and VY in the Y direction.

;; All numbers are Pixel units, and computer graphics coordinates

;; Constants
(define WIDTH 400)
(define HEIGHT 400)
(define GRAV-ACC 3)
(define SIZE 10)

;; A LoB is one of:
;; -empty
;; (cons ball lob)

#;(define (ball-template b)
    (ball-x b) ... (ball-y b)...(ball-vx b)...(ball-vy b)...)

#;(define (lob-template lob)
    (cond
      [(empty? lob) ..]
      [else
       ...(ball-y (first lob))... (lob-template (rest lob))]))

;; off-screen? : ball -> boolean
;; determines if a given ball is offscreen
(define (off-screen? a-ball)
  (or (>= (ball-x a-ball) WIDTH)
      (<= (ball-x a-ball) 0)
      (>= (ball-y a-ball) HEIGHT)
      (<= (ball-y a-ball) 0)))

;; gravity : Ball -> Ball
;; consumes a ball and produces a new one with Y velocity
;; increased by GRAV-ACC.
(define (gravity b)
  (make-ball (ball-x b)
             (ball-y b)
             (ball-vx b)
             (+ (ball-vy b)
                GRAV-ACC)
             (ball-c b)))

;; move : Ball -> Ball
;; consumes a ball and moves its x and y by
;; the ball's velosities ( vx and vy)
(define (move b)
  (make-ball (+ (ball-x b)
                (ball-vx b))
             (+ (ball-y b)
                (ball-vy b))
             (ball-vx b)
             (ball-vy b)
             (ball-c b)))

;; bounce: ball -> ball
;; consumes a ball and alters it's x and y velocities to reflect a bounce
(define (bounce b)
  (make-ball
   (ball-x b)
   (ball-y b)
   (ball-vx b)
   (* -1 (- (ball-vy b) 5))
   (ball-c b)))

;; draw-lob : lob -> image
;; consumes an lob and returns a scene that has
;; a circle of size SIZE for each ball in the list
;; in the correct coordinates
(define (draw-lob lob)
  (cond [(empty? lob) (empty-scene WIDTH HEIGHT)]
        [else (place-image (circle SIZE "solid" (ball-c (first lob)))
                           (ball-x (first lob))
                           (ball-y (first lob))
                           (draw-lob (rest lob)))]))

;; on-screen : lob -> lob
;; consumes an lob and filters balls that are off-screen
(define (on-screen lob)
  (cond [(empty? lob) empty]
        [else (cond [(off-screen? (first lob))
                     (on-screen (rest lob))]
                    (else (cons (first lob) (on-screen (rest lob)))))]))

;; gravity-all : lob -> lob
;; consumes lob and returns an lob with applied gravity on them
(define (gravity-all lob)
  (cond [(empty? lob) empty]
        [else (cons (gravity (first lob))
                    (gravity-all (rest lob)))]))



;; move-all : lob -> lob
;; consumes lob and returns an lob with applied move on them
(define (move-all lob)
  (cond [(empty? lob) empty]
        [(>= (ball-y (first lob))
             (- HEIGHT 30))
         (cons (move (bounce (first lob)))
               (move-all (rest lob)))]
        [else (cons (move (first lob))
                    (move-all (rest lob)))]))

;; number->color : number -> color
;; takes a number and produces a color
(define (number->color n)
  (cond [(= n 0) "red"]
        [(= n 1) "orange"]
        [(= n 2) "yellow"]
        [(= n 3) "green"]
        [(= n 4) "blue"]
        [(= n 5) "indigo"]
        [else "purple"]))


;; mouse : LoB Number Number MouseEvent -> LOP
;; Add a new random ball
(define (mouse lob x y me)
  (cond [(string=? me "drag")
         (cons (make-ball x y
                          (- (random 9) 4)
                          (- (+ (random 10) 10))
                          (number->color (random 7)))
               lob)]
        [else lob]))

;; tick : LoB -> LoB
;; Move, gravitize, then filter out all 
;;   the off-screen Balls
(define (tick lob)
  (on-screen (move-all (gravity-all lob))))

(define last (big-bang empty
                       (on-mouse mouse)
                       (to-draw draw-lob)
                       (on-tick tick)))