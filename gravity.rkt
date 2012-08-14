;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname grav_fork_lab8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/universe)
(require 2htdp/image)

;; Scene Width and Height...
(define WIDTH 400)
(define HEIGHT 400)

;; A Planet is:
;;   (make-planet Number Number Number Number String)
(define-struct planet (x y vx vy color))
;; x,y represent the planet's current location, and vx,vy represent
;;   its velocity (speed/direction)

;; Number of colors
(define NUM-COLORS 3)

;; color-for : Number -> String
;; Return a color for the given number
(define (color-for n)
  (cond [(= n 0) "red"]
        [(= n 1) "green"]
        [else "blue"]))

; Exercise 21 :
;; move-all : [Listof Planet] -> [Listof Planet]
;; moves all the planets in the list
(define (move-all lop)
  (local [(define (move-planet p) (make-planet (+ (planet-x p)
                                                  (planet-vx p))
                                               (+ (planet-y p)
                                                  (planet-vy p))
                                               (planet-vx p)
                                               (planet-vy p)
                                               (planet-color p)))]
    (map move-planet lop)))
;; draw-lop : [Listof planet] -> imae
;; makes a scene with all the planets in lop in it
(define (draw-lop lop)
  (local ((define (draw-planet p scene)
            (place-image
             (circle 20 'solid (planet-color p))
             (planet-x p)
             (planet-y p)
             scene)))
    (foldr draw-planet (empty-scene WIDTH HEIGHT) lop)))

;; distance : Planet Planet -> Number
;; Calculate the distance between the Planets
(define (distance p1 p2)
  (sqrt (+ (sqr (- (planet-x p1) (planet-x p2)))
           (sqr (- (planet-y p1) (planet-y p2))))))

;; apply-gravity : Planet Planet -> Planet
;; Apply the gravitational effects of the other Planet to the
;;   second Planet (note the order of the arguments...)
(define (apply-gravity p-other p)
  (local [(define dist (distance p p-other))
          (define dx (- (planet-x p) (planet-x p-other)))
          (define dy (- (planet-y p) (planet-y p-other)))]
    (cond [(< dist 1) p]
          [else (make-planet (planet-x p) (planet-y p)
                             (- (planet-vx p) (/ dx dist))
                             (- (planet-vy p) (/ dy dist))
                             (planet-color p))])))

;; gravity-one : planet [listof planet] -> planet
;; applies gravitational effects of the list of planets to a single planet
(define (gravity-one p lop)
  (foldr apply-gravity p lop))

;; gravity-all : [Listof Planet] -> [Listof Planet]
;; applies the effects of all planets to all planets
(define (gravity-all lop)
  (local
    ((define (gravity p) (gravity-one p lop)))
    (map gravity lop)))

;; tick : [Listof Planet] -> [Listof Planet]
;; Apply gravity, then move all the Planets
(define (tick lop)
  (move-all (gravity-all lop)))

;; mouse : [Listof Planet] Number Number String -> [Listof Planet]
;; Add a new planet where the mouse was clicked
(define (mouse lop x y me)
  (cond [(string=? me "button-down")
         (cons (make-planet x y 0 0 (color-for (random NUM-COLORS)))
               lop)]
        [else lop]))

;; Start with no planets...
(define last (big-bang empty
                       (on-mouse mouse)
                       (to-draw draw-lop)
                       (on-tick tick 1/20)))