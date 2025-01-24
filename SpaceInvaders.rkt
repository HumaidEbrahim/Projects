;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)


(define INVADER-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)


(define INVADE-RATE 100)
(define INVADE-PROBABILITY 10)

(define BACKGROUND .)

(define BACKGROUND2 (empty-scene WIDTH HEIGHT))

(define INVADER .)

(define INVADER2
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "green")       ;gun
                     (rectangle 20 10 "solid" "green"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-POS (- HEIGHT (/ (image-height TANK) 2)))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define R-BOUND (- WIDTH (image-width INVADER2)))
(define L-BOUND (image-width INVADER2))

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ListOfInvaders is one of:
;; - empty
;; - (cons invader ListOfInvader)

(define LOI1 empty)
(define LOI2 (list (make-invader 200 100 10) (make-invader 50 150 -10)))
(define G4 (make-game (list (make-invader 150 0 1)) empty T0))
#;
(define (fn-for-loi loi)
  (cond[(empty? loi) (...)]
       [else
        (...(fn-for-invader (first loi))
            (fn-for-loi (rest loi)))]))

;; ListOfMissiles is one of:
;; - empty
;; (cons missile ListOfMissiles)

(define LOM1 empty)
(define LOM2 (list (make-missile 150 300) (make-missile 10 20)))

#;
(define (fn-for-lom lom)
  (cond[(empty? lom) (...)]
       [else
        (...(fn-for-missile (first lom))
            (fn-for-lom (rest lom)))]))

;; ========================================================
;; Functions:

;; Game -> Game
;; start world with (main (make-game empty empty TANK)

(define (main g)
  (big-bang g
    (on-tick  game-state)
    (to-draw render)
    (on-key handle-key)
    (stop-when game-over? end-screen)))


;; 1. Game -> Game
;; tick missiles invaders and tank

(define (game-state g)
  (make-game 
   (invade(advance-invaders (game-invaders g) (game-missiles g)))
   (advance-missiles (game-missiles g) (game-invaders g))
   (move-tank (game-tank g)))) 

;; 2. Game -> Image
;; render current game state to screen

(define (render g)
  (render-invaders (game-invaders g)(render-missiles (game-missiles g) (render-tank (game-tank g))))) ;stub



;; 3. Game KeyEvent -> Game
;; change the direction of the tank when left and right keys and fire missiles when space

(check-expect (handle-key G0 "left") (make-game empty empty (make-tank 150 -1)))
(check-expect (handle-key G0 "right") (make-game empty empty (make-tank 150 1)))
(check-expect (handle-key G0 " ") (make-game empty (list(make-missile 150 TANK-POS)) T0))
(check-expect (handle-key G0 "a") G0)

;(define (handle-key g ke) g) ;stub

(define (handle-key g ke)
  (cond [(key=? ke "right") (make-game (game-invaders g) (game-missiles g)(turn-tank (game-tank g) 1))]
        [(key=? ke "left") (make-game (game-invaders g) (game-missiles g)(turn-tank (game-tank g) -1))]
        [(key=? ke " ") (make-game (game-invaders g)(fire-missile (game-missiles g) (game-tank g)) (game-tank g))]
        [else g]))

;; ============== Tank =====================

;; 4. Tank -> Tank
;; move the tank by tank speed units to the left -1 or right 1
(check-expect (move-tank T0) (make-tank (+ 150 TANK-SPEED) 1))
(check-expect (move-tank T2) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (move-tank (make-tank R-BOUND 1)) (make-tank (- R-BOUND TANK-SPEED) -1))
(check-expect (move-tank (make-tank L-BOUND -1)) (make-tank (+ L-BOUND TANK-SPEED) 1))

;(define (move-tank t) t) ;stub

(define (move-tank t)
  (cond
    [(= R-BOUND (tank-x t)) (make-tank (- (tank-x t) TANK-SPEED) -1)]
    [(= L-BOUND (tank-x t)) (make-tank (+ (tank-x t) TANK-SPEED) 1)]
    [(= 1 (tank-dir t)) (make-tank (+ (tank-x t) TANK-SPEED) 1)]
    [(= -1 (tank-dir t)) (make-tank (- (tank-x t) TANK-SPEED) -1)]))


;; 5. Tank Natural -> Tank
;; turn tank left -1 or right 1
(check-expect (turn-tank T2 1) (make-tank 50 1))
(check-expect (turn-tank T2 -1) (make-tank 50 -1))
(check-expect (turn-tank T0 -1) (make-tank 150 -1))

;(define (turn-tank t d) t) ;stub

(define (turn-tank t d)
  (cond [(= 1 d) (make-tank (tank-x t) 1 )]
        [(= -1 d) (make-tank (tank-x t) -1 )]))

;; 5.2 Tank -> Image
;; render tank

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-POS BACKGROUND))

;;  ============== Missiles =====================

;; 6. ListOfMissile Tank -> ListOfMissile
;; spawns a new missile at tank-x and tank height
(check-expect (fire-missile empty T0) (cons (make-missile (tank-x T0) TANK-POS) empty))
(check-expect (fire-missile LOM2 T2) (cons (make-missile (tank-x T2) TANK-POS) LOM2))

;(define(fire-missile lom t) empty) ;stub

(define (fire-missile lom t)
  (cons (make-missile (tank-x t) TANK-POS ) lom))

;; 7. ListOfMissile -> ListOfMissile
;; advance all missiles by missile speed per tick and filter hits
(check-expect (advance-missiles empty empty) empty)
(check-expect (advance-missiles LOM2 empty) (list (make-missile 150 (- 300 MISSILE-SPEED)) (make-missile 10 (- 20 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 150 0)) empty) empty)
(check-expect (advance-missiles (list (make-missile 150 200)) (list (make-invader 150 200 1))) empty)

;(define (advance-missiles lom loi) empty) ;stub

(define (advance-missiles lom loi)
  (cond[(empty? lom) empty]
       [else
        (if (or (filter-missile? (first lom)) (hit-mi? (first lom) loi))
            (advance-missiles (rest lom) loi)
            (cons (adv-missile (first lom)) (advance-missiles (rest lom) loi)))]))


;; 7.2 Missile -> Missile
;; advance missiles y by speed
(check-expect (adv-missile M1) (make-missile (missile-x M1) (- 300 MISSILE-SPEED)))

;(define (adv-missile m) m) ;stub

(define (adv-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; 7.3 ListOfMissiles -> Image
;; render missiles on background
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM2 BACKGROUND) (place-image MISSILE 150 300 (place-image MISSILE 10 20 BACKGROUND)))

;(define (render-missiles lom) BACKGROUND) ;stub

(define (render-missiles lom img)
  (cond[(empty? lom) img]
       [else
        (place-missile (first lom)
                       (render-missiles (rest lom) img))])) 
 

;; 7.4 Missile -> Image
;; place missile on rest of the image at x y position
(check-expect (place-missile M1 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))

;(define (place-missile m img) BACKGROUND) ;stub

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; 7,5 Missile -> Boolean
;; filter off-screen missiles y <= 0 produce true
(check-expect (filter-missile? M1) false)
(check-expect (filter-missile? (make-missile 150 0)) true)

;(define (filter-missile? m) false) ;stub

(define (filter-missile? m)
  (<= (missile-y m) 0))


;; ============== Invaders =====================

;; 8. ListOfInvader ListOfMissile -> ListOfInvader
;; move invaders down at 45deg angle
(check-expect (advance-invaders empty empty) empty)
(check-expect (advance-invaders (list (make-invader 150 0 1)
                                      (make-invader 150 0 -1)) empty) (list (make-invader (+ 150 INVADER-SPEED) (+ 0 INVADER-SPEED) 1)
                                                                            (make-invader (- 150 INVADER-SPEED) (+ 0 INVADER-SPEED) -1 )))

(check-expect (advance-invaders (list (make-invader R-BOUND 250 1)
                                      (make-invader L-BOUND 250 -1)) empty) (list (make-invader (- R-BOUND INVADER-SPEED) (+ 250 INVADER-SPEED) -1)
                                                                                  (make-invader (+ L-BOUND INVADER-SPEED) (+ 250 INVADER-SPEED) 1)))

(check-expect (advance-invaders (list (make-invader 150 10 1)) (list(make-missile 160 20))) empty) 
                                
;(define (advance-invaders loi lom) empty) ;stub

(define (advance-invaders loi lom)
  (cond[(empty? loi) empty]
       [else
        (if (hit-im? (first loi) lom)
            (advance-invaders (rest loi) lom)
            (cons (adv-invader (first loi)) (advance-invaders (rest loi) lom))
            )]))
 

;; 8.2 Invader -> Invader
;; advance invader and change dir if out of bounds
(check-expect (adv-invader (make-invader 150 0 1)) (make-invader (+ 150 INVADER-SPEED) (+ 0 INVADER-SPEED) 1))   ; R
(check-expect (adv-invader (make-invader 150 0 -1)) (make-invader (- 150 INVADER-SPEED) (+ 0 INVADER-SPEED) -1)) ; L
(check-expect (adv-invader (make-invader R-BOUND 250 1)) (make-invader (- R-BOUND INVADER-SPEED) (+ 250 INVADER-SPEED) -1)) ; R Bound
(check-expect (adv-invader (make-invader L-BOUND 250 -1)) (make-invader (+ L-BOUND INVADER-SPEED) (+ 250 INVADER-SPEED) 1)) ; L Bound

;(define (adv-invader inv) inv) ;stub

(define (adv-invader inv)
  (cond[(>= (invader-x inv) R-BOUND) (make-invader (- R-BOUND INVADER-SPEED) (+ (invader-y inv ) INVADER-SPEED) -1)]
       [(= L-BOUND (invader-x inv)) (make-invader (+ L-BOUND INVADER-SPEED) (+ (invader-y inv ) INVADER-SPEED) 1) ]
       [(= 1 (invader-dx inv)) (make-invader (+ (invader-x inv) INVADER-SPEED) (+ (invader-y inv) INVADER-SPEED) 1)]
       [(= -1 (invader-dx inv)) (make-invader (- (invader-x inv) INVADER-SPEED) (+ (invader-y inv) INVADER-SPEED) -1)]))


;; 9. ListOfInvader -> ListOfInvader
;; spawns invaders randomly

; (define (invade loi)) ;stub

(define (invade loi)
  (if (< (random INVADE-RATE) INVADE-PROBABILITY)
  (spawn-invader loi)
  loi))
        
;; 9. ListOfInvader -> ListOfInvader
;; spawn invader at random x and height 0
(check-random (spawn-invader empty) (list (make-invader (random WIDTH) 0 1)))
(check-random (spawn-invader LOI2) (cons (make-invader (random WIDTH) 0 1) LOI2))

;(define (spawn-invader loi ) empty) ;stub

(define (spawn-invader loi)
(cons (make-invader (random WIDTH) 0 1) loi))

;; 10. ListOfInvader Image -> Image
;; renders invaders at correct location per tick
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER 200 100 (place-image INVADER 50 150 BACKGROUND)))

;(define (render-invaders loi img) BACKGROUND) ;stub

(define (render-invaders loi img)
  (cond[(empty? loi) img]
       [else
        (place-invader (first loi)
                       (render-invaders (rest loi) img))]))

;; 10.2 Invader Image -> Image
;; place invader on image
(check-expect (place-invader I1 (render-missiles empty  (render-tank T0))) (place-image INVADER 150 100 (render-missiles empty  (render-tank T0))))

;(define (place-invader inv img) INVADER) ;stub

(define (place-invader inv img)
  (place-image INVADER (invader-x inv) (invader-y inv) img))


;; 11. Invader ListOfMissile -> Boolean
;; produce true if invader intersects with any missiles in list

;(define (hit-im? inv lom) false) ;stub

(define (hit-im? inv lom)
  (cond[(empty? lom) false]
       [else
        (if (intersect? inv (first lom))
            true
            (hit-im? inv (rest lom)))]))

;(define (hit-mi? m loi) false) ;stub

(define (hit-mi? m loi)
  (cond[(empty? loi) false]
       [else
        (if (intersect? (first loi) m)
            true
            (hit-mi? m (rest loi)))]))

;; 11.2  Invader Missile -> Boolean
;; produce true if missile-y is within 10 pixels froms invader 
(check-expect (intersect? I1 M1) false)
(check-expect (intersect? (make-invader 150 150 1) (make-missile 150 150)) true)
(check-expect (intersect? (make-invader 150 150 1) (make-missile 160 155)) true)
(check-expect (intersect? (make-invader 150 150 1) (make-missile 140 160)) true)

;(define (intersect? inv m) false);

(define (intersect? inv m)
  (and  (<= (- (missile-y m) (invader-y inv)) HIT-RANGE)
        (and (<= (- (missile-x m) (invader-x inv)) 10)
             (>= (- (missile-x m) (invader-x inv)) -10))))


;; ============== Game Over =====================

;; 12. Gamestate -> ListOfInvader
;; produces true if game over

;(define (game-over? gs) boolean) ;stub

(define (game-over? gs)
  (any-landed? (game-invaders gs)))

;; 12.2  ListOfInvader -> Boolean
;; produces true if any invader in list lands
(check-expect (any-landed? LOI1) false)
(check-expect (any-landed? LOI2) false)
(check-expect (any-landed? (list (make-invader 150 HEIGHT 1))) true)
(check-expect (any-landed? (list (make-invader 150 HEIGHT -1))) true)

;(check-expect (any-landed? loi) false) ;stub

(define (any-landed? loi)
  (cond[(empty? loi) false]
       [else
        (or (landed? (first loi)) (any-landed? (rest loi)))]))

;; 12.2 Invader-> Boolean
;; produces true if invader-y equals height
(check-expect (landed? I1) false)
(check-expect (landed? I2)true)
(check-expect (landed? I3)true)

;(define (landed? inv) false) ;stub

(define (landed? inv)
  (>= (invader-y inv) HEIGHT))

;; 13. Image -> Image
;; renders final screen when gameover
(check-expect (end-screen G0) (overlay (text "Game Over" 50 "white") BACKGROUND))

;(define (end-screen GAME-OVER) ;stub

(define (end-screen gs)
  (overlay (text "Game Over" 50 "white") BACKGROUND))
