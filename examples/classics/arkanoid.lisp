(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defclass player ()
  ((pos
     :initform nil
     :accessor player-pos)
   (size
     :initform nil
     :accessor player-size)
   (life
     :initform nil
     :accessor player-life)))

(defclass ball ()
  ((pos
     :initform nil
     :accessor ball-pos)
   (speed
     :initform nil
     :accessor ball-speed)
   (radius
     :initform nil
     :accessor ball-radius)
   (active
     :initform nil
     :accessor ball-active-p)))

(defclass brick ()
  ((pos
     :initform nil
     :accessor brick-pos)
   (active
     :initform nil
     :accessor brick-active-p)))

(defconstant +player-lives-count+ 5)
(defconstant +lines-of-bricks+ 5)
(defconstant +bricks-per-line+ 20)

(defparameter *w* 800)
(defparameter *h* 450)
(defparameter *game-over* nil)
(defparameter *pause* nil)
(defparameter *player* (make-instance 'player))
(defparameter *ball* (make-instance 'ball))
(defparameter *brick-size* nil)
(defparameter *bricks* 
  (make-array (list +lines-of-bricks+ +bricks-per-line+)
              :initial-element nil))

(defun initialize-game-state ()
    ;;  init brick size
    (setf *brick-size* (vec (/ (get-screen-width) +bricks-per-line+) 40))
    ;; init player
    (with-slots (pos size life) *player*
      (setf pos (vec (/ *w* 2) (* *h* (/ 7 8))))
      (setf size (vec (/ *w* 10) 20))
      (setf life +player-lives-count+))
    ;; init ball
    (with-slots (pos speed radius active) *ball*
      (setf pos (vec (/ *w* 2) (- (* *h* (/ 7 8)) 30)))
      (setf speed (vec 0 0))
      (setf radius 7.0)
      (setf active nil))
  ;; init all bricks
  (let ((initial-down-position 50)
        (bsx (vx *brick-size*))
        (bsy (vy *brick-size*)))
    (loop for i from 0 below +lines-of-bricks+ do
          (loop for j from 0 below +bricks-per-line+ do
                (setf (aref *bricks* i j) (make-instance 'brick))
                (with-slots (pos active) (aref *bricks* i j)
                  (setf pos (vec (+ (* j bsx) (/ bsx 2))
                                 (+ (* i bsy) initial-down-position)))
                  (setf active T))))))

(defun draw-game ()
  (with-drawing
    (clear-background :raywhite)
    (if (not *game-over*)
      (progn
        (with-slots (pos size life) *player*
          (let ((posx  (round (vx pos)))
                (posy  (round (vy pos)))
                (sizex (round (vx size)))
                (sizey (round (vy size))))
            ;; draw player bar
            (draw-rectangle (- posx (/ sizex 2))
                            (- posy (/ sizey 2))
                            sizex
                            sizey
                            :black)
            ;; draw player lives
            (loop for i from 0 below life do
                  (draw-rectangle (+ 20 (* i 40))
                                  (- *h* 30)
                                  35
                                  10
                                  :lightgray))))
        ;; draw ball
        (with-slots (pos radius) *ball*
          (draw-circle-v pos radius :maroon))
        ;; draw bricks
        (loop for i from 0 below +lines-of-bricks+ do
              (loop for j from 0 below +bricks-per-line+ do
                    (with-slots (pos active) (aref *bricks* i j)
                      (let ((bsx  (round (vx *brick-size*)))
                            (bsy  (round (vy *brick-size*)))
                            (posx (round (vx pos)))
                            (posy (round (vy pos))))
                        (when active 
                         (if (zerop (mod (+ i j) 2))
                             (draw-rectangle (- posx (/ bsx 2))
                                             (- posy (/ bsy 2))
                                             bsx
                                             bsy
                                             :gray)
                             (draw-rectangle (- posx (/ bsx 2))
                                             (- posy (/ bsy 2))
                                             bsx
                                             bsy
                                             :darkgray)))))))
        (when *pause*
            (draw-text "GAME PAUSED" 
                       (- (/ *w* 2)
                          (/ (measure-text "GAME PAUSED" 40) 2))
                       (- (/ *h* 2) 40)
                       40
                       :gray)))
      (draw-text "PRESS [ENTER] TO PLAY AGAIN"
                (- (/ (get-screen-width) 2)
                   (/ (measure-text "PRESS [ENTER] TO PLAY AGAIN" 20) 2))
                (- (/ (get-screen-height) 2) 50)
                20
                :gray))))

(defun update-game ()
  (if (not *game-over*)
      (progn 
        (when (is-key-pressed :key-p)
          (setf *pause* (not *pause*)))
        (unless *pause*
          ;; player movement logic
          (with-slots (pos size) *player*
           (let ((posx (vx pos))
                 (posy (vy pos))
                 (sizex (vx size)))
            (cond ((is-key-down :key-left) 
                   (setf pos (vec (decf posx 5) posy)))
                  ((<= (- posx (/ sizex 2)) 0) 
                   (setf pos (vec (/ sizex 2) posy)))
                  ((is-key-down :key-right)
                   (setf pos (vec (incf posx 5) posy)))
                  ((>= (+ posx (/ sizex 2)) *w*)
                   (setf pos (vec (- *w* (/ sizex 2)) posy))))))
          ;; ball launching logic
          (with-slots (active speed pos radius) *ball*
            (let ((posx (vx pos))
                  (posy (vy pos))
                  (speedx (vx speed))
                  (speedy (vy speed)))
              (unless active
               (when (is-key-pressed :key-space)
                 (setf active T)
                 (setf speed (vec 0 -5.0))))
            ;; ball movement logic
             (if active
                   (setf pos (vec (incf posx speedx) (incf posy speedy)))
                 (let ((playerposx (vx (player-pos *player*))))
                   (setf pos (vec playerposx (- (* *h* (/ 7 8)) 30)))))
             (let ((life (player-life *player*)))
               ;; collision ball vs wall
               (cond ((or (>= (+ posx radius) *w*)
                          (<= (- posx radius) 0))
                      (setf speed (vec (* speedx -1) speedy)))
                     ((<= (- posy radius) 0)
                      (setf speed (vec speedx (* speedy -1))))
                     ((>= (+ posy radius) *h*)
                      (setf speed (vec 0 0))
                      (setf active nil)
                      (setf life (decf life)))))
             (let ((playerposx (vx (player-pos *player*)))
                   (playerposy (vy (player-pos *player*)))
                   (playersizex (vx (player-size *player*)))
                   (playersizey (vy (player-size *player*))))
               ;; collision player vs ball
               (when (check-collision-circle-rec 
                       pos 
                       radius
                       (make-rectangle :x (- playerposx (/ playersizex 2))
                                       :y (- playerposy (/ playersizey 2))
                                       :width playersizex
                                       :height playersizey))
                 (when (> speedy 0)
                   (setf speed (vec 
                                 (/ (- posx playerposx)
                                    (* (/ playersizex 2) 5))
                                 (* speedy -1))))))))
          (loop for i from 0 below +lines-of-bricks+ do
                               (loop for j from 0 below +bricks-per-line+ do
                                     (let* ((brick (aref *bricks* i j))
                                            (brick-p (brick-active-p brick))
                                            (brickposx (vx (brick-pos brick)))
                                            (brickposy (vy (brick-pos brick)))
                                            (bricksizex (vx *brick-size*))
                                            (bricksizey (vy *brick-size*))
                                            (ballradius (ball-radius *ball*))
                                            (ballspeed (ball-speed *ball*))
                                            (ballspeedy (vy ballspeed))
                                            (ballspeedx (vx ballspeed))
                                            (ballpos (ball-pos *ball*))
                                            (ballposx (vx ballpos))
                                            (ballposy (vy ballpos)))
                                       (when brick-p
                                         (cond ((and (<= (- ballposy ballradius) 
                                                         (+ brickposy (/ bricksizey 2)))
                                                     (> (- ballposy ballradius)
                                                        (+ brickposy 
                                                           (/ bricksizey 2) 
                                                           ballspeedy))
                                                     (< (abs (- ballposx brickposx))
                                                        (+ (/ bricksizex 2) 
                                                           (* ballradius (/ 2 3))))
                                                     (< ballspeedy 0))
                                                (setf brick-p nil)
                                                (setf ballspeed (vec ballspeedx 
                                                                     (* -1 ballspeedy))))
                                               ((and (>= (+ ballposy ballradius)
                                                         (- brickposy (/ bricksizey 2)))
                                                     (< (+ ballposy ballradius)
                                                        (- brickposy 
                                                           (/ bricksizey 2)
                                                           ballspeedy))
                                                     (< (abs (- ballposx brickposx))
                                                        (+ (/ bricksizex 2)
                                                           (* ballradius (/ 2 3))))
                                                     (> ballspeedy 0))
                                                (setf brick-p nil)
                                                (setf ballspeed (vec ballspeedx
                                                                     (* ballspeedy -1))))
                                               ((and (>= (+ ballposx ballradius)
                                                         (- brickposx (/ bricksizex 2)))
                                                     (< (+ ballposx ballradius)
                                                        (+ (- brickposx 
                                                              (/ bricksizex 2))
                                                           ballspeedx))
                                                     (< (abs (- ballposy brickposy))
                                                        (+ (/ bricksizey 2)
                                                           (* ballradius (/ 2 3))))
                                                     (> ballspeedx 0))
                                                (setf brick-p nil)
                                                (setf ballspeed (vec (* ballspeedx -1)
                                                                     ballspeedy)))
                                               ((and (<= (- ballposx ballradius)
                                                         (+ brickposx (/ bricksizex 2)))
                                                     (> (- ballposx ballradius)
                                                        (+ brickposx
                                                           (/ bricksizex 2)
                                                           ballspeedx))
                                                     (< (abs (- ballposy brickposy))
                                                        (+ (/ bricksizey 2)
                                                           (* ballradius (/ 2 3))))
                                                     (< ballspeedx 0))
                                                (setf brick-p nil)
                                                (setf ballspeed (vec (* ballspeedx -1)
                                                                     ballspeedy))))))))))
                                                           
      (when (is-key-pressed :key-kp-enter)
        (initialize-game-state)
        (setf *game-over* nil))))

(defun main ()
  (with-window (*w* *h* "Arkanoid")
    (initialize-game-state)
    (set-target-fps 60)
    (loop while (not (window-should-close))
          do (progn 
               (update-game)
               (draw-game)))))
                    
(main)
