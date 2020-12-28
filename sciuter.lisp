;;;; sciuter.lisp
;; Trying again to make a shmup, this time with common lisp
;; and trivial-gamekit:
;; https://borodust.org/projects/trivial-gamekit/
;;
;; this file contains the implementation of logic and rendering of the game
;; built on top of entity.lisp, components.lisp and (eventually) system.lisp

(in-package #:sciuter)

(defparameter *width* 1024)
(defparameter *height* 800)

(gamekit:defgame the-game () ()
                 (:viewport-width *width*)
                 (:viewport-height *height*)
                 (:viewport-title "CL-Sciuter"))

(defvar *key-action-binding* '((:left . :left)
                               (:right . :right)
                               (:up . :up)
                               (:down . :down)
                               (:a . :left)
                               (:d . :right)
                               (:w . :up)
                               (:s . :down)
                               (:z . :fire)
                               (:y . :fire)
			       (:q . :quit)))

(defvar *action-bag* nil)

(defun bind-key-action (key action)
  (gamekit:bind-button key :pressed
                       (lambda ()
                         (push action *action-bag*)))
  (gamekit:bind-button key :released
                       (lambda ()
                         (alexandria:deletef *action-bag* action))))

(defparameter *bullet-drawable-component*
  (make-instance 'drawable
		 :parameters
		 (make-instance 'circle-drawing-parameters
				:fill-paint *yellow*
				:radius 10.0)))

(defparameter *bullet-damage* (make-instance 'damage :amount 10))
(defparameter *bullet-collision-mask* (make-instance 'collision-mask
						     :bits #b0001))
(defparameter *bullet-bounding-circle* (make-instance 'bounding-circle
						      :radius 10.0))
(defun spawn-bullet (x y dir speed)
  (let ((e (spawn-entity))
        (v (make-instance 'linear-velocity :dir dir
                                           :linear-speed speed))
        (p (make-instance 'point :pos (vec2 x y))))
    (attach-component e p)
    (attach-component e v)
    (attach-component e *screen-boundaries*)
    (attach-component e *bullet-drawable-component*)
    (attach-component e *bullet-bounding-circle*)
    (attach-component e *bullet-damage*)
    (attach-component e *bullet-collision-mask*)))

(defparameter *enemy-drawable-component*
  (make-instance 'drawable
		 :parameters
		 (make-instance 'circle-drawing-parameters
				:radius 60.0
				:fill-paint *green*
				:stroke-paint *yellow*)))

(defparameter *enemy-collision-mask* (make-instance 'collision-mask
						    :bits #b0001))

(defparameter *enemy-path-nodes*
  (make-array 5
	      :element-type 'path-node
	      :initial-contents
	      (list
	       (make-instance 'path-node
			      :pos (vec2 400 60)
			      :timestamp 0)
	       (make-instance 'path-node
			      :pos (vec2 900 420)
			      :timestamp 2)
	       (make-instance 'path-node
			      :pos (vec2 500 680)
			      :timestamp 4)
	       (make-instance 'path-node
			      :pos (vec2 100 220)
			      :timestamp 7)
	       (make-instance 'path-node
			      :pos (vec2 400 60)
			      :timestamp 10))))

(defparameter *enemy-linear-path* (make-linear-path *enemy-path-nodes* t))

(defun spawn-enemy (x y)
  (let ((e (spawn-entity :enemy))
        (p (make-instance 'point :pos (vec2 x y)))
	(c (make-instance 'bounding-circle :radius 60.0))
	(hp (make-instance 'health-points :amount 10)))
    (attach-component e p)
    (attach-component e *enemy-drawable-component*)
    (attach-component e c)
    (attach-component e hp)
    (attach-component e *enemy-collision-mask*)
    (attach-component e *enemy-linear-path*)))

(quote
  (detach-component :enemy 'linear-path)
  (attach-component :enemy *enemy-linear-path*)
 )

(defun spawn-player (x y)
  (let ((e (spawn-entity :player))
        (v (make-instance 'linear-velocity :dir (vec2 0.0 0.0)
                                           :linear-speed 350.0))
        (p (make-instance 'point :pos (vec2 x y)))
        (shot-timer (spawn-entity :player-shot-timer))
	;; ~20 shots/ second (at 60FPS)
        (timer-component (make-instance 'rolling-timer :seconds (/ 3.0 60)))
	(drawable (make-instance 'drawable
				 :parameters
				 (make-instance 'rect-drawing-parameters
						:width  15.0
						:height 15.0))))
    (attach-component e p)
    (attach-component e v)
    (attach-component e drawable)
    (attach-component shot-timer timer-component)))

(quote
  (setf (seconds (get-component :player-shot-timer 'rolling-timer)) (/ 5.0 60))
  (setf (linear-speed (get-component :player 'linear-velocity)) 350.0)
 )
(let ((score 0))
  (defun reset-score ()
    (setf score 0))

  (defun inc-score (amount)
    (incf score amount))

  (defun get-score ()
    score))

(defun reset-game ()
  (setf *action-bag* nil)
  (drain-unused-entities)
  (reset-score)
  (spawn-player 400 400)
  (spawn-enemy  500 600))

(defmethod gamekit:post-initialize ((app the-game))
  (loop for binding in *key-action-binding*
        do (destructuring-bind (key . action) binding
                               (bind-key-action key action)))
  (reset-game))

(defun draw-score ()
  (let ((score-str (format nil "SCORE: ~a" (get-score))))
    (draw-text score-str
	     (vec2 50 50)
	     :fill-color *black*)
    (draw-text score-str
	     (vec2 51 51)
	     :fill-color *red*)))

(defun draw-entities ()
  (loop for entity in (entities-with-component 'drawable)
        do (let ((position (pos (get-component entity 'point)))
		 (dp (parameters (get-component entity 'drawable))))
	     (render position dp))))

(defmethod gamekit:draw ((this the-game))
  (draw-entities)
  (draw-score))

(defun calculate-new-position (position velocity dt)
  (add position (step-velocity velocity dt)))

(defun update-positions (dt)
  (loop for entity in (entities-with-component 'linear-velocity)
        do (let ((vel      (get-component entity 'linear-velocity))
                 (position (get-component entity 'point)))
             (setf (pos position)
                   (calculate-new-position (pos position) vel dt)))))

(defun action-active? (action)
  (member action *action-bag*))

(defun get-new-control-direction ()
  "Calculate new player direction based on user input"
  (let ((dir (vec2 0 0)))
    (when (action-active? :right)
      (setf (x dir)  1))
    (when (action-active? :left)
      (setf (x dir) -1))
    (when (action-active? :up)
      (setf (y dir)  1))
    (when (action-active? :down)
      (setf (y dir) -1))
    (normalize dir)))

(defun update-player-direction ()
  (let ((new-dir (get-new-control-direction)))
    (setf (dir (get-component :player 'linear-velocity)) new-dir)))

(defun player-shot ()
  "Short: make the player to shot.
   Long: If the :fire button is pressed and the :player-shot-timer
   has expired spawn a bullet."
  (let ((pos (pos (get-component :player 'point)))
        (timer (get-component :player-shot-timer 'rolling-timer)))
    (when (and
	   (action-active? :fire)
	   timer
	   (expired-timer? timer))
      (spawn-bullet (x pos) (y pos) (normalize (vec2 -0.3 1)) 450)
      (spawn-bullet (x pos) (y pos) (vec2 0 1) 450)
      (spawn-bullet (x pos) (y pos) (normalize (vec2 0.3 1)) 450)
      (reset-timer timer))))

(defun remove-out-of-boundaries ()
  "Remove all entities that have a 'boundary component and are
   outside of that boundary limits."
  (loop for entity in (entities-with-component 'boundary)
	do (let ((position (pos (get-component entity 'point)))
		 (boundary (get-component entity 'boundary)))
	     (when (not (inside-boundaries? position boundary))
	       (retire-entity entity)))))

(defun resolve-collisions ()
  (loop for bullet in (entities-with-component 'damage)
	do (let ((bullet-pos (pos (get-component bullet 'point)))
		 (bullet-mask (bits (get-component bullet 'collision-mask)))
		 (bullet-radius (radius (get-component bullet 'bounding-circle))))
	     (loop named inner
		   for target in (entities-with-component 'health-points)
		   do (let ((target-pos
			      (pos (get-component target 'point)))
			    (target-mask
			      (bits (get-component target 'collision-mask)))
			    (target-radius
			      (radius (get-component target 'bounding-circle))))
			(when (and
			       (not (= 0
				       (logand bullet-mask target-mask)))
			       (circle-overlap bullet-pos bullet-radius
					       target-pos target-radius))
			  (inc-score 100)
			  (retire-entity bullet)
			  (return-from inner)))))))

(defun update-entities-with-path (dt)
  (loop for entity in (entities-with-component 'linear-path)
	do (let ((lp (get-component entity 'linear-path))
		 (position (get-component entity 'point)))
	     (setf (pos position) (update-path lp dt)))))

(defparameter *fixed-dt* (/ 1.0 60.0)) ;; ~60FPS

(defmethod gamekit:act ((this the-game))
  (update-timers *fixed-dt*)
  (update-player-direction)
  (update-positions *fixed-dt*)
  (update-entities-with-path *fixed-dt*)
  (player-shot)
  (remove-out-of-boundaries)
  (resolve-collisions)
  (when (action-active? :quit)
    (stop)))

(defun run ()
  (gamekit:start 'the-game)
  (gamekit:bind-any-gamepad
   (lambda (gamepad state)
     (if (eq :connected state)
	 (format t "gamepad connected ~a" gamepad)
	 (format t "gamepad disconnected ~a" gamepad)))))

(defun stop ()
  (gamekit:stop))
