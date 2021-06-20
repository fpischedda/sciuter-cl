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

(defparameter *enemy-drawable-component*
  (make-instance 'drawable
		 :parameters
		 (make-instance 'circle-drawing-parameters
				:radius 60.0
				:fill-paint *green*
				:stroke-paint *yellow*)))

(defparameter *enemy-collision-mask* (make-instance 'collision-mask
						    :bits #b0001))

(defun make-path-node (pos timestamp)
  "helper function to create a path-node instance"
  (make-instance 'path-node
		 :pos pos
		 :timestamp timestamp))

(defparameter *enemy-path-nodes*
  (make-array 5
	      :element-type 'path-node
	      :initial-contents
	      (list
	       (make-path-node (vec2 400 60) 0)
	       (make-path-node (vec2 900 420) 2)
	       (make-path-node (vec2 500 680) 4)
	       (make-path-node (vec2 100 220) 7)
	       (make-path-node (vec2 400 60) 10))))

(defun make-enemy-path (relative-pos)
  (make-linear-path *enemy-path-nodes* :relative-pos relative-pos :repeat t))

(defun spawn-enemy (x y &key entity-id relative-pos (bullets-second 50))
  "create a enemy entity, with all the needed components;
   return the newly created entity."
  (let ((e (spawn-entity entity-id))
	(c (make-instance 'bounding-circle :radius 60.0))
	(hp (make-instance 'health-points :amount 10))
	(eb (make-enemy-behavior bullets-second)))
    (attach-component e (vec2 x y) :position)
    (attach-component e *enemy-drawable-component*)
    (attach-component e c)
    (attach-component e hp)
    (attach-component e *enemy-collision-mask*)
    (attach-component e (make-enemy-path relative-pos))
    (attach-component e eb)
    e))

(defun spawn-player (x y)
  (let ((e (spawn-entity :player))
        (v (make-instance 'linear-velocity :dir (vec2 0.0 0.0)
                                           :linear-speed 350.0))
        (shot-timer (spawn-entity :player-shot-timer))
	;; ~20 shots/ second (at 60FPS)
        (timer-component (make-instance 'rolling-timer :seconds (/ 1.0 20)))
	(drawable (make-instance 'drawable
				 :parameters
				 (make-instance 'rect-drawing-parameters
						:width  15.0
						:height 15.0))))
    (attach-component e (vec2 x y) :position)
    (attach-component e v)
    (attach-component e drawable)
    (attach-component shot-timer timer-component)))

(let ((score 0))
  (defun reset-score (&optional (new-score 0))
    (setf score new-score))

  (defun inc-score (amount)
    (incf score amount))

  (defun get-score ()
    score))

(defun reset-game ()
  (setf *action-bag* nil)
  (drain-unused-entities)
  (reset-score)
  (spawn-player 400 400)
  (spawn-enemy  500 600 :entity-id :enemy) ;; this one has an explicit id to help debugging
  (spawn-enemy  200 100 :relative-pos (vec2 200 100))
  )

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
        do (let ((position (get-component entity :position))
		 (dp (parameters (get-component entity 'drawable))))
	     (render position dp))))

(defmethod gamekit:draw ((this the-game))
  (draw-entities)
  (draw-score))

(defun calculate-new-position (position velocity dt)
  (add position (step-velocity velocity dt)))

(defun update-positions (dt)
  "iterate over entities with linear-velicity component and update their
   position component accordingly to the provided delta time dt."
  (loop for entity in (entities-with-component 'linear-velocity)
        do (let ((vel      (get-component entity 'linear-velocity))
                 (position (get-component entity :position)))
             (attach-component entity
			    (calculate-new-position position vel dt)
			    :position))))

(defun action-active? (action)
  "takes an action symbol (for example :fire) and returns truty if the
   the action is active."
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
  (let ((pos (get-component :player :position))
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
	do (let ((position (get-component entity :position))
		 (boundary (get-component entity 'boundary)))
	     (when (not (inside-boundaries? position boundary))
	       (retire-entity entity)))))

(defun resolve-collisions ()
  (loop for bullet in (entities-with-component 'damage)
	do (let ((bullet-pos (get-component bullet :position))
		 (bullet-mask (bits (get-component bullet 'collision-mask)))
		 (bullet-radius (radius (get-component bullet 'bounding-circle))))
	     (loop named inner
		   for target in (entities-with-component 'health-points)
		   do (let ((target-pos
			      (get-component target :position))
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
	do (let ((lp (get-component entity 'linear-path)))
	     (attach-component entity (update-path lp dt) :position))))

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
