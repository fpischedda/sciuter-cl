;;;; sciuter.lisp
;; Trying again to make a shmup, this time with common lisp
;; and trivial-gamekit:
;; https://borodust.org/projects/trivial-gamekit/
;;
;; this file contains the implementation of logic and rendering of the game
;; built on top of entity.lisp, components.lisp and (eventually) system.lisp

(in-package #:sciuter)

(defparameter *width* 512)
(defparameter *height* 512)

(gamekit:register-resource-package :keyword
                                   (asdf:system-relative-pathname :sciuter "assets/"))


(gamekit:define-image :boss-image "boss.png")
;; (gamekit:define-sound :snake-grab "snake-grab.ogg")

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
			       (:p . :pause)
			       (:q . :quit)))

(defvar *action-bag* nil)

(defun bind-key-action (key action)
  (gamekit:bind-button key :pressed
                       (lambda ()
                         (push action *action-bag*)))
  (gamekit:bind-button key :released
                       (lambda ()
                         (alexandria:deletef *action-bag* action))))

(defparameter *enemy-drawable-component-circle*
  (make-instance 'drawable
		 :parameters
		 (make-instance 'circle-drawing-parameters
				:radius 60.0
				:fill-paint *green*
				:stroke-paint *yellow*)))

(defun enemy-drawable-component-image ()
  (make-instance 'drawable
		 :parameters
		 (make-image-drawing-parameters	:boss-image)))

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
	       (make-path-node (vec2 50 60) 0)
	       (make-path-node (vec2 500 420) 2)
	       (make-path-node (vec2 200 180) 4)
	       (make-path-node (vec2 10 320) 7)
	       (make-path-node (vec2 50 60) 10))))

(defparameter *enemy-path-nodes-other*
  (make-array 5
	      :element-type 'path-node
	      :initial-contents
	      (list
	       (make-path-node (vec2 350 10) 0)
	       (make-path-node (vec2 50 420) 2)
	       (make-path-node (vec2 50 100) 5)
	       (make-path-node (vec2 200 220) 6)
	       (make-path-node (vec2 350 10) 9))))

(defun make-enemy-path (path-nodes relative-pos start-time)
  (make-linear-path path-nodes
		    :relative-pos relative-pos
		    :repeat t
		    :start-time t))

(defun spawn-enemy (x y &key entity-id
			  relative-pos
			  (path-nodes *enemy-path-nodes*)
			  (drawable *enemy-drawable-component-circle*)
			  (bullets-second 50)
			  (start-time 0.0))
  "create a enemy entity, with all the needed components;
   return the newly created entity."
  (let ((e (spawn-entity entity-id))
	(eb (make-enemy-behavior bullets-second)))
    (attach-component e (vec2 x y) :position)
    (attach-component e drawable)
    (attach-component e 30.0 :bounding-circle)
    (attach-component e 10 :health-points)
    (attach-component e *enemy-collision-mask*)
    (attach-component e (make-enemy-path path-nodes relative-pos start-time))
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
  (spawn-enemy  200 100
		:path-nodes *enemy-path-nodes-other*
		:drawable (enemy-drawable-component-image))
  )

;; (attach-component :enemy *enemy-drawable-component-image*)

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

(defun calculate-new-position (position velocity dt)
  (add position (step-velocity velocity dt)))

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

(defparameter *fixed-dt* (/ 1.0 60.0)) ;; ~60FPS

(let ((paused nil))
  (defun pause ()
    (setf paused t))

  (defun unpause ()
    (setf paused nil))

  (defun paused? () paused)

  (defun toggle-pause ()
    (setf paused (not (paused?)))))

(defmethod gamekit:draw ((this the-game))
  (draw-entities)
  (draw-score)
  (draw-debug))

(defmethod gamekit:act ((this the-game))
  (when (not (paused?))
    (update-timers *fixed-dt*)
    (update-player-direction)
    (update-positions *fixed-dt*)
    (update-entities-with-path *fixed-dt*)
    (player-shot)
    (remove-out-of-boundaries)
    (resolve-collisions)
    (when (action-active? :pause)
      (toggle-pause))
    (when (action-active? :quit)
      (stop))))

(defun run ()
  (gamekit:start 'the-game)
  (gamekit:bind-any-gamepad
   (lambda (gamepad state)
     (if (eq :connected state)
	 (format t "gamepad connected ~a" gamepad)
	 (format t "gamepad disconnected ~a" gamepad)))))

(defun stop ()
  (gamekit:stop))
