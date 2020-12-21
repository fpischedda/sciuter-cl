;;;; sciuter.lisp
;; Trying again to make a shmup, this time with common lisp
;; and trivial-gamekit:
;; https://borodust.org/projects/trivial-gamekit/

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
                               (:y . :fire)))

(defvar *action-bag* nil)

(defun spawn-bullet (x y dir speed)
  (let ((e (spawn-entity))
        (v (make-instance 'linear-velocity :dir dir
                                           :linear-speed speed))
        (p (make-instance 'point :pos (vec2 x y))))
    (attach-component e p)
    (attach-component e v)
    (attach-component e *screen-boundaries*)))

(defun spawn-player (x y)
  (let ((e (spawn-entity :player))
        (v (make-instance 'linear-velocity :dir (vec2 0.0 0.0)
                                           :linear-speed 50.0))
        (p (make-instance 'point :pos (vec2 x y)))
        (shot-timer (spawn-entity :player-shot-timer))
        (timer-component (make-instance 'rolling-timer :seconds 0.32)))
    (attach-component e p)
    (attach-component e v)
    (attach-component shot-timer timer-component)))

(defun reset-game ()
  (setf *action-bag* nil)
  (drain-unused-entities)
  (spawn-player 400 400))

(defun bind-key-action (key action)
  (gamekit:bind-button key :pressed
                       (lambda ()
                         (push action *action-bag*)))
  (gamekit:bind-button key :released
                       (lambda ()
                         (alexandria:deletef *action-bag* action))))

(defmethod gamekit:post-initialize ((app the-game))
  (loop for binding in *key-action-binding*
        do (destructuring-bind (key . action) binding
                               (bind-key-action key action)))
  (reset-game))

(defun draw-warning ()
  (dotimes (i 4)
    (draw-text "W A R N I N G !!!"
               (vec2 (* *width* .35)
                     (- (* *height* .75) (* i 24)))
               :fill-color (nth i `(,*black*
                                    ,*red*
                                    ,*orange*
                                    ,*yellow*)))))

(defmethod draw (position)
  (gamekit:draw-circle position
                       20
                       :fill-paint *blue*
                       :stroke-paint *black*
                       :fill-paint (vec4 0 0 0 0)
                       :thickness 2))

(defun draw-entities ()
  (loop for position in (data-for-component 'point)
        do (draw (pos position))))

(defmethod gamekit:draw ((this the-game))
  (draw-warning)
  (draw-entities)
  )

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
      (spawn-bullet (x pos) (y pos) (vec2 0 1) 50)
      (reset-timer timer))))

(defun remove-out-of-boundaries ()
  "Remove all entities that have a 'boundary component and are
   outside of that boundariy limits."
  (loop for entity in (entities-with-component 'boundary)
	do (let ((position (pos (get-component entity 'point)))
		 (boundary (get-component entity 'boundary)))
	     (when (not (inside-boundaries? position boundary))
	       (retire-entity entity)))))

(defparameter *frame-ms* 0.16) ;; more or less 60FPS

(defmethod gamekit:act ((this the-game))
  (update-timers *frame-ms*)
  (update-player-direction)
  (update-positions *frame-ms*)
  (player-shot)
  (remove-out-of-boundaries)
  )

(defun run ()
  (gamekit:start 'the-game))

(defun stop ()
  (gamekit:stop))
