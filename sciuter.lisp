;;;; sciuter.lisp
;; Trying again to make a shmup, this time with common lisp
;; and trivial-gamekit:
;; https://borodust.org/projects/trivial-gamekit/

(in-package #:sciuter)

(defparameter *width* 1024)
(defparameter *height* 800)

(let ((entity-id- 0))
  (defun gen-entity-id ()
    (incf entity-id- 1)))

;; hash table of all components associated to an entity
;; also keep track of live entities (hash-map-keys)
(defvar *entities* (make-hash-table :size 1024))

(defvar *inactive-entities* nil)

(defvar *components* (make-hash-table :size 1024))

;; for each component type (identified by a keyword) keep a registry of
;; entities associated to it and the component data
;; (i.e components => component type => entities => entity => data)
;; also associate a component to an entity; for each entity there is a
;; hash map of component-type => component data
(defun attach-component (entity component)
  (let* ((component-type (type-of component))
         (entity-slot (gethash entity *entities*)))
    (setf (gethash component-type entity-slot) component)
    (multiple-value-bind (component-slot present)
        (gethash component-type *components* (make-hash-table :size 256))
      (setf (gethash entity component-slot) component)
      (when (not present)
        (setf (gethash component-type *components*) component-slot)))))

;; remove the entity from the component registry and the component
;; from the entity registry
(defun detach-component (entity component)
  (let* ((component-type (type-of component))
         (component-slot (gethash component-type *components*))
         (entity-slot (gethash entity *entities*)))
    (when component-slot (remhash entity component-slot))
    (when entity-slot (remhash component-type entity-slot))))

;; given a component type return all entities with that component
;; attached attached to them
(defun entities-with-component (component-type)
  (alexandria:if-let ((component-slot (gethash component-type *components*)))
    (alexandria:hash-table-keys component-slot)
    '()))

;; given a component type return all components
;; attached attached to some entities
(defun data-for-component (component-type)
  (alexandria:if-let ((component-slot (gethash component-type *components*)))
    (alexandria:hash-table-values component-slot)
    '()))

(defun components-of-entity (entity)
  (alexandria:hash-table-values (gethash entity *entities*)))

(defun get-component (entity component-type)
  (gethash component-type (gethash entity *entities*)))

;; example components

(defclass point ()
  ((pos :initarg :pos
        :initform (gamekit:vec2 0.0 0.0)
        :accessor pos)))

(defclass linear-velocity ()
  ((dir           :initarg :dir
                  :initform (gamekit:vec2 0.0 0.0)
                  :accessor dir)
   (linear-speed  :initarg :linear-speed
                  :initform 0.0
                  :accessor linear-speed)
   ))

(defmethod step-velocity ((v linear-velocity) dt)
  (gamekit:mult (dir v) (* (linear-speed v) dt)))

(defclass rolling-timer ()
  ((seconds :initarg :seconds
            :initform 0.0
            :accessor seconds)
   (counter :initform 0.0
            :accessor counter)))

(defmethod reset-timer ((timer rolling-timer))
  (with-slots (seconds counter) timer
    (setf counter seconds)))

(defmethod update-timer ((timer rolling-timer) dt)
  (decf (counter timer) dt))

(defmethod expired-timer? ((timer rolling-timer))
  (<= (counter timer) 0))

(defun update-timers (dt)
  (loop for timer in (data-for-component 'rolling-timer)
        do (update-timer timer dt)))

(defclass boundary ()
  ((limits :initarg  :limits
           :initform '(0 0 0 0)
           :accessor limits)))

(defparameter *screen-boundaries*
  (make-instance 'boundary :limits '(0 0 1024.0 800.0)))

(defmethod inside-boundaries? (pos (boundaries boundary))
  (destructuring-bind (top left bottom right) (limits boundaries)
    (let ((x (x pos))
	  (y (y pos)))
      (and (>= x left) (<= x right)
                 (>= y top)  (<= y bottom)))))

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

(defun spawn-entity (&optional id)
  (let ((eid (or id
                 (pop *inactive-entities*)
                 (gen-entity-id))))
    (setf (gethash eid *entities*) (make-hash-table :size 16))
    eid))

(defun retire-entity (e)
  (loop for component being the hash-values of (gethash e *entities*)
        do (detach-component e component))
  (push e *inactive-entities*)
  (remhash e *entities*))

(defun drain-unused-entities (entities)
  (loop for entity being the hash-keys of entities
        do (retire-entity entity)))

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
  (drain-unused-entities *entities*)
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
