;;;; components.lisp
;; Trying again to make a shmup, this time with common lisp
;; and trivial-gamekit:
;; https://borodust.org/projects/trivial-gamekit/
;;
;; this source includes some basic components

(cl:in-package :sciuter)

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

(defparameter *base-circle* (make-instance 'circle-drawing-parameters
					   :fill-paint *yellow*
					   :radius 5.0))
(defclass drawable ()
  ((parameters :initarg  :parameters
	       :initform *base-circle*
	       :accessor parameters)))

(defclass bounding-circle ()
  ((radius :initarg  :radius
	   :initform 5.0
	   :accessor radius)))

(defun vec-length (v)
  (let ((vx (x v))
	(vy (y v)))
    (sqrt (+ (* vx vx) (* vy vy)))))

(defun circle-overlap (a-pos a-radius b-pos b-radius)
  (<= (vec-length (subt b-pos a-pos)) (+ a-radius b-radius)))

(defclass damage ()
  ((amount :initarg  :amount
	   :initform 1
	   :accessor amount)))

(defclass health-points ()
  ((amount :initarg  :amount
	   :initform 1
	   :accessor amount)))

(defclass collision-mask ()
  ((bits :initarg  :bits
	 :initform 0
	 :accessor bits)))

(defclass path-node ()
  ((pos       :initarg  :pos
	      :initform (vec2 0 0)
	      :reader   pos)
   (timestamp :initarg  :timestamp
	      :initform 0
	      :reader   timestamp)))

(defclass linear-path ()
  ((path-nodes   :initarg  :path-nodes
                 :initform (make-array 0 :element-type 'path-node)
                 :accessor path-nodes)
   (current-node :initarg  :current-node
		 :initform 0
		 :accessor current-node)
   (next-time    :initarg  :next-time
		 :initform 1
		 :accessor next-time)
   (current-time :initarg  :current-time
		 :initform 0.0
		 :accessor current-time)
   (relative-pos :initarg  :relative-pos
		 :initform (vec2 0 0)
		 :accessor relative-pos)
   (repeat       :initarg  :repeat
		 :initform nil
		 :accessor repeat)))

(defun make-linear-path (nodes &key (relative-pos nil) (repeat nil))
  (make-instance 'linear-path
		 :path-nodes nodes
		 :relative-pos (or relative-pos (vec2 0 0))
		 :repeat repeat
		 :next-time (timestamp (aref nodes 1))))

(defun reset-path (path)
  (with-slots (current-node current-time next-time path-nodes) path
    (setf current-node 0)
    (setf next-time (timestamp (aref path-nodes 1)))
    (setf current-time 0)))

(defun lerp-nodes (current-node next-node curr-time)
  "return the linear interpolation of the two nodes at the
   specified time; each node has a timestamp attribute and
   curr-time is excpected to be
  (timestamp current-node) <= curr-time <= (timestamp next-node)"
  (let ((curr-node-timestamp (timestamp current-node))
	(next-node-timestamp (timestamp next-node)))
    (gamekit:lerp (pos current-node) (pos next-node)
		  (/ (- curr-time curr-node-timestamp)
		     (- next-node-timestamp curr-node-timestamp)))))

(defmethod update-path ((path linear-path) dt)
  (with-slots (path-nodes current-node next-time current-time relative-pos repeat) path
    (let ((last-node-index (- (array-dimension path-nodes 0) 1)))
      (incf current-time dt)
      (when (> current-time next-time) ;; when its time to move to next node
	(if (< current-node last-node-index)
	    ;; and there is a next node, update the current node index
	    ;; and set next-time to the timesptamp of the next node
	    (progn
	      (incf current-node 1)
	      (when (< current-node last-node-index)
		(setf next-time (timestamp (aref path-nodes (1+ current-node))))))
	    (when repeat (reset-path path))))

      (gamekit:add relative-pos
		   (if (= current-node last-node-index)
		       (pos (aref path-nodes current-node))
		       (lerp-nodes (aref path-nodes current-node)
				   (aref path-nodes (1+ current-node))
				   current-time))))))

(defparameter *test-path-nodes*
  (make-array 5
	      :element-type 'path-node
	      :initial-contents
	      (list
	       (make-instance 'path-node
			      :pos (vec2 600 60)
			      :timestamp 0)
	       (make-instance 'path-node
			      :pos (vec2 600 220)
			      :timestamp 2)
	       (make-instance 'path-node
			      :pos (vec2 300 320)
			      :timestamp 4)
	       (make-instance 'path-node
			      :pos (vec2 100 120)
			      :timestamp 7)
	       (make-instance 'path-node
			      :pos (vec2 600 60)
			      :timestamp 10))))

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
(defun spawn-bullet (x y dir speed &optional
				     (drawable *bullet-drawable-component*)
				     (damage *bullet-damage*)
				     (collision-mask *bullet-collision-mask*)
				     (bounding-circle *bullet-bounding-circle*))
  (let ((e (spawn-entity))
        (v (make-instance 'linear-velocity :dir dir
                                           :linear-speed speed))
        (p (vec2 x y)))
    (attach-component e p :position)
    (attach-component e v)
    (attach-component e *screen-boundaries*)
    (attach-component e drawable)
    (attach-component e bounding-circle)
    (attach-component e damage)
    (attach-component e collision-mask)))

(defclass enemy-behavior ()
  ((shoting-timer :initarg  :shoting-timer
		  :accessor shoting-timer)))

(defun make-enemy-behavior (bullets-second)
  (let ((timer (make-instance 'rolling-timer :seconds (/ 1.0 bullets-second))))
    (make-instance 'enemy-behavior :shoting-timer timer)))

(defparameter *enemy-bullet-drawable-component*
  (make-instance 'drawable
		 :parameters
		 (make-instance 'circle-drawing-parameters
				:fill-paint *red*
				:stroke-paint *black*
				:radius 5.0)))

(defparameter *enemy-bullet-damage* (make-instance 'damage :amount 1))
(defparameter *enemy-bullet-collision-mask* (make-instance 'collision-mask
							   :bits #b0010))
(defparameter *enemy-bullet-bounding-circle*
  (make-instance 'bounding-circle
		 :radius 5.0))

(defun update-enemy-behavior (enemy behavior dt)
  (let ((timer (shoting-timer behavior)))
    (update-timer timer dt)
    (when (expired-timer? timer)
      (reset-timer timer)
      (let ((position (get-component enemy :position)))
	(spawn-bullet (x position) (y position)
		      (vec2 0 -1) 200
		      *enemy-bullet-drawable-component*
		      *enemy-bullet-damage*
		      *enemy-bullet-collision-mask*
		      *enemy-bullet-bounding-circle*)))))
