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
