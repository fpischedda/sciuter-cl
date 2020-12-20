(in-package #:sciuter)

(defclass entity ()
  ((pos       :initarg :pos
	      :initform (gamekit:vec2 0.0 0.0)
	      :accessor pos)
   (color     :initarg :color
	      :initform (gamekit:vec4 1 1 1 1)
	      :accessor color)
   (radius    :initarg :radius
	      :initform 5)
   (dead      :initarg :dead
	      :initform nil
	      :accessor dead)
   (HP        :initarg :HP
	      :accessor hp
	      :initform 1)))
