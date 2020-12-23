;;;; components.lisp
;; Trying again to make a shmup, this time with common lisp
;; and trivial-gamekit:
;; https://borodust.org/projects/trivial-gamekit/
;;
;; The idea is to provide a general purpose `drawable' compoment
;; which will hold a `drawing-parameter' class that should cointain
;; all the parameters needed to draw something (for ex. shapes, sprites,
;; text and so on). For each class there should be a specialized function
;; that will handle the rendering of the entity.
;;
;; This source will contain some example parameters and methods for basic
;; shapes.

(cl:in-package :sciuter)

;; example drawing parameters

(defclass circle-drawing-parameters ()
  ((radius       :initarg  :radius
                 :initform 5.0
                 :accessor radius)
   (fill-paint   :initarg  :fill-paint
                 :initform *red*
                 :accessor fill-paint)
   (stroke-paint :initarg  :stroke-paint
                 :initform *blue*
                 :accessor stroke-paint)
   (thikness     :initarg  :thikness
                 :initform 2.0
                 :accessor thikness)))

(defmethod render (position (dp circle-drawing-parameters))
  (with-slots (radius fill-paint stroke-paint thikness) dp
    (gamekit:draw-circle position
                         radius
                         :fill-paint fill-paint
                         :stroke-paint stroke-paint
                         :thickness thikness)))

(defclass rect-drawing-parameters ()
  ((width        :initarg  :width
                 :initform 5.0
                 :accessor width)
   (height       :initarg  :height
                 :initform 5.0
                 :accessor height)
   (fill-paint   :initarg  :fill-paint
                 :initform *red*
                 :accessor fill-paint)
   (stroke-paint :initarg  :stroke-paint
                 :initform *blue*
                 :accessor stroke-paint)
   (thikness     :initarg  :thikness
                 :initform 2.0
                 :accessor thikness)))

(defmethod render (position (dp rect-drawing-parameters))
  (with-slots (width height fill-paint stroke-paint thikness) dp
    (gamekit:draw-rect position
                       width
		       height
                       :fill-paint fill-paint
                       :stroke-paint stroke-paint
                       :thickness thikness)))
