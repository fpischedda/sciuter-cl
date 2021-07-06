(in-package :sciuter)

(defun draw-entities ()
  "iterate over entities with drawable and position components
   and render them to the screen."
  (loop for entity in (entities-with-component 'drawable)
        do (let ((position (get-component entity :position))
		 (dp (parameters (get-component entity 'drawable))))
	     (render position dp))))

(defun update-positions (dt)
  "iterate over entities with linear-velicity component and update their
   position component accordingly to the provided delta time dt."
  (loop for entity in (entities-with-component 'linear-velocity)
        do (let ((vel      (get-component entity 'linear-velocity))
                 (position (get-component entity :position)))
             (attach-component entity
			    (calculate-new-position position vel dt)
			    :position))))

(defun remove-out-of-boundaries ()
  "Remove all entities that have a 'boundary component and are
   outside of that boundary limits."
  (loop for entity in (entities-with-component 'boundary)
	do (let ((position (get-component entity :position))
		 (boundary (get-component entity 'boundary)))
	     (when (not (inside-boundaries? position boundary))
	       (retire-entity entity)))))

(defun resolve-collisions ()
  (loop for bullet in (entities-with-component :damage)
	do (let ((bullet-pos (get-component bullet :position))
		 (bullet-mask (bits (get-component bullet 'collision-mask)))
		 (bullet-radius (get-component bullet :bounding-circle)))
	     (loop named inner
		   for target in (entities-with-component :health-points)
		   do (let ((target-pos
			      (get-component target :position))
			    (target-mask
			      (bits (get-component target 'collision-mask)))
			    (target-radius
			      (get-component target :bounding-circle)))
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

(defun draw-debug ()
  "iterate over entities with :bounding-circle and position components
   and render them to the screen as circles as big as the bounding-circle is"
  (loop for entity in (entities-with-component :bounding-circle)
        do (let ((position (get-component entity :position))
		 (radius (get-component entity :bounding-circle)))
	     (gamekit:draw-circle position
				  radius
				  :stroke-paint *black*
				  :thickness 2.0))))
