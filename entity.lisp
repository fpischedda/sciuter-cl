(cl:in-package :sciuter)

(let ((entity-id- 0))
  (defun gen-entity-id ()
    (incf entity-id- 1))

  (defun reset-entity-ids ()
    (setf entity-id- 0)))

;; hash table of all components associated to an entity
;; also keep track of live entities (hash-map-keys)
(defvar *entities* (make-hash-table :size 1024))

(defvar *inactive-entities* nil)

(defvar *components* (make-hash-table :size 1024))

(defun attach-component (entity component &optional component-type)
  "for each component type, identified by component type if specified or
   (type-of component) keep a registry of entities associated to it and
   the component data
   (i.e components => component type => entities => entity => data)
   also associate a component to an entity; for each entity there is a
   hash map of component-type => component data."
  (let* ((component-type (or component-type (type-of component)))
         (entity-slot (gethash entity *entities*)))
    (setf (gethash component-type entity-slot) component)
    (multiple-value-bind (component-slot present)
        (gethash component-type *components* (make-hash-table :size 256))
      (setf (gethash entity component-slot) component)
      (when (not present)
        (setf (gethash component-type *components*) component-slot)))))

(defun detach-component (entity component &optional component-type)
  "remove the entity from the component registry and the component
   from the entity registry"
  (let* ((component-type (or component-type (type-of component)))
         (component-slot (gethash component-type *components*))
         (entity-slot (gethash entity *entities*)))
    (when component-slot (remhash entity component-slot))
    (when entity-slot (remhash component-type entity-slot))))

(defun entities-with-component (component-type)
  "given a component type return all entities with that component
   attached attached to them"
  (alexandria:if-let ((component-slot (gethash component-type *components*)))
    (alexandria:hash-table-keys component-slot)
    '()))

(defun data-for-component (component-type)
  "given a component type return all components
   attached attached to some entities"
  (alexandria:if-let ((component-slot (gethash component-type *components*)))
    (alexandria:hash-table-values component-slot)
    '()))

(defun components-of-entity (entity)
  (alexandria:hash-table-values (gethash entity *entities*)))

(defun get-component (entity component-type)
  (gethash component-type (gethash entity *entities*)))

(defun spawn-entity (&optional id)
  (let ((eid (or id
                 (pop *inactive-entities*)
                 (gen-entity-id))))
    (setf (gethash eid *entities*) (make-hash-table :size 16))
    eid))

(defun retire-entity (e)
  (maphash (lambda (type component)
	     (detach-component e component type))
	   (gethash e *entities*))
  (push e *inactive-entities*)
  (remhash e *entities*))

(defun drain-unused-entities ()
  (loop for entity being the hash-keys of *entities*
        do (retire-entity entity)))
