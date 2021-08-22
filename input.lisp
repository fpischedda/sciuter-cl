;;;; input.lisp
;; General input handling functions

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

(let (action-bag nil)

  (defun reset-input ()
    (setf action-bag nil))

  (defun action-active? (action)
    "takes an action symbol (for example :fire) and returns truty if the
     the action is active."
    (member action action-bag))

  (defun bind-key-action (key action)
    "set the input handler to push/remove the right action from/to
     the action bag"
    (gamekit:bind-button key :pressed
			 (lambda ()
                           (push action action-bag)))
    (gamekit:bind-button key :released
			 (lambda ()
                           (alexandria:deletef action-bag action))))

  (defun init-key-bindings ()
    "each time a key is pressed/release then thend the corresponding
     acction (if any) must be addedde/removed to/from the action bag"
    (loop for binding in *key-action-binding*
          do (destructuring-bind (key . action) binding
               (bind-key-action key action)))))
