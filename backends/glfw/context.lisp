#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.glfw)

(utility:eval-always
  (defun foo (&optional (sym :start) (prefix "") (bar "%GLFW"))
    (multiple-value-bind (sym existsp)
	(find-symbol
	 (concatenate 'string "+" prefix (symbol-name sym) "+")
	 (find-package bar))
      (unless existsp
	(error "symbol not found"))
      sym)))

(defmacro cl-glfw3-keyword (keyword &optional (prefix ""))
  (foo keyword prefix))

(defvar *window-table* (tg:make-weak-hash-table :test 'eq :weakness :value))

(defclass context (trial:context)
  ((title :initarg :title :accessor title)
   (cursor-visible :initform T :accessor cursor-visible)
   (mouse-pos :initform (vec 0 0) :accessor mouse-pos)
   (initargs :initform NIL :accessor initargs)
   (window :initform NIL :accessor window))
  (:default-initargs
   :resizable T
   :visible T
   :decorated T
   :robustness (cl-glfw3-keyword :no-robustness)
   :forward-compat T
   :debug-context NIL))

(defmethod initialize-instance ((context context) &key)
  (call-next-method)
  (create-context context)
  )

(defmethod shared-initialize :after ((context context) slots
                                     &key (version NIL version-p)
                                          (profile NIL profile-p)
                                          (width NIL width-p)
                                          (height NIL height-p)
                                          (title NIL title-p)
                                          (double-buffering NIL double-buffering-p)
                                          (stereo-buffer NIL stereo-buffer-p)
                                          (vsync NIL vsync-p)
                                     ;; Extra options
                                          (resizable NIL resizable-p)
                                          (visible NIL visible-p)
                                          (decorated NIL decorated-p)
                                          (robustness NIL robustness-p)
                                          (forward-compat NIL forward-compat-p)
                                          (debug-context NIL debug-context-p))
  (flet (((setf g) (value name) (setf (getf (initargs context) name) value)))
    (macrolet ((maybe-set (var &optional (name (intern (string var) :keyword)))
                 `(when ,(let ((*print-case* (readtable-case *readtable*)))
                           (intern (format NIL "~a-~a" var 'p)))
                    (setf (g ,name) ,var))))
      (maybe-set width)
      (maybe-set height)
      (maybe-set double-buffering)
      (maybe-set stereo-buffer)
      (maybe-set resizable)
      (maybe-set visible)
      (maybe-set decorated)
      (maybe-set robustness :context-robustness)
      (maybe-set forward-compat :opengl-forward-compat)
      (maybe-set debug-context :opengl-debug-context)
      (when vsync-p
        (setf (g :refresh-rate)
              (ecase vsync (:off 0) (:on 1) (:adaptive -1))))
      (when version-p
        (setf (g :context-version-major) (first version))
        (setf (g :context-version-minor) (second version)))
      (when profile-p
        (setf (g :opengl-profile) (ecase profile
                                    ((NIL) (cl-glfw3-keyword :opengl-any-profile))
                                    (:core (cl-glfw3-keyword :opengl-core-profile))
                                    (:compatibility
				     (cl-glfw3-keyword :opengl-compat-profile))))))))

(defmethod create-context ((context context))
  (let ((initargs (initargs context)))
    (macrolet ((output-hints (&rest hints)
                 `(progn
                    ,@(loop for (name type attrib) in hints
                            collect `(%glfw:window-hint
                                      ,attrib
                                      (cffi:convert-to-foreign
                                       (getf initargs ,name) ,type))))))
      (output-hints
       (:resizable :boolean (cl-glfw3-keyword :resizable))
       (:visible :boolean (cl-glfw3-keyword :visible))
       (:decorated :boolean (cl-glfw3-keyword :decorated))
       (:refresh-rate :int (cl-glfw3-keyword :refresh-rate))
       (:stereo-buffer :boolean (cl-glfw3-keyword :stereo))
       (:context-version-major :int (cl-glfw3-keyword :context-version-major))
       (:context-version-minor :int (cl-glfw3-keyword :context-version-minor))
       (:context-robustness
	:int ;;FIXME
	;;'%glfw::robustness
	(cl-glfw3-keyword :context-robustness)
	)
       (:opengl-forward-compat :boolean (cl-glfw3-keyword :opengl-forward-compat))
       (:opengl-debug-context :boolean (cl-glfw3-keyword :opengl-debug-context))
       (:opengl-profile
	:int ;;FIXME
	;;'%glfw::opengl-profile
	(cl-glfw3-keyword :opengl-profile))
       ;; This option is not in %glfw for some reason.
       (:double-buffering :boolean (cl-glfw3-keyword :doublebuffer)))
      (v:info :trial.backend.glfw "Creating context ~a" context)
      (let ((window (%glfw:create-window (getf initargs :width)
                                         (getf initargs :height)
                                         (title context)
                                         (cffi:null-pointer)
                                         (if (shared-with context)
                                             (window (shared-with context))
                                             (cffi:null-pointer)))))
        (when (claw:wrapper-null-p window)
          (error "Error creating context."))
        (setf (gethash (cffi:pointer-address (claw:ptr window)) *window-table*) context)
        (setf (window context) window)
        (%glfw:make-context-current window)
        (%glfw:set-window-size-callback window (cffi:get-callback 'ctx-size))
        (%glfw:set-window-focus-callback window (cffi:get-callback 'ctx-focus))
        (%glfw:set-key-callback window (cffi:get-callback 'ctx-key))
        (%glfw:set-char-callback window (cffi:get-callback 'ctx-char))
        (%glfw:set-mouse-button-callback window (cffi:get-callback 'ctx-button))
        (%glfw:set-cursor-pos-callback window (cffi:get-callback 'ctx-pos))
        (%glfw:set-scroll-callback window (cffi:get-callback 'ctx-scroll))))))

(defmethod destroy-context ((context context))
  (%glfw:destroy-window (window context))
  (setf (window context) NIL))

(defmethod valid-p ((context context))
  (not (null (window context))))

(defmethod make-current ((context context))
  (%glfw:make-context-current (window context)))

(defmethod done-current ((context context))
  (%glfw:make-context-current (cffi:null-pointer)))

(defmethod hide ((context context))
  (%glfw:hide-window (window context)))

(defun get-window-size (window)
  (cffi:with-foreign-objects ((w :int)
			      (h :int))
    (%glfw:get-window-size window w h)
    (values (cffi:mem-ref w :int)
	    (cffi:mem-ref h :int))))

(defun set-window-monitor (monitor width height window &key
							 (x-position 0) (y-position 0)
							 (refresh-rate %glfw:+dont-care+))
  (let ((monitor (if (null monitor) (cffi:null-pointer) monitor)))
    (%glfw:set-window-monitor window monitor x-position y-position width height refresh-rate)))
(defmethod show ((context context) &key (fullscreen NIL f-p))
  (%glfw:show-window (window context))
  (when f-p
    (multiple-value-bind (w h) (get-window-size (window context))
      (set-window-monitor (when fullscreen (%glfw:get-primary-monitor))
			  w h (window context)))))

(defmethod resize ((context context) width height)
  (%glfw:set-window-size (window context) width height))

(defmethod quit ((context context))
  (%glfw:set-window-should-close (window context)
				 (if T
				     (cl-glfw3-keyword :true)
				     (cl-glfw3-keyword :false))))

(defmethod swap-buffers ((context context))
  (%glfw:swap-buffers (window context)))

(defmethod show-cursor ((context context))
  (%glfw:set-input-mode (window context)
			(cl-glfw3-keyword :cursor)
			(cl-glfw3-keyword :normal "CURSOR-"))
  (setf (cursor-visible context) T))

(defmethod hide-cursor ((context context))
  (%glfw:set-input-mode (window context)
			(cl-glfw3-keyword :cursor)
			(cl-glfw3-keyword :hidden "CURSOR-"))
  (setf (cursor-visible context) NIL))

(defmethod lock-cursor ((context context))
  (%glfw:set-input-mode (window context)
			(cl-glfw3-keyword :cursor)
			(cl-glfw3-keyword :disabled "CURSOR-")))

(defmethod unlock-cursor ((context context))
  (if (cursor-visible context)
      (show-cursor context)
      (hide-cursor context)))

(defmethod (setf title) :before (value (context context))
  (%glfw:set-window-title (window context) value))

(defmethod width ((context context))
  (values (get-window-size (window context))))

(defmethod height ((context context))
  (multiple-value-bind (w h) (get-window-size (window context))
    (declare (ignore w))
    h))

(defmethod profile ((context context))
  (utility:etouq
    `(ecase (%glfw:get-window-attrib (window context) (cl-glfw3-keyword :opengl-profile))
       (,(cl-glfw3-keyword :opengl-any-profile) NIL)
       (,(cl-glfw3-keyword :opengl-core-profile) :core)
       (,(cl-glfw3-keyword :opengl-compat-profile) :compatibility))))

(defmethod version ((context context))
  (list (%glfw:get-window-attrib (window context) (cl-glfw3-keyword :context-version-major))
        (%glfw:get-window-attrib (window context) (cl-glfw3-keyword :context-version-minor))))

(defun make-context (&optional handler &rest initargs)
  (apply #'make-instance 'context :handler handler initargs))

(defun coerce-from-glfw3-bool (bool)
  (cond ((eq (cl-glfw3-keyword :true) bool) t)
	((eq (cl-glfw3-keyword :false) bool) nil)
	(t (error "that's not a bool ~a" bool))))

(defun launch-with-context (&optional main &rest initargs)
  (flet ((body ()
           (glfw:with-init ()
             (let ((main (apply #'make-instance main initargs)))
               (start main)
               (unwind-protect
                    (loop with window = (window (trial:context main))
		       until (coerce-from-glfw3-bool (%glfw:window-should-close window))
                          do (%glfw:poll-events)
                             ;; Apparently bt:thread-yield is a no-op sometimes,
                             ;; making this loop consume the core. Sleep instead.
                             (sleep 0.001))
                 (finalize main))))))
    #+darwin
    (tmt:with-body-in-main-thread ()
      (body))
    #-darwin
    (body)))

(defmacro %with-context (&body body)
  `(let ((context (gethash (cffi:pointer-address window) *window-table*)))
     ,@body))

(glfw:define-framebuffer-size-callback ctx-size (window w h)
  (%with-context
    (handle (make-instance 'resize
                           :width w
                           :height h)
            (handler context))))

;;FIXME::add to bodge-glfw
(defmacro def-window-focus-callback (name (window focusedp) &body body)
  `(claw:defcallback ,name :void ((,window (:pointer %glfw:window))
                                  (,focusedp :int) ;;boolean
				  ;;See https://www.glfw.org/docs/latest/window_guide.html
				  )
     ,@body))

(def-window-focus-callback ctx-focus (window focusedp)
  (%with-context
    (handle (make-instance (if focusedp 'gain-focus 'lose-focus))
            (handler context))))

(glfw:define-key-callback ctx-key (window key scancode action modifiers)
  (declare (ignore scancode))
  (%with-context
    (case action
      (:press
       (v:debug :trial.input "Key pressed: ~a" key)
       (handle (make-instance 'key-press
                              :key (glfw-key->key key)
                              :modifiers modifiers)
               (handler context)))
      (:release
       (v:debug :trial.input "Key released: ~a" key)
       (handle (make-instance 'key-release
                              :key (glfw-key->key key)
                              :modifiers modifiers)
               (handler context))))))

(glfw:define-char-callback ctx-char (window char)
  (%with-context
    (handle (make-instance 'text-entered
                           :text (string char))
            (handler context))))

(glfw:define-mouse-button-callback ctx-button (window button action modifiers)
  (declare (ignore modifiers))
  (%with-context
    (case action
      (:press
       (v:debug :trial.input "Mouse pressed: ~a" (glfw-button->button button))
       (handle (make-instance 'mouse-press
                              :pos (mouse-pos context)
                              :button (glfw-button->button button))
               (handler context)))
      (:release
       (v:debug :trial.input "Mouse released: ~a" (glfw-button->button button))
       (handle (make-instance 'mouse-release
                              :pos (mouse-pos context)
                              :button (glfw-button->button button))
               (handler context))))))

(glfw:define-scroll-callback ctx-scroll (window x y)
  (%with-context
    (v:debug :trial.input "Mouse wheel: ~a ~a" x y)
    (handle (make-instance 'mouse-scroll
                           :pos (mouse-pos context)
                           :delta y)
            (handler context))))

(glfw:define-cursor-pos-callback ctx-pos (window x y)
  (%with-context
    (let ((current (vec x (- (multiple-value-bind (w h) (get-window-size (window context))
			       (declare (ignorable w))
			       h)
			     y))))
      (handle (make-instance 'mouse-move
                             :pos current
                             :old-pos (mouse-pos context))
              (handler context))
      (setf (mouse-pos context) current))))

(defun glfw-button->button (button)
  (let ((button (gethash button *mouse-button-hash*)))
    (case button
      ((:1 :left) :left)
      ((:2  :right) :right)
      (:3  :middle)
      (:4  :x1)
      (:5  :x2)
      (:6  :x3)
      (:7  :x4)
      (:8 :x5)
      (T button))))

(defun glfw-key->key (key)
  (let ((key (gethash key *keys-hash*)))
    (case key
      (:grave-accent :section)
      (T key))))


(utility:eval-always
  (defun find-syms ()
    (let ((acc))
      (let ((package (find-package "%GLFW")))
	(do-symbols (sym package)
	  (when (and (boundp sym)
		     (eq (symbol-package sym)
			 package))
	    (push sym acc))))
      acc))

  ;;FIXME::assumes %glfw naming conventions
  (defun prefix-type-p (sym string)
    (let ((prefix (concatenate 'string "+" string "-"))
	  (symbol-name (symbol-name sym)))
      (if (string=
	   (safe-subseq symbol-name 0 (length prefix))
	   prefix)
	  (safe-subseq symbol-name
		       (length prefix)
		       (1- (length symbol-name)))
	  nil)))

  (defun safe-subseq (seq start end)
    (let ((len (length seq)))
      (subseq seq
	      (min len start)
	      (min len end))))
  (defparameter *keys-hash* (make-hash-table :test 'eql))
  (defparameter *mouse-button-hash* (make-hash-table :test 'eql))
  (defun symstuff ()
    (let ((syms (find-syms)))
      (flet ((get-keys (name hash)
	       (mapcar (lambda (x)
			 (setf (gethash 
				(first x)
				hash)
			       (utility:keywordify (second x))))
		       (remove nil
			       (mapcar (lambda (n)
					 (list (symbol-value n)
					       (prefix-type-p n name)))
				       syms)
			       :key 'second))))
	(get-keys "KEY" *keys-hash*)
	(get-keys "MOUSE-BUTTON" *mouse-button-hash*))))
  (symstuff))
