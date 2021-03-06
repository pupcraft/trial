#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass mesh (gl-asset vertex-array)
  ((geometry-name :initarg :geometry-name :accessor geometry-name)
   (data-usage :initarg :data-usage :accessor data-usage))
  (:default-initargs
   :bindings NIL
   :geometry-name NIL
   :data-usage :static-draw))

(defmethod destructor ((mesh mesh))
  (let ((prev (call-next-method))
        (bindings (bindings mesh)))
    (lambda ()
      (funcall prev)
      (loop for (buffer) in bindings
            do (when (allocated-p buffer) (deallocate buffer))
               (maybe-free-static-vector (buffer-data buffer))))))

(defmethod load ((mesh mesh))
  (let* ((input (coerce-asset-input mesh T))
         (input (typecase input
                  (geometry
		   (let ((hash (meshes input)))
		     (if (= 1 (hash-table-count hash))
			 (first (alexandria:hash-table-values hash))
			 (gethash (geometry-name mesh) hash))))
                  (pathname
		   (let ((hash (meshes (read-geometry input T))))
		     (if (= 1 (hash-table-count hash))
			 (first (alexandria:hash-table-values hash))
			 (gethash (geometry-name mesh) hash))))
                  (T input)))
         (vao (etypecase input
                (vertex-mesh (change-class (copy-instance input) 'vertex-array))
                (vertex-array input))))
    (setf (bindings mesh) (mapcar #'enlist (bindings vao)))
    (setf (size mesh) (size vao))
    (loop for (buffer) in (bindings mesh)
          do (setf (data-usage buffer) (data-usage mesh))
             (allocate buffer))
    (allocate mesh)
    (case (data-usage mesh)
      (:static-draw
       ;; Free buffers again to avoid leaving garbage around.
       ;; We can do this safely because we know the data is fresh
       ;; and won't be used in the future due to static-draw.
       (loop for (buffer) in (bindings mesh)
             do (deallocate buffer)
                (maybe-free-static-vector (buffer-data buffer)))))
    (setf (bindings mesh) NIL)))
