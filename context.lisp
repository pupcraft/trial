#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *context* NIL)

(defclass context ()
  ((glformat :initform NIL :accessor glformat)
   (glcontext :initform NIL :reader glcontext)
   (current-thread :initform NIL :accessor current-thread)
   (waiting :initform 0 :accessor context-waiting)
   (lock :initform (bt:make-lock "Context lock") :reader context-lock)
   (wait-lock :initform (bt:make-lock "Context wait lock") :reader context-wait-lock)))

(defmethod initialize-instance :before ((context context) &key (accum NIL)
                                                               (alpha T)
                                                               (depth T)
                                                               (direct-rendering T)
                                                               (double-buffer T)
                                                               (overlay NIL)
                                                               (plane 0)
                                                               (profile :compatibility)
                                                               (rgba T)
                                                               (sample-buffers T)
                                                               (samples 2)
                                                               (stencil T)
                                                               (stereo NIL)
                                                               (swap-interval 0)
                                                               (version (list 3 0)))
  (let ((format (q+:make-qglformat)))
    (setf (q+:accum format) accum)
    (setf (q+:alpha format) alpha)
    (setf (q+:depth format) depth)
    (setf (q+:direct-rendering format) direct-rendering)
    (setf (q+:double-buffer format) double-buffer)
    (setf (q+:overlay format) overlay)
    (setf (q+:plane format) plane)
    (setf (q+:profile format) (ecase profile
                                ((NIL :none :no-profile) (q+:qglformat.no-profile))
                                ((:core) (q+:qglformat.core-profile))
                                (:compatibility (q+:qglformat.compatibility-profile))))
    (setf (q+:rgba format) rgba)
    (setf (q+:sample-buffers format) sample-buffers)
    (setf (q+:samples format) samples)
    (setf (q+:stencil format) stencil)
    (setf (q+:stereo format) stereo)
    (setf (q+:swap-interval format) swap-interval)
    (setf (q+:version format) (values (first version) (second version)))
    (setf (glformat context) format)))

(defmethod setup ((context context))
  (let ((glcontext (q+:context context)))
    (if (q+:is-valid glcontext)
        (v:info :trial.context "~a successfully created context." context)
        (error "Failed to create context."))
    (acquire-context context)))

(defmethod finalize :after ((context context))
  (finalize (glformat context)))

(defmethod acquire-context ((context context) &key force)
  (let ((current (current-thread context))
        (this (bt:current-thread)))
    (when (or force (not (eql this current)))
      (cond ((and force current)
             (v:warn :trial.context "~a stealing ~a from ~a." this context current))
            (current
             (v:info :trial.context "~a waiting to acquire ~a..." this context)
             (bt:with-lock-held ((context-wait-lock context))
               (incf (context-waiting context)))
             (bt:acquire-lock (context-lock context))
             (bt:with-lock-held ((context-wait-lock context))
               (decf (context-waiting context))))
            (T
             (bt:acquire-lock (context-lock context))))
      (v:info :trial.context "~a acquiring ~a." this context)
      (setf (current-thread context) this)
      (q+:make-current context))))

(defmethod release-context ((context context) &key reentrant)
  (let ((current (current-thread context))
        (this (bt:current-thread)))
    (when (and (eql this current)
               (or (not reentrant) (< 0 (context-waiting context)))
               (not *context*))
      (v:info :trial.context "~a releasing ~a." this context)
      (setf (current-thread context) NIL)
      (q+:done-current context)
      (bt:release-lock (context-lock context)))))

(defmacro with-context ((context &key force reentrant) &body body)
  (let ((cont (gensym "CONTEXT")))
    `(let ((,cont ,context))
       (acquire-context ,cont :force ,force)
       (unwind-protect
            (let ((*context* ,cont))
              ,@body)
         (release-context ,cont :reentrant ,reentrant)))))
