#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-asset (trial fullscreen-square) packed-vertex-array
    (#(0 1 2 2 3 0)
     3 #(+1.0 +1.0 +0.0
         +1.0 -1.0 +0.0
         -1.0 -1.0 +0.0
         -1.0 +1.0 +0.0)
     2 #(1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0)))

;; FIXME: Allow specifying which attributes to include
;; FIXME: Generate normals
;; FIXME: Add shapes:
;;        - Box
;;        - Torus
;;        - Tube
;;        - Cone
;;        - Arrow

(defun make-rectangle (w h &key (align :center) mesh pack (x 0) (y 0) (z 0))
  (let (l r u b)
    (ecase align
      (:center (setf l (- (/ w 2)) r (+ (/ w 2))
                     u (- (/ h 2)) b (+ (/ h 2))))
      (:topleft (setf l 0 r w
                      u 0 b h))
      (:bottomleft (setf l 0 r w
                         u (- h) b 0)))
    (incf l x) (incf r x) (incf u y) (incf b y)
    (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'textured-vertex)) :pack pack)
      (vertex :position (vec r u z) :uv (vec 1.0 1.0))
      (vertex :position (vec r b z) :uv (vec 1.0 0.0))
      (vertex :position (vec l b z) :uv (vec 0.0 0.0))
      (vertex :position (vec l b z) :uv (vec 0.0 0.0))
      (vertex :position (vec l u z) :uv (vec 0.0 1.0))
      (vertex :position (vec r u z) :uv (vec 1.0 1.0)))))

(defun make-cube (size &key mesh pack (x 0) (y 0) (z 0))
  (let ((s (/ size 2)))
    (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'textured-vertex)) :pack pack)
      (vertex :position (vec (+ x s) (+ y s) (- z s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- x s) (+ y s) (- z s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- x s) (+ y s) (+ z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- x s) (+ y s) (+ z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ x s) (+ y s) (+ z s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ x s) (+ y s) (- z s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ x s) (- y s) (+ z s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- x s) (- y s) (+ z s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- x s) (- y s) (- z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- x s) (- y s) (- z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ x s) (- y s) (- z s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ x s) (- y s) (+ z s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ x s) (+ y s) (+ z s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- x s) (+ y s) (+ z s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- x s) (- y s) (+ z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- x s) (- y s) (+ z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ x s) (- y s) (+ z s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ x s) (+ y s) (+ z s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ x s) (- y s) (- z s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- x s) (- y s) (- z s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- x s) (+ y s) (- z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- x s) (+ y s) (- z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ x s) (+ y s) (- z s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ x s) (- y s) (- z s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (- x s) (+ y s) (+ z s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (- x s) (+ y s) (- z s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (- x s) (- y s) (- z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- x s) (- y s) (- z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (- x s) (- y s) (+ z s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (- x s) (+ y s) (+ z s)) :uv (vec 1.0 1.0))

      (vertex :position (vec (+ x s) (+ y s) (- z s)) :uv (vec 1.0 1.0))
      (vertex :position (vec (+ x s) (+ y s) (+ z s)) :uv (vec 0.0 1.0))
      (vertex :position (vec (+ x s) (- y s) (+ z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ x s) (- y s) (+ z s)) :uv (vec 0.0 0.0))
      (vertex :position (vec (+ x s) (- y s) (- z s)) :uv (vec 1.0 0.0))
      (vertex :position (vec (+ x s) (+ y s) (- z s)) :uv (vec 1.0 1.0)))))

(defun make-quad-grid (size x-count y-count &key mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (loop repeat x-count
          for _x from (+ x (- (* size (/ x-count 2)))) by size
          do (loop repeat y-count
                   for _y from (+ y (- (* size (/ y-count 2)))) by size
                   do (vertex :position (vec (+ _x) (+ _y) z))
                      (vertex :position (vec (+ _x) (- _y) z))
                      (vertex :position (vec (- _x) (- _y) z))
                      (vertex :position (vec (- _x) (- _y) z))
                      (vertex :position (vec (- _x) (+ _y) z))
                      (vertex :position (vec (+ _x) (+ _y) z))))))

(defun make-line-grid (size w h &key mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex :face-length 2)) :pack pack)
    (let ((w (/ w 2)) (h (/ h 2))
          (ws (/ w size)) (hs (/ h size)))
      (loop for _x from (- w) to w by ws
            do (vertex :position (vec (+ x _x) y (- z h)))
               (vertex :position (vec (+ x _x) y (+ z h))))
      (loop for _z from (- h) to h by hs
            do (vertex :position (vec (- x w) y (+ z _z)))
               (vertex :position (vec (+ x w) y (+ z _z)))))))

(defun make-sphere (size &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  (let ((lat segments) (lng segments))
    (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
      (loop for i from lat downto 1
            for lat0 = (* PI (- (/ (1- i) lat) 0.5))
            for lat1 = (* PI (- (/ i lat) 0.5))
            for z0 = (sin lat0)
            for zr0 = (cos lat0)
            for z1 = (sin lat1)
            for zr1 = (cos lat1)
            do (loop for j from lng downto 0 by 2
                     for l1 = (* 2 PI (/ (- j 1) lng)) for l2 = (* 2 PI (/ (- j 2) lng))
                     for x1 = (cos l1) for x2 = (cos l2)
                     for y1 = (sin l1) for y2 = (sin l2)
                     do (vertex :position (vec (+ x (* x1 zr0 size)) (+ y (* y1 zr0 size)) (+ z (* z0 size))))
                        (vertex :position (vec (+ x (* x1 zr1 size)) (+ y (* y1 zr1 size)) (+ z (* z1 size))))
                        (vertex :position (vec (+ x (* x2 zr0 size)) (+ y (* y2 zr0 size)) (+ z (* z0 size))))
                     
                        (vertex :position (vec (+ x (* x2 zr0 size)) (+ y (* y2 zr0 size)) (+ z (* z0 size))))
                        (vertex :position (vec (+ x (* x1 zr1 size)) (+ y (* y1 zr1 size)) (+ z (* z1 size))))
                        (vertex :position (vec (+ x (* x2 zr1 size)) (+ y (* y2 zr1 size)) (+ z (* z1 size)))))))))

(defun make-disc (size &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (loop for i from 0 to (* 2 PI) by (/ (* 2 PI) segments)
          do (vertex :position (vec x y z))
             (vertex :position (vec (+ x (* size (cos (1+ i)))) (+ y (* size (sin (1+ i)))) z))
             (vertex :position (vec (+ x (* size (cos      i))) (+ y (* size (sin      i))) z)))))

(defun make-cylinder (size height &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'vertex)) :pack pack)
    (loop for i from 0 to (* 2 PI) by (/ (* 2 PI) segments)
          for e1b = (vec (+ x (* size (cos      i))) (+ y (* size (sin      i))) z)
          for e2b = (vec (+ x (* size (cos (1+ i)))) (+ y (* size (sin (1+ i)))) z)
          for e1t = (nv+ (vec 0 height 0) e1b)
          for e2t = (nv+ (vec 0 height 0) e2b)
          do ;; Bottom disc
             (vertex :position (vec x y z))
             (vertex :position e1b)
             (vertex :position e2b)
             ;; Top Disc
             (vertex :position (vec x (+ y height) z))
             (vertex :position e2t)
             (vertex :position e1t)
             ;; Wall
             (vertex :position e2b)
             (vertex :position e1b)
             (vertex :position e1t)
             (vertex :position e1t)
             (vertex :position e2t)
             (vertex :position e2b))))

(defun make-torus (size thickness &key mesh pack (x 0) (y 0) (z 0))
  )

(defun make-tube (size height inner-size &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  )

(defun make-cone (size height &key (segments 32) mesh pack (x 0) (y 0) (z 0))
  )
