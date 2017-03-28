(in-package #:trial)
(in-readtable :qtools)

(define-pool workbench
  :base 'trial)

(define-asset (workbench av) texture-asset
    (#p"~/av.png"))

(define-asset (workbench cube) packed-vao-asset
    (#(0 1 2 2 3 0
       0 1 5 5 4 0
       2 3 7 7 6 2
       4 5 6 6 7 4)
     3 #(+0.5 +0.5 +0.5
         +0.5 -0.5 +0.5
         -0.5 -0.5 +0.5
         -0.5 +0.5 +0.5
         +0.5 +0.5 -0.5
         +0.5 -0.5 -0.5
         -0.5 -0.5 -0.5
         -0.5 +0.5 -0.5)
     2 #(1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0
         1.0 1.0
         1.0 0.0
         0.0 0.0
         0.0 1.0)))

(define-shader-pass blur-shader (post-effect-pass)
  ("previous"))

(define-class-shader blur-shader :fragment-shader
  "
in vec2 texCoord;
out vec4 outColor;
uniform sampler2D previous;
const float blurSizeH = 1.0 / 300.0;
const float blurSizeV = 1.0 / 200.0;

void main(){
  vec4 sum = vec4(0.0);
  for (int x = -4; x <= 4; x++){
    for (int y = -4; y <= 4; y++){
      sum += texture(previous,
                     vec2(texCoord.x + x * blurSizeH, texCoord.y + y * blurSizeV)) / 81.0;
    }
  }
  outColor = sum;
}")

(define-shader-subject testcube (vertex-subject textured-subject)
  ()
  (:default-initargs
   :texture (asset 'workbench 'av)
   :vertex-array (asset 'workbench 'cube)))

(defvar *pipeline* (make-instance 'pipeline))

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    (enter (make-instance 'testcube) scene)
    (load scene)
    (clear *pipeline*)
    (connect-pass (make-instance 'per-object-pass)
                  (make-instance 'blur-shader)
                  "previous" *pipeline*)
    (pack-pipeline *pipeline* main)
    (load *pipeline*)))

(defmethod paint ((source main) (target main))
  (let ((scene (scene source)))
    (gl:viewport 0 0 (width target) (height target))
    (issue scene 'tick)
    (process scene)
    (reset-matrix (projection-matrix))
    (reset-matrix (view-matrix))
    (perspective-projection 45 (/ (width target) (height target)) 0.1 100)
    (translate-by 0 0 -3 (view-matrix))
    (rotate +vx+ 0.03)
    (rotate +vy+ 0.05)
    (rotate +vz+ 0.07)
    (paint *pipeline* target)))
