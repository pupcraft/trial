(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0 0 0 0)))

(define-pool workbench
  :base 'trial)

(defparameter *skybox-path* (merge-pathnames "data/nissi-beach/"
					     (asdf:system-source-directory :trial)))

(define-asset (workbench skybox) image
  (flet ((foo (x)
	   (merge-pathnames (concatenate 'string x ".jpg") *skybox-path*)))
    (mapcar #'foo
	    (list "posx"
		  "negx"
		  "posy"
		  "negy"
		  "posz"
		  "negz")))
  :target :texture-cube-map
  :min-filter :linear)

(define-asset (workbench white) image
    #p"white.png")

(define-asset (workbench black) image
    #p"black.png")

(define-asset (workbench neutral-normal) image
    #p"neutral-normal.png")

;; Building
(define-asset (workbench building-b) mesh

  ;;#p"/home/linus/models/building_08/obj/Building08.obj"
  ;;#p"/home/imac/Documents/obj/knight/knight.obj"
  ;;#p"/home/imac/quicklisp/local-projects/trial/data/teapot.dae"
  ;;#p"/home/imac/quicklisp/local-projects/trial/data/teapot.vf"
  ;;#p"/home/imac/quicklisp/local-projects/trial/data/lamp.DAE"
  #P"/home/imac/Documents/obj/cat/cat.obj"
  :geometry-name "<dummy_root>"
  )

(define-asset (workbench building-a) mesh
  ;;#p"/home/linus/models/building_08/obj/Building08.obj"
  ;;#p"/home/imac/Documents/obj/knight/knight.obj"
  ;;#P"/home/imac/Documents/obj/cat/cat.obj"
  ;#p"/home/imac/quicklisp/local-projects/trial/data/teapot.vf"
  ;;#+nil
  ;;"/home/imac/quicklisp/local-projects/trial/data/lamp.DAE"
  #p"/home/imac/quicklisp/local-projects/trial/data/teapot.dae"
  :geometry-name "B_Set06_5_B"
  )

(define-asset (workbench building-a-albedo) image
  ;; #p"/home/linus/models/building_08/obj/B_Set06_5_A_A.png"
  #p"/home/imac/Documents/obj/knight/armor.jpg"
  :internal-fromat :srgb)

(define-asset (workbench building-b-albedo) image
  ;;#p"/home/linus/models/building_08/obj/B_Set06_5_B_A.jpg"
  #p"/home/imac/Documents/obj/knight/armor.jpg"
  :internal-fromat :srgb)

(define-asset (workbench building-a-specular) image
  ;;#p"/home/linus/models/building_08/obj/B_Set06_5_A_M.png"
  #p"/home/imac/Documents/obj/knight/armor.jpg"
  )

(define-asset (workbench building-a-normal) image
  ;;#p"/home/linus/models/building_08/obj/B_Set06_5_A_N.png"
  #p"/home/imac/Documents/obj/knight/armor.jpg"
  )

(define-asset (workbench building-a-roughness) image
  #p"/home/imac/Documents/obj/knight/armor.jpg"
    ;;#p"/home/linus/models/building_08/obj/B_Set06_5_A_R.png"
  )

;; Extra
(define-asset (workbench sphere) mesh
    (make-sphere 1))

(define-asset (workbench ground) mesh
    (update-vertices (lambda (v) (nv* (uv v) 10)) (make-cube '(1000 1000 10))))

(defparameter *scene-size* 800)

(define-shader-subject point-light (geometry-shaded located-entity)
  ((index :initarg :index :initform 0 :accessor index)
   (color :initarg :color :initform (vec 0 0 0) :accessor color)
   (attenuation :initarg :attenuation :initform '(0.07 0.017) :accessor attenuation)
   (direction :initarg :direction :accessor direction))
  (:default-initargs
   :direction (vec3-random -3 +3)
   :diffuse-map (asset 'workbench 'white)
   :specular-map (asset 'workbench 'black)
   :normal-map (asset 'workbench 'neutral-normal)
   :roughness-map (asset 'workbench 'black)
   :occlusion-map (asset 'workbench 'black)
   :vertex-array (asset 'workbench 'sphere)))

(define-handler (point-light tick) (ev dt tt)
  (let ((buffer (asset 'trial 'light-block))
        (i (index point-light)))
    (flet ((field (i field)
             (format NIL "LightBlock.lights[~d].~(~a~)" i field)))
      (setf (buffer-field buffer (field i 'type)) 2)
      (setf (buffer-field buffer (field i 'position)) (location point-light))
      (setf (buffer-field buffer (field i 'color)) (color point-light))
      (setf (buffer-field buffer (field i 'attenuation_linear)) (first (attenuation point-light)))
      (setf (buffer-field buffer (field i 'attenuation_quadratic)) (second (attenuation point-light))))
    (let ((dir (direction point-light))
          (loc (location point-light)))
      (setf (vx dir) (random (* 30 dt)))
      (setf (vy dir) (+ (vy dir) (random (* 2 dt))))
      (setf (vz dir) (+ (vz dir) (random (* 2 dt))))
      (flet ((wrap (x)
               (- (mod (+ x *scene-size*) (* 2 *scene-size*)) *scene-size*)))
        (vsetf loc
               (wrap (+ (vx loc) (* (vx dir) (sin (vy dir)) (cos (vz dir)))))
               (mod (+ (vy loc) (* (vx dir) (sin (vy dir)) (sin (vz dir)))) *scene-size*)
               (wrap (+ (vz loc) (* (vx dir) (cos (vy dir))))))))))

(defmethod paint ((point-light point-light) (pass shadow-map-pass)))

(define-shader-entity test (geometry-shaded located-entity scaled-entity)
  ())

(define-shader-pass deferred+shadow-pass (high-color-pass
                                          hdr-output-pass
                                          deferred-render-pass
                                          shadow-render-pass)
  ((occlusion-map :port-type input)))

(define-class-shader (deferred+shadow-pass :fragment-shader 5)
  (gl-source (asset 'trial 'light-block))
  "in vec2 tex_coord;

uniform sampler2D position_map;
uniform sampler2D normal_map;
uniform sampler2D albedo_map;
uniform sampler2D occlusion_map;

float lighting_strength = 1.0;
vec3 ambient_light = vec3(0.1);
void main(){
  vec3 position = texture(position_map, tex_coord).rgb;
  vec3 normal = texture(normal_map, tex_coord).rgb;
  vec3 light_direction = light_block.lights[0].position-position;
  float bias = shadow_bias(normal, light_direction);
  float shadow = shadow_factor(position, bias);
  lighting_strength = 1-(0.9 * shadow);
  ambient_light *= texture(occlusion_map, tex_coord).r;
}")

;; (print (list :location (location (unit :camera (scene (handler *context*))))
;;              :rotation (rotation (unit :camera (scene (handler *context*))))))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'editor-camera :LOCATION (VEC3 -178.21268 68.54084 121.44603)
                                         :ROTATION (VEC3 0.060004286 1.976846 0.0))
           scene)
    ;; (enter (make-instance 'skybox :texture (asset 'workbench 'skybox)) scene)
    (flet ((add (vert diff spec norm rough ao &rest initargs)
             (enter (apply #'make-instance 'test
                           :specular-map (asset 'workbench spec)
                           :diffuse-map (asset 'workbench diff)
                           :normal-map (asset 'workbench norm)
                           :roughness-map (asset 'workbench rough)
                           :occlusion-map (asset 'workbench ao)
                           :vertex-array (asset 'workbench vert)
                           initargs)
                    scene)))
      ;;;(print (inspect (asset 'workbench 'building-b)))
      (print "ASDFADFASDF")
      #+nil
      (add 'building-a
           'building-a-albedo
           'building-a-specular
           'building-a-normal
           'building-a-roughness
           'white
           :scaling (vec 100 100 100)
           :location (vec -400 0 0))
      
      (add 'building-b
           'building-b-albedo
           'building-a-roughness ;;'black
           'neutral-normal
           'building-a-roughness ;;'black
           'building-a-roughness ;;'white
           :scaling (vec 100 100 100)		 ;;(vec 100 100 100)
           :location (vec 50 0 100)	;;(vec -400 0 0)
	   ))
    #+nil
    (dotimes (i 3;(1- MAX-LIGHTS)
	      )
      (enter (make-instance 'point-light :index (1+ i)
                                         :location (vec3-random (- *scene-size*) *scene-size*)
                                         :color (vec3-random 500 700)
                                         :attenuation '(0.07 0.017))
             scene))
    (let* ((shadow (make-instance 'shadow-map-pass :projection-matrix (mortho -800 800 -800 800 1.0 2000)
                                                   :view-matrix (mlookat (vec 400 300 150) (vec 0 0 0) (vec 0 1 0))
                                                   :name :shadow-map-pass))
           (geometry (make-instance 'geometry-pass))
           (ssao (make-instance 'ssao-pass))
           (lighting (make-instance 'deferred+shadow-pass :shadow-map-pass shadow))
           (h-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
           (v-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
           (h-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
           (v-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
           (skybox (make-instance 'skybox-pass :texture (asset 'workbench 'skybox)))
           (tone-map (make-instance 'bloom-pass))
           (blend (make-instance 'blend-pass)))
      (connect (port geometry 'position) (port ssao 'position-map) scene)
      (connect (port geometry 'normal) (port ssao 'normal-map) scene)
      ;; (connect (port ssao 'occlusion) (port (make-instance 'copy-pass) 'previous-pass) scene)
      (connect (port shadow 'shadow) (port lighting 'shadow-map) scene)
      (connect (port geometry 'position) (port lighting 'position-map) scene)
      (connect (port geometry 'normal) (port lighting 'normal-map) scene)
      (connect (port geometry 'albedo) (port lighting 'albedo-map) scene)
      (connect (port geometry 'metal) (port lighting 'metal-map) scene)
      (connect (port ssao 'occlusion) (port h-blur2 'previous-pass) scene)
      (connect (port h-blur2 'color) (port v-blur2 'previous-pass) scene)
      (connect (port v-blur2 'color) (port lighting 'occlusion-map) scene)
      (connect (port lighting 'high-pass) (port h-blur 'previous-pass) scene)
      (connect (port h-blur 'color) (port v-blur 'previous-pass) scene)
      (connect (port v-blur 'color) (port tone-map 'high-pass) scene)
      (connect (port lighting 'color) (port tone-map 'previous-pass) scene)
      (connect (port skybox 'color) (port blend 'a-pass) scene)
      (connect (port tone-map 'color) (port blend 'b-pass) scene)))
  
  (defmethod change-scene :after ((workbench workbench) scene &key old)
    (declare (ignore old))
    (let ((buffer (asset 'trial 'light-block)))
      (flet ((field (i field)
               (format NIL "LightBlock.lights[~d].~(~a~)" i field)))
        (setf (buffer-field buffer (field 0 'type)) 1)
        (setf (buffer-field buffer (field 0 'direction)) (nv- (vec 400 300 150)))
        (setf (buffer-field buffer (field 0 'color)) ;(vec 0.9 0.85 0.6)
              (v* (vunit (vec 9 8 5)) 10)))
      (setf (buffer-field buffer "LightBlock.count") 1))
    )
  (maybe-reload-scene))

(defmethod update :after ((workbench workbench) tt dt)
  (let* ((buffer (asset 'trial 'light-block))
         (shadow (unit :shadow-map-pass (scene workbench)))
         (light (vec 400 400 300))
         (color (vunit (vec 9 7 5))))
    ;; (setf (buffer-field buffer "LightBlock.lights[0].type") 1)
    ;; (setf (buffer-field buffer "LightBlock.lights[0].color") color)
    ;; (setf (buffer-field buffer "LightBlock.lights[0].position") light)
    ;; (setf (buffer-field buffer "LightBlock.lights[0].direction") (v- light))
    ;; (setf (shadow-projection-matrix shadow) (mortho -800 800 -800 800 1.0 1500))
    ;; (setf (shadow-view-matrix shadow) (mlookat (vec 400 300 150) (vec 0 0 0) (vec 0 1 0)))
    ))
