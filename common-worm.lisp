(in-package :common-worm)

(defclass worm ()
  ((head-x :initarg :x :initform (/ *screen-width* 2) :accessor head-x)
   (head-y :initarg :y :initform (/ *screen-width* 2) :accessor head-y)
   (max-length :initform 100 :accessor max-length)
   (horiz-dir :initform 0 :accessor horiz-dir)
   (vert-dir :initform -1 :accessor vert-dir)
   (color :initform sdl:*green* :initarg :color :accessor color)
   (body :initform nil :accessor body)
   (crashed :initform nil :accessor crashed-p)))

(defmethod crashed-p :around ((worm worm))
  (with-accessors ((x head-x)
		   (y head-y))
      worm
    (or (call-next-method)
	(<= x 0)
	(>= x (1- *screen-width*))
	(<= y 0)
	(>= y (1- *screen-height*)))))

(defmethod move ((worm worm))
  (with-accessors ((x head-x) (y head-y)) worm
    (incf x (horiz-dir worm))
    (incf y (vert-dir worm))
    (when (member (cons x y) (body worm) :test #'equal)
      (setf (crashed-p worm) t))
    (setf (body worm) (append (body worm) (list (cons x y))))
    (when (> (length (body worm))
	     (max-length worm))
      (pop (body worm)))))

(defmethod draw ((worm worm))
  (loop for (x . y) in (body worm)
     do (sdl:draw-pixel-* x y :color (color worm))))

(defmethod handle-key (key (worm worm))
  (with-accessors ((hd horiz-dir) (vd vert-dir))  worm
    (case key
      (:sdl-key-up (unless (= vd 1)
		     (setf hd 0)
		     (setf vd -1)))
      (:sdl-key-down (unless (= vd -1)
		       (setf hd 0)
		       (setf vd 1)))
      (:sdl-key-left (unless (= hd 1)
		       (setf hd -1)
		       (setf vd 0)))
      (:sdl-key-right (unless (= hd -1)
			(setf hd 1)
			(setf vd 0))))))

;;; Edibles
(defclass edible ()
  ((x :initarg :x :initform (random *screen-width*) :accessor x-loc)
   (y :initarg :y :initform (random *screen-height*) :accessor y-loc)
   (size :initarg :size :initform 5 :accessor size)
   (color :initarg :color :initform *obj-color* :accessor color)))

(defclass food (edible)
  ((color :initform sdl:*green*)
   (bonus :initform 25 :accessor bonus)))

(defclass poison (edible)
  ((color :initform sdl:*magenta*)
   (penalty :initform 15 :accessor penalty)))

(defun make-random-edible ()
  (let ((choice (random 2)))
    (case choice
      (0 (make-instance 'food))
      (1 (make-instance 'poison)))))

(defmethod draw ((edible edible))
  (with-accessors ((x x-loc) (y y-loc)
		   (size size) (color color)) edible
    (sdl:draw-box-* x y size size :color color)))

(defmethod nom-nom ((worm worm) (food food))
  (incf (max-length worm) (bonus food))
  (erase food))
(defmethod nom-nom ((worm worm) (blagh poison))
  (decf (max-length worm) (penalty blagh))
  (setf (body worm) (subseq (body worm) (penalty blagh) (length (body worm))))
  (erase blagh))

(defmethod collided-p ((worm worm) (edible edible))
  (with-accessors ((worm-x head-x)
		   (worm-y head-y))
      worm
    (with-accessors ((food-x x-loc)
		     (food-y y-loc)
		     (size size))
	edible
      (not (or (< worm-x food-x)
	       (> worm-x (+ food-x size))
	       (< worm-y food-y)
	       (> worm-y (+ food-y size)))))))

(defparameter *screen-width* 400)
(defparameter *screen-height* 400)
(defparameter *obj-color* sdl:*white*)
(defparameter *bg-color* sdl:*black*)

(defun draw-score (score)
  (sdl:draw-string-shaded-* (format nil "Score: ~a" score)
			    (- (/ *screen-width* 2) 25) 
			    (+ (/ *screen-height* 2) 20)
			    *obj-color* *bg-color*))

(defvar *running* nil)
(defun main ()
  (setf *running* t)
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:initialise-default-font)
    (sdl:window *screen-width* *screen-height*
		:title-caption "OMG WURM"
		:icon-caption "OMG WURM")
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display *bg-color*)
    (let ((worm (make-instance 'worm))
	  (food (make-random-edible))
	  (score 0))
     (sdl:with-events ()
       (:quit-event () (prog1 t
			 (setf *running* nil)
			 (format t "~&Final score: ~a" score)))
       (:key-down-event (:key key)
			(handle-key key worm))
       (:idle ()
	      (sdl:clear-display *bg-color*)
	      ;; Draw stuff
	      (sdl:draw-string-shaded-* "OMG WURM"
					(- (/ *screen-width* 2) 25)
					(/ *screen-height* 2)
					*obj-color* *bg-color*)
	      (draw-score score)
	      (move worm)
	      (draw worm)
	      (draw food)
	      ;; collision/crash checks
	      (when (crashed-p worm)
		(format t "Crashed!")
		(setf *running* nil))
	      (when (collided-p worm food)
		(incf score)
		(nom-nom worm food)
		(setf food (make-random-edible)))
	      ;; not too short, are we?
	      (when (>= 0 (max-length worm))
		(format t "Poisoned to death!")
		(setf *running* nil))
	      ;; Update the display, we're done here.
	      (sdl:update-display)
	      (when (not *running*)
		(sdl:push-quit-event)))))))
