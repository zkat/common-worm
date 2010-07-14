(defpackage #:common-worm
  (:use :cl)
  (:export :main))
(in-package :common-worm)

;;;
;;; Entities
;;;

;;; Worm
(defclass worm ()
  ((head-x :initarg :x :accessor head-x)
   (head-y :initarg :y :accessor head-y)
   (max-length :initform 100 :accessor max-length)
   (horiz-dir :initform 0 :accessor horiz-dir)
   (vert-dir :initform 1 :accessor vert-dir)
   (speed :initform 0.01 :accessor speed)
   (color :initform uid:*green* :initarg :color :accessor color)
   (body :initform nil :accessor body)
   (crashedp :initform nil :accessor crashedp)
   (game :initarg :game :accessor game)))

(defmethod crashedp :around ((worm worm))
  (with-accessors ((x head-x)
                   (y head-y))
      worm
    (or (call-next-method)
        (<= x 0)
        (>= x (1- (uid:width (game worm))))
        (<= y 0)
        (>= y (1- (uid:height (game worm)))))))

(defmethod uid:on-key-down ((worm worm) keycode keysym string)
  (declare (ignore keycode string))
  (with-accessors ((hd horiz-dir)
                   (vd vert-dir))
      worm
    (case keysym
      (:up
       (unless (= vd -1)
         (setf hd 0)
         (setf vd 1)))
      (:down
       (unless (= vd 1)
         (setf hd 0)
         (setf vd -1)))
      (:left
       (unless (= hd 1)
         (setf hd -1)
         (setf vd 0)))
      (:right
       (unless (= hd -1)
         (setf hd 1)
         (setf vd 0))))))

(defmethod uid:on-update ((worm worm) dt)
  (flet ((point~= (p1 p2) (and (>= (abs (speed worm))
                                   (- (uid:point-x p1)
                                      (uid:point-x p2))
                                   (- (abs (speed worm))))
                               (>= (abs (speed worm))
                                   (- (uid:point-y p1)
                                      (uid:point-y p2))
                                   (- (abs (speed worm)))))))
    (with-accessors ((x head-x) (y head-y)) worm
      (unless (zerop dt)
        (incf x (/ (* (horiz-dir worm) (speed worm)) dt))
        (incf y (/ (* (vert-dir worm) (speed worm)) dt)))

      ;; TODO - this won't work anymore, since x/y positions are in floats now. Derp.
      (let ((new-point (uid:make-point x y)))

        (when (member new-point (body worm) :test #'point~=)
          (setf (crashedp worm) t))

        (setf (body worm) (append (body worm) (list new-point))))

      (when (> (length (body worm))
               (max-length worm))
        (pop (body worm))))))

(defmethod uid:on-draw ((worm worm))
  (uid:with-color (color worm)
    (gl:with-primitive :lines
      (loop for (point next-point) on (body worm)
         while next-point
         do
         (gl:vertex (uid:point-x point) (uid:point-y point))
         (gl:vertex (uid:point-x next-point) (uid:point-y next-point))))))

;;; Edibles
(defclass edible ()
  ((x :initarg :x :accessor x-loc)
   (y :initarg :y :accessor y-loc)
   (size :initarg :size :accessor size)
   (color :initarg :color :accessor color))
  (:default-initargs :size 5))

(defclass food (edible)
  ((bonus :initform 25 :accessor bonus))
  (:default-initargs :color uid:*green*))

(defclass poison (edible)
  ((penalty :initform 15 :accessor penalty))
  (:default-initargs :color uid:*magenta*))

(defmethod uid:on-draw ((edible edible))
  (with-accessors ((x x-loc)
                   (y y-loc)
                   (size size)
                   (color color))
      edible
    (uid:draw-rectangle x y size size :color color)))

(defmethod nom-nom ((worm worm) (food food))
  (incf (max-length worm) (bonus food)))
(defmethod nom-nom ((worm worm) (blagh poison))
  (decf (max-length worm) (penalty blagh))
  (setf (body worm)
        (subseq (body worm)
                (penalty blagh)
                (length (body worm)))))

(defmethod collidedp ((worm worm) (edible edible))
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

;;;
;;; Game
;;;
(defclass common-worm (uid:simple-game-engine)
  ((pausedp :initform nil :accessor pausedp)
   (obj-color :initform uid:*white* :accessor obj-color)
   (worm :initarg :worm :accessor worm)
   (food :initform nil :accessor food)
   (font :initarg :font :accessor font)
   (score :initform 0 :accessor score))
  (:default-initargs :fps-limit 30
    :width 400
    :height 400
    :font (make-instance 'uid:ftgl-font
                         :filepath (merge-pathnames "example.ttf" *default-pathname-defaults*)
                         :size 14)))

(defun main ()
  (uid:run (make-instance 'common-worm)))

(defun draw-text (game text x y)
  (uid:draw text :x x :y y :font (font game)))

(defun draw-score (game)
  (draw-text game (format nil "Score: ~a" (score game))
             (- (/ (uid:width game) 2) 20)
             (- (/ (uid:height game) 2) 40)))

(defmethod uid:init ((game common-worm))
  (setf (food game) (make-instance 'food
                                   :x (random (uid:width game))
                                   :y (random (uid:height game)))
        (worm game) (make-instance 'worm :x 1 :y 1 :game game)))

(defmethod uid:on-draw ((game common-worm))
  (uid:clear game)
  (uid:with-color uid:*white*
    (draw-text game (if (pausedp game)
                        "[P] to UNPAUSE"
                        "[P] to PAUSE")
               (- (/ (uid:width game) 2)
                  (if (pausedp game) 48 38))
               (- (/ (uid:height game) 2) 20))
    (draw-text game "OMG WURM"
               (- (/ (uid:width game) 2) 35)
               (/ (uid:height game) 2))
    (draw-text game (format nil "FPS: ~,1f" (uid:fps (uid:clock game)))
               0 (- (uid:height game) 15))
    (draw-score game))
  (uid:on-draw (food game))
  (uid:on-draw (worm game)))

(defmethod uid:on-update ((game common-worm) dt)
  (unless (pausedp game)
    (uid:on-update (worm game) dt)
    (when (crashedp (worm game))
      (print "Crashed!")
      (uid:close-window game))
    (when (collidedp (worm game) (food game))
      (incf (score game))
      (nom-nom (worm game) (food game))
      (setf (food game) (make-instance 'food
                                       :x (random (uid:width game))
                                       :y (random (uid:height game)))))
    (when (>= 0 (max-length (worm game)))
      (print "Poisoned to death!")
      (uid:close-window game))))

(defmethod uid:on-key-down ((game common-worm) keycode keysym string)
  (when (eq :p keysym)
    (setf (pausedp game)
          (not (pausedp game))))
  (when (eq :escape keysym)
    (uid:close-window game))
  (uid:on-key-down (worm game) keycode keysym string))

