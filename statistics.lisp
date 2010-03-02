(in-package :com.gigamonkeys.statistics)

(defun mean (numbers)
  (let ((length (length numbers)))
    (values (/ (apply #'+ numbers) length) length)))

(defun median (numbers)
  (elt (sort (copy-seq numbers) #'<) (floor (length numbers) 2)))

(defun mode (numbers)
  (declare (ignore numbers))
  (error "nyi"))

(defun numbers-stats (numbers)
  (list 
   :min (reduce #'min numbers)
   :max (reduce #'max numbers)
   :mean (mean numbers)
   :median (median numbers)
   :standard-deviation (standard-deviation numbers)
   :data-points (length numbers)))

(defun variance (numbers)
  (multiple-value-bind (mean length) (mean numbers)
    (/ (loop for x in numbers summing (expt (- x mean) 2)) length)))

(defun standard-deviation (numbers)
  (sqrt (variance numbers)))


#|
         float x1, x2, w, y1, y2;
 
         do {
                 x1 = 2.0 * ranf() - 1.0;
                 x2 = 2.0 * ranf() - 1.0;
                 w = x1 * x1 + x2 * x2;
         } while ( w >= 1.0 );

         w = sqrt( (-2.0 * ln( w ) ) / w );
         y1 = x1 * w;
         y2 = x2 * w;
|#

(defun box-muller ()
  "Based on code at http://www.taygeta.com/random/gaussian.html"
  (loop 
     for x1 = (1- (* 2.0d0 (random 1d0)))
     for x2 = (1- (* 2.0d0 (random 1d0)))
     for w = (+ (* x1 x1) (* x2 x2))
     while (>= w 1d0)
     finally 
       (let ((w (sqrt (/ (* -2d0 (log w)) w))))
	 (return (values (* x1 w) (* x2 w))))))

(defun normal-random-number (mean standard-deviation)
  (+ (* (box-muller) standard-deviation) mean))





