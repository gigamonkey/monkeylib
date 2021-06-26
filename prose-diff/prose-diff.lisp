;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :monkeylib-prose-diff)

(defparameter *word-regexp* (create-scanner "((?:\\\\n{.*?})|(?:\\w*’\\w+)|(?:\\w+))"))
(defparameter *old-mode-regexp* (create-scanner "-\\*- mode: .*; -\\*-"))
(defparameter *new-mode-line* "-*- mode: coders-at-work-editing; -*-")
(defparameter *minimum-match* 4)

(defun text (file)
  (with-output-to-string (s)
    (with-open-file (in file)
      (loop for line = (read-line in nil nil)
         while line do (write-line line s)))))

(defun words (text)
  (let (words)
    (do-matches (start end *word-regexp* text (coerce (nreverse words) 'vector))
      (push (intern (string-upcase (subseq text start end)) :keyword) words))))


(defun word-starts (text)
  (let (positions)
    (do-matches (start end *word-regexp* text (coerce (nreverse positions) 'vector))
      (push start positions))))

(defun positions-table (words)
  (let ((table (make-hash-table)))
    (loop for position from (1- (length words)) downto 0
       do (push position (gethash (aref words position) table ())))
    table))

(defun dump-table (table)
  (loop for k being the hash-keys of table
       do (format t "~a => ~a~%" k (gethash k table))))

;; XXX -- the policy of removing the first match if there are ties (by
;; length) is not necessarily quite right since you could have
;; something like: 'a b c d a b c e' in the original and then in the
;; edited: 'a b c x a b c d' in which case the first match attempt on
;; 'a' would find the two 'a b c' sequences and then would take the
;; first one leaving '- - - d a b c e'. Then when matching starting at
;; the second 'a' in the edited text the 'a b c d' has been broken up
;; so it will only match 'a b c' leaving an extra 'e'. Don't know how
;; much this will matter in practice. (Hmmm, maybe could do something
;; like: for each occurence of word W in edited find the longest match
;; in original. Then assign the longest overall to which ever
;; occurence of W it goes with and then axe any matches that overlap
;; with it and assign the longest remaining to it's W and so on. Then
;; move to the next word in edited.)
;;
;; Maybe for section that has ties, we should remember the position in
;; edited but leave the word vectors alone. Then after we've gone
;; through all of the edited words, go back and try the remembered
;; positions again. This will deal with the case where a short phrase
;; matches a number of places and the first one happens to be the
;; wrong one. If we don't take it out, then that instance of the short
;; phrase will hopefully be taken as part of some bigger piece of text
;; used later in the edited text. As it stands now, we'll take the
;; short phrase out of the middle of the longer text which will then
;; get broken into two bits surrounding the short phrase. (Which I
;; guess will then be taken from somewhere else.)

(defun find-original-text (original-text edited-text)
  "Given `original-text' and `edited-text' return two word vectors
  representing the given texts with the sequences of words occuring in
  both texts replaced by nil. Thus the sequences of words that remain
  in original are the ones not used in the edited version and the
  words that remain in edited are the ones added during editing."
  (loop
     with original        = (words original-text)
     with edited          = (words edited-text)
     with positions-table = (positions-table original)
     with edited-idx      = 0

     for word = (aref edited edited-idx)
     for positions = (gethash word positions-table)

     do
       (loop
          with longest-match = 0
          with longest-match-starts = ()

          for original-idx in positions
          for match = (match-length original edited original-idx edited-idx longest-match)

          do (when (>= match longest-match)
               (when (> match longest-match)
                 (setf longest-match-starts ())
                 (setf longest-match match))
               (push original-idx longest-match-starts))

          finally (cond
                    ((> longest-match *minimum-match*)
                     (null-words original (first longest-match-starts) longest-match)
                     (null-words edited edited-idx longest-match)
                     (incf edited-idx longest-match))
                    (t (incf edited-idx))))

     while (< edited-idx (length edited))
     finally (return (values original edited))))

(defun null-words (words start count)
  (loop repeat count for i from start do (setf (aref words i) nil)))

(defun map-in-and-out (words word-starts in-fn out-fn)
  "Given a vector `words' with some words nulled out and a parallel
  vector `word-starts' containing the starting positions of the words
  in a text (not given), map over the chunks of text, calling `in-fn'
  for each chunk of words present in `words' and `out-fn' for each
  chunk of words missing from `words'. The functions are called with a
  bounding index designator of the underlying text."
  (loop with text-idx = 0
     with word-idx = 0
     while word-idx
     do (let ((word (aref words word-idx))
              (end (aref word-starts word-idx)))
          (unless (zerop word-idx)
            (funcall (if word out-fn in-fn) text-idx end)
            (setf text-idx end))
          (setf word-idx (position nil words :start (1+ word-idx) :key (if word #'identity #'not))))
     finally (funcall (if (aref words (1- (length words))) in-fn out-fn) text-idx nil)))

(defun show-cuts (&key master edited output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (let ((text (text master)))
      (flet ((make-emitter (label)
               (lambda (start end)
                 (let ((text (subseq text start end)))
                   (when (zerop start)
                     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
                   (format out "{{~a}}~a" label text)))))
        (let ((words (find-original-text text (text edited))))
          (map-in-and-out words (word-starts text) (make-emitter "toss") (make-emitter "keep")))))))

(defun show-leftovers (&key master edited output)
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((text (text master)))
      (flet ((make-emitter (label)
               (lambda (start end)
                 (let ((text (subseq text start end)))
                   (when (zerop start)
                     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
                   (format out "{{~a}}~a" label text)))))
        (let ((words (find-original-text text (text edited))))
          (map-in-and-out words (word-starts text) (make-emitter "keep") (make-emitter "book")))))))

(defun show-unused (&key master edited output)
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (text master))
          (edited-text (text edited)))
      (flet ((make-emitter ()
               (lambda (start end)
                 (let ((text (subseq master-text start end)))
                   (write-string text out))))
             (make-elipsator ()
               (lambda (start end)
                 (let ((text (subseq master-text start end)))
                   (if (find #\Newline text)
                       (format out "~2&§~2%")
                       (format out " … "))))))
        (let ((words (find-original-text master-text edited-text)))
          (map-in-and-out words (word-starts master-text) (make-emitter) (make-elipsator)))))))

(defun show-possible-cuts (&key master edited output (minimum 0))
  "Emit all sections left in `master' after `edited' text has been
  removed, longer than `minimum' characters. Sections are output in
  the order they appear in `master'."
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (text master))
          (edited-text (text edited)))
      (let ((chunks-found 0))
        (flet ((make-emitter ()
                 (lambda (start end)
                   (let ((text (subseq master-text start end)))
                     (when (> (length text) minimum)
                       (when (zerop start)
                         (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
                       (incf chunks-found)
                       (format t "~&Found chunk of ~:d characters." (length text))
                       (format out "~2&{{~a}}" text))))))
          (let ((words (find-original-text master-text edited-text)))
            (map-in-and-out words (word-starts master-text) (make-emitter) (constantly nil))))
        (format t "~&~:d chunks of more than ~:d characters found." chunks-found minimum)))))

(defun unused (&key master edited output (minimum 0))
  "Emit version of `master' with sections used in `edited' replaced with ellipses."
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (text master))
          (edited-text (text edited)))
      (let ((chunks-found 0))
        (flet ((emit-unused (start end)
                 (let ((text (subseq master-text start end)))
                   (when (> (length text) minimum)
                     (incf chunks-found)
                     (format t "~&Found chunk of ~:d characters." (length text))
                     (write-string text out))))
               (emit-used (start end)
                 (declare (ignore start end))
                 (write-string " … " out)))
          (let ((words (find-original-text master-text edited-text)))
            (map-in-and-out words (word-starts master-text) #'emit-unused #'emit-used)))
        (format t "~&~:d chunks of more than ~:d characters found." chunks-found minimum)))))


(defun used-sections (&key master edited output (minimum 0))
  "Emit all sections from `edited' found in `master' in the order they
  appear in `master'."
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (text master))
          (edited-text (text edited)))
      (let ((chunks-found 0))
        (flet ((make-emitter ()
                 (lambda (start end)
                   (let ((text (subseq master-text start end)))
                     (when (> (length text) minimum)
                       (when (zerop start)
                         (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
                       (incf chunks-found)
                       (format t "~&Found chunk of ~:d characters." (length text))
                       (format out "~2&{{~a}}" text))))))
          (let ((words (find-original-text master-text edited-text)))
            (map-in-and-out words (word-starts master-text) (constantly nil) (make-emitter))))
        (format t "~&~:d chunks of more than ~:d characters found." chunks-found minimum)))))

(defun show-sorted-cuts (&key master edited output)
  "Emit all sections left in `master' after `edited' text has been
  removed, sorted by size, longer sections first."
  (with-open-file (out (ensure-directories-exist output) :direction :output :if-exists :supersede)
    (let ((master-text (text master))
          (edited-text (text edited)))
      (let ((words (find-original-text master-text edited-text))
            (chunks ()))
        (flet ((collector (start end)
                 (let ((text (subseq master-text start end)))
                   (push (list (length text) text start end) chunks))))
          (map-in-and-out words (word-starts master-text) #'collector (constantly nil)))
        (loop for (length text start end) in (sort chunks #'> :key #'first) do
             (format out "~2&** ~:d characters (~:d-~:d)~2%" length start end)
             (write-string text out))))))


(defun show-additions (&key master edited output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (let ((text (text edited)))
      (flet ((make-emitter (label)
               (lambda (start end)
                 (let ((text (subseq text start end)))
                   (when (zerop start)
                     (setf text (regex-replace *old-mode-regexp* text *new-mode-line*)))
                   (format out "{{~a}}~a" label text)))))
        (let ((words (nth-value 1 (find-original-text (text master) text))))
          (map-in-and-out words (word-starts text) (make-emitter "book") (make-emitter "keep")))))))


(defun match-length (original edited o-start e-start longest-match)
  (flet ((same (i)
           (let ((e (+ e-start i))
                 (o (+ o-start i)))
             (and (< e (length edited))
                  (< o (length original))
                  (eql (aref edited e) (aref original o))))))
    ;; Check the zeroth element in case the original got nulled out
    ;; due to a previous match. Then check that this match could at
    ;; least in theory be as long as the previous longest match, i.e.
    ;; the last element matches, before we go to the bother of
    ;; checking all the intervening elements.
    (if (and (same 0) (or (zerop longest-match) (same (1- longest-match))))
        (loop for i from 1 when (not (same i)) return i)
        0)))

;;; Scrap

(defun words-with-text-positions (text)
  (let (words positions)
    (do-matches (start end *word-regexp* text
                       (values (coerce (nreverse words) 'vector)
                               (nreverse (cons (length text) positions))))
      (push (intern (string-upcase (subseq text start end)) :keyword) words)
      (push start positions))))

(defun reconstitute-text (text)
  (multiple-value-bind (words positions) (words-with-text-positions text)
    (declare (ignore words))
    (string= text
             (with-output-to-string (s)
               (loop for (start end) on (cons 0 positions )
                  while end
                  do (format s "~a" (subseq text start end)))))))



;;; Experiments with fancy multi-level LCS foo.


(defun adding (n)
  #'(lambda (i) (+ i n)))

(defun extract-by-positions (s positions)
  (map (class-of s) #'(lambda (i) (aref s i)) positions))

(defun all-subseqs (orig edited)
  (labels ((positions (orig-start orig-end edited-start edited-end)
             (multiple-value-bind (orig-positions edited-positions)
                 (lcs-positions (subseq orig orig-start orig-end) (subseq edited edited-start edited-end))
               (values
                (map 'vector (adding orig-start) orig-positions)
                (map 'vector (adding edited-start) edited-positions))))

           (item (orig-start orig-end edited-start edited-end)
             (multiple-value-bind (orig-positions edited-positions)
                 (positions orig-start orig-end edited-start edited-end)
               (list
                (extract-by-positions orig orig-positions)
                orig-positions
                edited-positions))))
    (let ((found ()))
      (loop for orig-start from 0 below (length orig) do
        (loop for orig-end from (1+ orig-start) to (length orig) do
          (loop for edited-start from 0 below (length edited) do
            (loop for edited-end from (1+ edited-start) to (length edited)
                  when (and (eql (aref orig orig-start) (aref edited edited-start))
                            (eql (aref orig (1- orig-end)) (aref edited (1- edited-end)))) do
                    (let ((r (item orig-start orig-end edited-start edited-end)))
                      (when (< 0 (length (first r)))
                        (push `(,(score-item r) ,@r) found)))))))
      found)))


(defun test-traverse (orig edited)
  (let ((count 0))
    (let ((edited-positions (positions-table edited)))
      (loop for orig-start from 0 below (length orig) do
        (loop for orig-end from (1+ orig-start) below (length orig) do
          (loop for edited-start in (gethash (aref orig orig-start) edited-positions) do
            (loop for edited-end in (gethash (aref orig orig-end) edited-positions) do
              (incf count))))))
    count))


(defun score-item (item)
  (destructuring-bind (lcs orig-positions edited-positions) item
    (score (length lcs) (+ (positions-length orig-positions) (positions-length edited-positions)))))

(defun positions-length (positions)
  (- (1+ (aref positions (1- (length positions)))) (aref positions 0)))

(defun empty-bits (length)
  (make-array length :element-type 'bit :initial-element 0))

(defun positions->bits (positions length)
  (let ((bits (make-array length :element-type 'bit :initial-element 0)))
    (loop for p across positions do (setf (sbit bits p) 1))
    bits))

(defun overlap-p (b1 b2)
  (not (not (find 1 (bit-and b1 b2)))))

(defun find-subseqs (orig edited)
  "Given original and edited versions of some text, find the best
subsequences to account for common bits"
  (let ((found ())
        (orig-used (empty-bits (length orig)))
        (edited-used (empty-bits (length edited))))
    (dolist (item (sort (all-subseqs orig edited) #'> :key #'car))
      (destructuring-bind (score lcs orig-positions edited-positions) item
        (declare (ignore score lcs))
        (let ((in-orig (positions->bits orig-positions (length orig)))
              (in-edited (positions->bits edited-positions (length edited))))
          (when (not (or (overlap-p orig-used in-orig)
                         (overlap-p edited-used in-edited)))
            (bit-ior orig-used in-orig t)
            (bit-ior edited-used in-edited t)
            (push item found)))))
    (nreverse found)))



(defun foo-all-subseqs (orig edited)
  (let ((found ()))
    (loop for orig-start from 0 below (length orig) do
      (loop for orig-end from (1+ orig-start) to (length orig) do
        (loop for edited-start from 0 below (length edited) do
          (loop for edited-end from (1+ edited-start) to (length edited) do
            (let ((r (sim orig orig-start orig-end edited edited-start edited-end)))
              (when (plusp (first r))
                (push r found)))))))
    found))


(defun foo (orig edited)
  (dolist (x (sort (foo-all-subseqs orig edited) #'< :key #'car))
    (print x)))


(defun sim (orig orig-start orig-end edited edited-start edited-end)
  (let* ((lcs (lcs (subseq orig orig-start orig-end) (subseq edited edited-start edited-end)))
         (lcs-length (length lcs))
         (orig-length (- orig-end orig-start))
         (edited-length (- edited-end edited-start))
         (total-length (+ orig-length edited-length))
         (similarity (/ lcs-length (/ total-length 2d0)))
         (log-total-length (log total-length)))
    (list
     (* similarity log-total-length)
     :log-total-length log-total-length
     :similarity similarity
     :lcs (coerce lcs 'string)
     :lcs-length lcs-length
     :orig-start orig-start
     :orig-end orig-end
     :orig-length orig-length
     :orig-section (subseq orig orig-start orig-end)
     :edited-start edited-start
     :edited-end edited-end
     :edited-length edited-length
     :edited-section (subseq edited edited-start edited-end))))


(defvar *length-exponent* 1.1)

(defun score (lcs-length total-length &key (length-exponent *length-exponent*))
  (float (/ (expt lcs-length length-exponent) total-length) 0d0)
  #+(or)(let ((similarity (/ lcs-length (/ total-length 2d0))))
          (* similarity (log total-length))))


;;; GAH. This is where I figured out that when adding the node to the
;;; end of a subsequence you don't necessarily get the best score by
;;; adding the node going back to the previous node with the best
;;; score.

(defun test-score (n)
  (let ((results ()))
    (loop repeat n do
      (let* ((a (1+ (random 100)))
             (b (+ a (random 100)))
             (c (1+ (random 100)))
             (d (+ c (random 100))))
        (flet ((check (p)
                 (loop for x from 0 to 1000
                       unless (funcall p (score (+ a 1) (+ b 1 x)) (score (+ c 1) (+ d 1 x)))
                         do
                            (format t "~&(score ~a ~a~*) ~a (score ~a ~a~*) but ~7:* (score (+ ~a 1) (+ ~a 1 ~a)) not ~a (score (+ ~a 1) (+ ~a 1 ~a))" a b x p c d x)
                            (push (list :a a :b b :c c :d d :x x) results)
                            (loop-finish))))
          (unless (<= (score a b) (score c d))
            (psetf a c b d c a d b))
          (check '<))))
    results))


(defun test-score-2 (n)
  (loop for a from 1 to n do
    (loop for b from (1+ a) to n do
      (loop for c from (1+ a) to n do
        (loop for d from (1+ c) to n
              when (zerop (- (score a b) (score c d)))
                do
                   (print (list :a a :b b :c c :d d))
                   (loop-finish))))))



(defun find-zero (a b c d)
  (loop for x from 0
          thereis (when (= (score a (+ b x)) (score c (+ d x)))
                    (list :a a :b b :c c :d d :x x))))


(defun subseq-nodes (table)
  "Given a LCS table, find all the cells that represent nodes of some subseq."
  (destructuring-bind (rows columns) (array-dimensions table)
    (let ((r ()))
      (loop for row from 1 below rows do
        (loop for col from 1 below columns do
          (when (= (aref table (1- row) (1- col))
                   (aref table (1- row) col)
                   (aref table row (1- col))
                   (1- (aref table row col)))
            (push (list row col (aref table row col)) r))))
      r)))


(defun nodes-by-length (nodes)
  (let ((table (make-hash-table)))
    (dolist (n nodes)
      (destructuring-bind (row col length) n
        (declare (ignore row col))
        (push n (gethash length table))))
    table))


;;; A bunch of ways to try and find all the node chains. The good one is node-chains-dp2 at the bottom.

(defun rnode-chains (node by-length)
  (destructuring-bind (row col length) node
    (if (= 1 length)
      (list (list node))
      (let ((chains ()))
        (dolist (prev (gethash  (1- length) by-length))
          (when (and (< (first prev) row) (< (second prev) col))
            (dolist (chain (node-chains prev by-length))
              (push (cons node chain) chains))))
        chains))))

(defun node-chains (len by-length)
  (flet ((can-continue-p (node next)
           ;;(format t "~&Checking ~a vs ~a~&" node next)
           (destructuring-bind (r1 c1 l1) node
             (declare (ignore l1))
             (destructuring-bind (r2 c2 l2) next
               (declare (ignore l2))
               (and (< r1 r2) (< c1 c2))))))

  (let ((chains ()))
    (dolist (head (gethash len by-length))
      (let ((continuations (node-chains (1+ len) by-length)))
        (if continuations
          (dolist (rest continuations)
            (when (can-continue-p head (first rest))
              (push (cons head rest) chains)))
          (push (cons head nil) chains))))
    chains)))


(defun node-chains-dp (by-length)
  (let* ((end (reduce #'max (hash-table-keys by-length)))
         (table (make-array (1+ end) :initial-element nil)))
    ;; Fill the last row in the table.
    (setf (aref table end) (mapcar #'list (gethash end by-length)))
    (labels ((can-continue-p (node next)
               ;;(format t "~&Checking ~a vs ~a~&" node next)
               (destructuring-bind (r1 c1 l1) node
                 (declare (ignore l1))
                 (destructuring-bind (r2 c2 l2) next
                   (declare (ignore l2))
                   (and (< r1 r2) (< c1 c2)))))
             (chains (n)
               (let ((chains ()))
                 (dolist (head (gethash n by-length))
                   (dolist (rest (aref table (1+ n)))
                     (when (can-continue-p head (first rest))
                       (push (cons head rest) chains))))
                 chains)))
      (loop for n from (1- end) downto 1 do
        (setf (aref table n) (chains n)))
      (aref table 1))))

(defun node-chains-dp2 (by-length)
  (let* ((end (reduce #'max (hash-table-keys by-length)))
         (prev (mapcar #'list (gethash end by-length))))
    (labels ((can-continue-p (node next)
               (destructuring-bind (r1 c1 l1) node
                 (declare (ignore l1))
                 (destructuring-bind (r2 c2 l2) next
                   (declare (ignore l2))
                   (and (< r1 r2) (< c1 c2)))))
             (chains (n)
               (let ((chains ()))
                 (dolist (head (gethash n by-length))
                   (dolist (rest prev)
                     (when (can-continue-p head (first rest))
                       (push (cons head rest) chains))))
                 chains)))
      (loop for n from (1- end) downto 1 do
        (setf prev (chains n)))
      prev)))


(defclass sub-chain ()
  ((start :initarg :start :reader start)
   (end :initarg :end :reader end)
   (nodes :initarg :nodes :reader nodes)))

(defmethod print-object ((obj sub-chain) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (with-slots (start end nodes) obj
      (format stream "start: ~a; end: ~a; nodes: ~a; score: ~a" start end nodes (score-of obj)))))

(defun score-of (chain)
  (with-slots (start end nodes) chain
    (score nodes (total-distance start end))))

(defun total-distance (c1 c2)
  (/ (+ (- (car c2) (car c1)) (- (cdr c2) (cdr c1))) 2))

(defun combine (c1 c2)
  (make-instance 'sub-chain
    :start (start c1)
    :end (end c2)
    :nodes (+ (nodes c1) (nodes c2))))

(defun list->sub-chain (x)
  (destructuring-bind (x y n) x
    (declare (ignore n))
    (make-instance 'sub-chain :start (cons x y) :end (cons (1+ x) (1+ y)) :nodes 1)))

(defun chain->sub-chains (chain)
  (mapcar #'list->sub-chain chain))

(defun coalesce-chains (chains &key (length-exponent *length-exponent*))
  (let ((*length-exponent* length-exponent))
    (destructuring-bind (first . rest) chains
      (if rest
        (multiple-value-bind (rest rest-changed) (coalesce-chains rest)
          (let ((combined (combine first (first rest))))
            (if (> (score-of combined) (score-of first))
              (values (cons combined (rest rest)) t)
              (values (cons first rest) rest-changed))))
        (values chains nil)))))



#+(or)(defun max-distance (nodes total-distance)
  "Given a substring of nodes of a chain spanning a total distance, what is most extra distance we can add to pick up another node?"
  (let ((new-lcs-length (1+ lcs-length)))
    (- (floor (/ (* new-lcs-length new-lcs-length total-distance) (* lcs-length lcs-length))) total-distance)))



#+(or)
(progn
  (defparameter *lcs-table* (lcs-table
                             (words (file-text "/Users/peter/writing/books/we-learned-a-lot/interviews/andré-arko/transcripts/20210516-andré-arko.txt"))
                             (words (file-text "/Users/peter/writing/books/we-learned-a-lot/book/stories/rubygems.txt"))))

  (defparameter *nodes* (subseq-nodes *lcs-table*))

  (defparameter *nodes-by-length* (nodes-by-length *nodes*))

  (defparameter *raw-chains* (node-chains-dp2 *nodes-by-length*)))
