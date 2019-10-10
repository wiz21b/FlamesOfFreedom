;; -*-lexical-binding: t;-*-
;;; flames-of-freedom.el --- The flames of freedom

;; Copyright (C) 2019 Stéphane Champailler
;; Author: Stéphane Champailler <schampailler@skynet.be>
;; Version: 1.0
;; Keywords: multimedia

;; This code is covered by the GNU's Affero General Public License Version 3.


;;; Code

(defun fof-make-vector-by-step (steps)
  "Build a vector by steps.

A step is a pair ( what . how-many). steps is a list of steps.

Example : (( 'a' . 1 ) ( 'b' . 2 )) will give
          [ 'a' 'b' 'b' ] "

  (cond ((null steps) [ ] )
	(1 (let ( (step (car steps)))
	     (vconcat (make-vector (car step) (cdr step))
		      (fof-make-vector-by-step (cdr steps)))))))

(defun fof-dups (l)
  "Count elements in a vector.

Input : a vector.
Output : a list of pair (element . count).

Example : '(10 10 10 20 20 30) will give
          '((10 . 3) (20 . 2) (30. 1))"

  ;; This was originally written as a tail recursive function.
  ;; However this brought emacs to its limits (and it raised error
  ;; because stack depth). So I rewrote it in a more imperative way.

  (let ( (i 1)
	 (current-elt (aref l 0))
	 (current-cnt 1)
	 (ret-list '()))

    (while (< i (length l))
      (let ( (first-of-rest (aref l i)) )
	(if (eq current-elt first-of-rest)
	    (setq current-cnt (+ 1 current-cnt))
	  (progn
	    (setq ret-list (cons (cons current-elt current-cnt) ret-list))
	    (setq current-elt first-of-rest)
	    (setq current-cnt 1))))

      (setq i (+ 1 i)))

    (if (> current-cnt 0)
	(progn
	  (setq ret-list (cons (cons current-elt current-cnt) ret-list))))

    (reverse ret-list)))


(defconst fof-int-to-blocks
  (fof-make-vector-by-step '( (1  . #x20)
			      (2  . ?.)
			      (2  . #x2591)
			      (5  . #x2592)
			      (8  . #x2592)
			      (10 . #x2592)
			      (10 . #x2593))))

(defconst fof-int-to-faces
  (fof-make-vector-by-step
   '((1  . (face (:foreground "grey"   :background "black")))
     (2  . (face (:foreground "grey"   :background "black")))
     (2  . (face (:foreground "grey"   :background "black")))
     (2  . (face (:foreground "orange" :background "grey")))
     (3  . (face (:foreground "orange" :background "red")))
     (4  . (face (:foreground "red"    :background "yellow")))
     (4  . (face (:foreground "yellow" :background "yellow")))
     (10 . (face (:foreground "white"  :background "yellow")))
     (10 . (face (:foreground "white"  :background "white"))))))

(defun fof-write-line (text pos)
  (goto-char (point-min))
  (forward-line pos)
  (delete-region (point) (+ (point) (length text)))
  (insert (propertize text 'face '(:foreground "white" :background "black"))))

(defun fof-make-flames-buffer (flame-buffer-width flame-buffer-height)
  (let ( (l (make-vector flame-buffer-height nil)))
    (dotimes (i flame-buffer-height)
      (aset l i (make-vector flame-buffer-width 0)))
    l))

(defun fof-update-flames (l flame-buffer-width time)

  ;; The lowest line of the flames is random.  I don't update it
  ;; too often so that the flames look nicer.

  (if (= 0 (% time 5))
      (let ((factor (if (< time flame-buffer-width)
			(truncate (+ 1 (* 35 (/ (float time) flame-buffer-width))))
		      36))
	    (last-line (- (length l) 1)))
	(dotimes (i flame-buffer-width)
	  (aset (aref l last-line)
		i (random factor)))))

  ;; Just compute the flames as usual. Add a bit of randomness to
  ;; make it look better.

  (dotimes (i (- (length l) 1))
    (let ( (v (aref l (+ i 1)))
	   (v2 (aref l i))
	   (width (- flame-buffer-width 2)))
      (dotimes (z width)
	(aset v2 (1+ z)
	      (/
	       (+ (aref v (+ 2 z)) (aref v (1+ z)) (aref v z)
		  (aref v2 (1+ z)) (- (random 4) 1))
	       4))))))

(defun fof-flames-to-string (l)
  "Build a big string representing the whole frame buffer.
The string has properties that tell the colour of each
of its characters."

  ;; I trust emacs to propose best-in-class implementations of
  ;; mapconcat

  (mapconcat
   (lambda (line)
     (mapconcat
      (lambda (x) (propertize
	  	   (make-string (cdr x)
	  			(aref fof-int-to-blocks (car x)))
	  	   'face (aref fof-int-to-faces (car x)) ))
      (fof-dups line)
      ""))
   l
   "\n"))

(defun fof-flames-to-string (l)

  (let* ((width-base (length (aref l 1)))
	 (width (+ 1 (length (aref l 1))))
	 (bigv (make-vector (* (length l) width) 0)) ; Recreating the vector is not signifcantly slower.

	 (i 0))
    (dotimes (y (length l))

      (let ((line (aref l y)))
	(dotimes (x width-base)
	  (aset bigv (+ i x) (aref fof-int-to-blocks (aref line x)))))

      (setq i (+ width i -1))
      (aset bigv i ?\n )
      (setq i (1+ i)))

  ;; Doing one big concat is faster than concatenating several strings together
  ;; Remember concat takes characters and makes strings out of them.
  (concat bigv)))


;; Things I tried to improve speed :

;; * A bit of profiling indicates that removing the call to
;;   put-text-property increases the speed by 50%.

;; * Using put-text-property directly on buffer seems faster than
;;   putting properties on strings and then inserting the string+props
;;   in the buffer

;; * set-text-properties is not faster then put-text-property
;;   in my case (but set-text-properties is cleaner).

;; * I tried to defsubst the lambda expression => no noticeable speed
;;   improvement.

;; * I tried to transform i into a global variable (defvar) => no
;;   noticeable speed improvement.

;; * I tried double buffering (hypothesis, emacs displays a buffer
;;   more efficitenly when it's a window refresh. Bad hypothesis, this
;;   makes things significantly slower.

;; * Byte compiling does make a noticeable improvement.

;; * Moved face construction outside the loop. Cleaner code, performance
;;   are the same.

(defun fof-flames-to-string-props (l)
  (let ((i 1))
    (dotimes (y (length l))

      (let ((line-props (fof-dups (aref l y))))
	(mapc (lambda (p)
		(set-text-properties i (+ i (cdr p)) (aref fof-int-to-faces (car p)))
		(setq i (+ i (cdr p))))
	      line-props))
      ;; skip \n at the end of each line
      (setq i (+ i 1)))))

;; This accelerates things by +/- 20%
(byte-compile 'fof-dups)
(byte-compile 'fof-update-flames)
(byte-compile 'fof-flames-to-string)
(byte-compile 'fof-flames-to-string-props)

(defun flames-of-freedom-my-message (&optional the-message testing)
  "Displays the Flames of Freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

A message is displayed. It is a list of sentences separated by
\"|\".  If you just want to stare at a comforting fire, just
leave the message empty."

  (interactive "sMessage to show (sentences separated by |):")

  (let* ((window (selected-window))

	 ;; -1 seems necessary for this to work in emacs-nw
	 (flame-buffer-width (- (window-body-width window) 0))
	 (flame-buffer-height (+ 3 (window-total-size)))
	 (l (fof-make-flames-buffer flame-buffer-width flame-buffer-height))
	 (buffer1 (get-buffer-create "Flames Of Freedom"))
	 (messages (vconcat (split-string the-message "|")))
	 (current-msg 0)
	 (drawn-frames 0)
	 (drawn-frames-benchmarking 0)
	 (start-time (float-time))
	 (last-time (float-time)))

    (if testing
	(message (format "%d x %d = %d" flame-buffer-width flame-buffer-height (* flame-buffer-width flame-buffer-height))))

    ;; help testing by makeing excution results repeatable.
    (if testing (random "alphabravo"))

    (buffer-disable-undo buffer1)
    (set-window-buffer window buffer1)
    (set-buffer buffer1)

    ;; Make the cursor disappears
    (setq cursor-type nil)

    (while (not (input-pending-p))

      ;; Computing the flames

      (fof-update-flames l flame-buffer-width drawn-frames)

      ;; Displaying the flames in the buffer.

      ;; * Replacing the buffer (setf (buffer-substring ...))
      ;;   instead of erasing/recreating it is not faster.
      ;; * Double buffering is slower.

      (let* ((sub-vec (seq-take l (min (length l) (window-total-size))))
	     (big-string (fof-flames-to-string sub-vec)))
	(erase-buffer)
	(insert big-string)
	(fof-flames-to-string-props sub-vec))

      ;; Display the messages in the middle of the screen

      (if (and the-message (> (length the-message) 0))

	  (progn
	    (if (> (float-time) (+ 2 last-time))
		(progn
		  (setq last-time (float-time))
		  (setq current-msg (% (+ 1 current-msg) (length messages)))))

	    (let* ((pos (/ (window-total-size) 2))
		   (blank-line (make-string (- flame-buffer-width 1) ?\  ))
		   (msg (aref messages current-msg))
		   (half (/ (- flame-buffer-width (length msg)) 2))
		   (centered-text (concat (make-string half ?\ )
					  msg
					  (make-string half ?\ )
					  (if (< (+ (* half 2) (length msg)) flame-buffer-width) " "))))

	      (fof-write-line blank-line (- pos 1))
	      (fof-write-line centered-text pos)
	      (fof-write-line blank-line (+ pos 1)))))

      ;; Remove useless information

      (set-buffer-modified-p nil) ;; with-silent-modification shows buffer modified indicator anyway

      ;; Make sure the window will display the buffer from its top

      (set-window-start window 1) ;; !!! This results in a 10% performance improvement. Dont't know why

      ;; Remember emacs favor processing/input over display so if I
      ;; don't ask, redisplay never get a chance to occur.

      (redisplay)

      (if (and testing (= drawn-frames-benchmarking 0))
	  (let ((passed-time (- (float-time) start-time)))
	    (if (> passed-time 10)
		(progn
		  (setq drawn-frames-benchmarking drawn-frames)
		  (message (format "%d frames drawn in %d seconds,  %f fps" drawn-frames-benchmarking passed-time (/ drawn-frames passed-time)))))))

      (setq drawn-frames (+ 1 drawn-frames)))

    (kill-buffer buffer1)

    ;; Make sure that the key the user has typed to exit our program
    ;; doesn't show up in its buffer
    (discard-input)))



(defun flames-of-freedom-default ()
  "Displays the Flames of Freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

A little poem is displayed."
  (interactive)
  (flames-of-freedom-my-message "These are the eternal flames of freedom,|Showing us light in darkness|beyond the thought police.|Software is our sword,|GPL the great ultimate.|" 1))


(flames-of-freedom-default)
