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
   '((1  . (:foreground "grey"   :background "black"))
     (2  . (:foreground "grey"   :background "black"))
     (2  . (:foreground "grey"   :background "black"))
     (2  . (:foreground "orange" :background "grey"))
     (3  . (:foreground "orange" :background "red"))
     (4  . (:foreground "red"    :background "yellow"))
     (4  . (:foreground "yellow" :background "yellow"))
     (10 . (:foreground "white"  :background "yellow"))
     (10 . (:foreground "white"  :background "white")))))

(defun fof-write-line (text pos)
  (goto-char 0)
  (forward-line pos)
  (delete-region (point) (+ (point) (length text)))
  (insert (propertize text 'face '(:foreground "white" :background "black"))))



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

;; This accelerates things by +/- 7%
(byte-compile 'fof-dups)
(byte-compile 'fof-update-flames)
(byte-compile 'fof-flames-to-string)

(defun flames-of-freedom-my-message (&optional the-message testing)
  "Displays the flames of freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

A message is displayed. It is a list of sentences separated by
\"|\".  If you just want to stare at a comforting fire, just
leave the message empty."

  (interactive "sMessage to show (sentences separated by |):")

  (let* ((window (selected-window))

	 ;; -1 seems necessary for this to work in emacs-nw
	 (flame-buffer-width (- (window-body-width window) 1))
	 (flame-buffer-height (+ 3 (window-total-size)))
	 (l (make-vector flame-buffer-height nil))
	 (buffer1 (get-buffer-create "Flames Of Freedom"))
	 (messages (vconcat (split-string the-message "|")))
	 (current-msg 0)
	 (drawn-frames 0)
	 (drawn-frames-benchmarking 0)
	 (start-time (float-time))
	 (passed-time 0)
	 (last-time (float-time)))

    ;; help testing by makeing excution results repeatable.
    (if testing (random "alphabravo"))

    (dotimes (i (length l))
      (aset l i (make-vector flame-buffer-width 0)))

    (buffer-disable-undo buffer1)
    (set-window-buffer window buffer1)
    (set-buffer buffer1)

    (while (not (input-pending-p))

      ;; Computing the flames

      (fof-update-flames l flame-buffer-width drawn-frames)

      ;; Displaying the flames in the buffer (actually replacing the
      ;; content of the buffer). This avoids double buffering but
      ;; doesn't make things significally faster.

      (let ((big-string (fof-flames-to-string
			 (seq-take l (min (length l) (window-total-size))))))
	(if (> (buffer-size) 1000)
	    (progn
	      ;; buffer-substring "end" index is not inclusive (see emacs
	      ;; documentation example)
	      (setf (buffer-substring 1 (+ (length big-string) 1)) big-string))
	  (progn
	    (insert big-string))))

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

      (set-window-start window 0) ;; !!! This results in a 10% performance improvement. Dont't know why

      ;; Remember emacs favor processing/input over display so if I
      ;; don't ask, redisplay never get a chance to occur.

      (goto-char 1) ;; Dunno why, but other values don't work as expected
      (redisplay)

      (if (and testing (< passed-time 10))
	  (progn
	    (setq passed-time (- (float-time) start-time))
	    (setq drawn-frames-benchmarking (1+ drawn-frames-benchmarking))))

      (setq drawn-frames (+ 1 drawn-frames)))

    (if testing
	(message (format "%d frames drawn in %d seconds,  %f fps" drawn-frames-benchmarking passed-time (/ drawn-frames passed-time))))


    (kill-buffer buffer1)

    ;; Make sure that the key the user has typed to exit our program
    ;; doesn't show up in its buffer
    (discard-input)))



(defun flames-of-freedom-default ()
  "Displays the flames of freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

A little poem is displayed."
  (interactive)
  (flames-of-freedom-my-message "These are the eternal flames of freedom,|Showing us light in darkness|beyond the thought police.|Software is our sword,|GPL the great ultimate.|" 1))


(flames-of-freedom-default)
