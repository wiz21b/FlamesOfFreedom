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
Example : with ( ( 'a' . 1 ) ( 'b' . 2 ) ) will return [ 'a' 'b' 'b' ] "

  (cond ((null steps) [ ] )
	(1 (let ( (step (car steps)))
	     (vconcat (make-vector (car step) (cdr step))
		      (fof-make-vector-by-step (cdr steps)))))))

(defun fof-dups (l)
  "Count elements in a vector.

Input : a vector.
Output : a list of pair (element . count).
Example : '(10 10 10 20 20 30) will give '((10 . 3) (20 . 2) (30 . 1))"

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
  (fof-make-vector-by-step '( (1  . (:foreground "grey" :background "black"))
                              (2  . (:foreground "grey" :background "black"))
                              (2  . (:foreground "grey" :background "black"))
                              (2  . (:foreground "orange" :background "grey"))
                              (3  . (:foreground "orange" :background "red"))
                              (4  . (:foreground "red" :background "yellow"))
                              (4  . (:foreground "yellow" :background "yellow"))
                              (10 . (:foreground "white" :background "yellow"))
                              (10 . (:foreground "white" :background "white")))))

(defun fof-write-line (text pos)
  (goto-char 0)
  (forward-line pos)
  (delete-region (point) (+ (point) (length text)))
  (insert (propertize text 'face '(:foreground "white" :background "black"))))


(defun flames-of-freedom-default ()
  "Displays the flames of freedom.

These are the eternal flames of freedom,
Showing us light in the darkness of the policy of truth.
(and an homage to RMS who is having tough times in this year
2019)."
  (interactive)
  (flames-of-freedom-my-message "These are the eternal flames of freedom,|Showing us light in darkness|beyond the thought police.|Software is our sword,|GPL the great ultimate.|"))


(defun flames-of-freedom-my-message (&optional the-message)
  "Displays the flames of freedom.

These are the eternal flames of freedom,
With this command you can specify your own message to show in the
flames. The message is a list of sentences separated by \"|\".
If you just want to stare at a comforting fire, just leave the
message empty."

  (interactive "sMessage to show (sentences separated by |):")

  (let* ((window (selected-window))
	 (flame-buffer-width (- (window-body-width window) 1))
	 (flame-buffer-height (+ 3 (window-total-size)))
	 (l (make-vector flame-buffer-height nil))
	 (time 0)
	 (buffer1 (get-buffer-create "FlamesOfFreedomI"))
	 (buffer2 (get-buffer-create "FlamesOfFreedoml"))
	 (messages (vconcat (split-string the-message "|")))
	 (current-msg 0)
	 (last-time (float-time))
	 )

    (dotimes (i (length l))
      (aset l i (make-vector flame-buffer-width 0)))

    (buffer-disable-undo buffer1)
    (buffer-disable-undo buffer2)

    (set-window-buffer window buffer2)
    (set-buffer buffer2)


    (while (not (input-pending-p))

      ;; Computing the flames

      ;; The lowest line of the flames is random.  I don't update it
      ;; too often so that the flames look nicer.

      (if (= 0 (% time 5))
	  (let ((factor (if (< time flame-buffer-width)
			   (truncate (+ 1 (* 35 (/ (float time) flame-buffer-width))))
			  36)))
	    ;;(message (princ factor))
	    (dotimes (i flame-buffer-width)
	      (aset (aref l (- flame-buffer-height 1))
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
		   4)))))

      ;; That's screen flipping in emacs :-)

      (cond ((eq (current-buffer) buffer1)
	     (set-window-buffer window buffer1 t)
	     (set-buffer buffer2))
	    ((eq (current-buffer) buffer2)
	     (set-window-buffer window buffer2 t)
	     (set-buffer buffer1)))

      ;; Displaying the flames in the buffer

      (erase-buffer)
      (goto-char 0)

      (let ( (i 1) )
	(while (< i (- (length l) 1))

	  (insert-before-markers
	   (mapconcat
	    (lambda (x) (propertize
	  		 (make-string (cdr x)
	  			      (aref fof-int-to-blocks (car x)))
	  		 'face (aref fof-int-to-faces (car x)) ))
	    (fof-dups (aref l i))
	    ""))

	  ;; FIXME The way I break a line introduces unwanted
	  ;; spaces...

	  (insert-char ?\n)
	  (end-of-buffer)

	  (setq i (+ i 1))))

      ;; Display the message sin the middle of the screen

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

      (set-buffer-modified-p nil)

      ;; Make sure the window will display the buffer from its top

      (set-window-start window 0)
      (goto-char 0)

      ;; Remmber emacs favor processing over display so if I don't ask,
      ;; redisplay never get a chance to occur.

      (redisplay)

      (setq time (+ 1 time)))


    (kill-buffer buffer2)
    (kill-buffer buffer1)

    ;; Make sure that the key the user has typed to exit our mode
    ;; doesn't show up in its buffer
    (discard-input)))

(flames-of-freedom-default)
