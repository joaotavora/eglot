;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generate Fischer Random style positions
;;
;; Just call `chess-fischer-random-position' to generate such a
;; position.
;;

(require 'chess-pos)

(defvar pieces-vector [?r ?n ?b ?q ?k ?b ?n ?r])

(defun chess-shuffle-vector (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)"
  (let ((i 0)
	j
	temp
	(len (length vector)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (aref vector i))
      (aset vector i (aref vector j))
      (aset vector j temp)
      (setq i (1+ i))))
  vector)

;;;###autoload
(defun chess-fischer-random-position ()
  "Generate a Fischer Random style position."
  (let (pieces position)
    (while (null position)
      (setq pieces (chess-shuffle-vector pieces-vector))
      (let (first-bishop first-rook king)
	(catch 'retry
	  (dotimes (i 8)
	    (let ((piece (aref pieces i)))
	      (cond
	       ((= ?b piece)
		(if first-bishop
		    (if (= (mod i 2) first-bishop)
			(throw 'retry t))
		  (setq first-bishop (mod i 2))))
	       ((= ?k piece)
		(if (null first-rook)
		    (throw 'retry t))
		(setq king i))
	       ((= ?r piece)
		(if first-rook
		    (if (null king)
			(throw 'retry t))
		  (setq first-rook i))))))
	  (setq position (chess-pos-create)))))

    ;; set the home row pieces
    (dotimes (i 8)
      (chess-pos-set-piece position (chess-rf-to-index 0 i)
			   (aref pieces i))
      (chess-pos-set-piece position (chess-rf-to-index 7 i)
			   (upcase (aref pieces i))))

    position))

(provide 'chess-random)

;;; chess-random.el ends here
