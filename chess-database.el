;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic code for manipulating game databases
;;

(defvar chess-database-handler nil)

(make-variable-buffer-local 'chess-database-handler)

(chess-message-catalog 'english
  '((no-such-database . "There is no such chess database module '%s'")))

(defun chess-database-open (module file)
  "Returns the opened database object, or nil."
  (let* ((name (symbol-name module))
	 (handler (intern-soft (concat name "-handler")))
	 buffer)
    (unless handler
      (chess-error 'no-such-database name))
    (when (setq buffer (funcall handler 'open file))
      (with-current-buffer buffer
	(setq chess-database-handler handler)
	(add-hook 'kill-buffer-hook 'chess-database-close nil t)
	(add-hook 'after-revert-hook 'chess-database-rescan nil t)
	(current-buffer)))))

(defsubst chess-database-command (database event &rest args)
  (with-current-buffer database
    (apply chess-database-handler event args)))

(defun chess-database-close (&optional database)
  (let ((buf (or database (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(remove-hook 'kill-buffer-hook 'chess-database-close t))
      (chess-database-command buf 'save)
      (chess-database-command buf 'close)
      (kill-buffer buf))))

(defun chess-database-save (database)
  (chess-database-command database 'save))

(defun chess-database-rescan (&optional database)
  (chess-database-command database 'rescan))

(defun chess-database-count (database)
  (chess-database-command database 'count))

(defun chess-database-read (database index-or-moniker)
  (if (integerp index-or-moniker)
      (chess-database-command database 'read index-or-moniker)
    (if (string-match "\\`\\([^:]+\\):\\([^#]+\\)#\\([0-9]+\\)\\'"
		      index-or-moniker)
	(let* ((type (match-string 1 index-or-moniker))
	       (path (match-string 2 index-or-moniker))
	       (index (string-to-int
		       (match-string 3 index-or-moniker)))
	       (db (chess-database-open
		    (intern (concat "chess-" type)) path)))
	  (if db
	      (chess-database-read db index))))))

(defun chess-database-write (database game)
  (chess-database-command database 'write game))

(defun chess-database-replace (database game &optional index)
  (chess-database-command database 'replace game index))

(defun chess-database-query (database &rest terms)
  (chess-database-command database 'query terms))

(provide 'chess-database)

;;; chess-database.el ends here
