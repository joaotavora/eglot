(defvar positions (with-temp-buffer
                    (insert-file-contents-literally "positions.el")
                    (read (current-buffer))))

(with-temp-buffer
  (insert-file-contents-literally "file.txt")
  (print (car (benchmark-run-compiled 5
                (mapc #'eglot--lsp-position-to-point positions)))))
