(require 'cl-lib)

(defun generate-test-data ()
  (with-temp-file "file.txt"
    (setq-local buffer-file-coding-system 'utf-8)
    (add-file-local-variable 'buffer-file-coding-system 'utf-8)
    (goto-char (point-min))
    (let ((specs
           (cl-loop
            for line from 0 below 1000
            append (cl-loop
                    with width
                    with lsp-char-pos
                    ;; For lines with of distributed length between 0 and 90
                    for char below (abs (% (random) 90))
                    ;; write a funky character, one in ten times
                    if (zerop (abs (% (random) 10)))
                    do (setq width 2) and sum 2 into lsp-char-pos
                    and do (insert "𐐀")
                    else
                    do (setq width 1) and sum 1 into lsp-char-pos and do (insert "y")
                    ;; and, one in twenty times, write out data for verification
                    if (zerop (abs (% (random) 20)))
                    collect `((:line ,line :character ,(- lsp-char-pos width))
                              (:point ,(1- (point)) :char-after ,(char-after (1- (point))))))
            and do (insert "\n"))))
      (with-temp-file "positions.el"
        (add-file-local-variable 'flymake-diagnostic-functions nil)
        (goto-char (point-min))
        (let ((print-length nil))
          (let ((standard-output (current-buffer)))
            (pp `(setq eglot--124-benchmark-specs
                       (quote ,specs))))
          nil)))))

(generate-test-data)
