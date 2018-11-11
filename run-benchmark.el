(require 'cl-lib)
(require 'eglot)

(defvar eglot--124-benchmark-specs nil)

(defun eglot--124-benchmark (positions-file test-file)
  (load positions-file nil 'nomessage)

  (with-current-buffer
      (find-file-noselect test-file)
    (let* (misses
           (total-time
            (car (benchmark-run 5
                   (pcase-dolist
                       (`(,lsp-position (:point ,expected-point :char-after ,expected-char-after))
                        eglot--124-benchmark-specs)
                     (let ((observed-point (eglot--lsp-position-to-point lsp-position)))
                       (unless (and (= expected-point observed-point)
                                    (eql (char-after expected-point)
                                         (char-after observed-point)))
                         (push `(,lsp-position
                                 :expected-point ,expected-point
                                 :expected-char-after ,expected-char-after
                                 :observed-point ,observed-point
                                 :observed-char-after ,(char-after observed-point))
                               misses))))))))
      (princ (format "%ss total time, %d misses" total-time (length misses)))
      misses)))

(setq command-line-args
      (cl-member load-file-name command-line-args
                 :key #'expand-file-name :test #'equal))

(eglot--124-benchmark
 (expand-file-name (or (cl-second command-line-args) "positions.el"))
 (or (cl-third command-line-args) "file.txt"))

(provide 'run-benchmark)
;;; run-benchmark.el ends here
