;;; eglot-benchmark.el --- for move-to-column functions -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; compile, then:
;; emacs -Q --batch -l eglot-benchmark.elc --eval '(run)'
;;
;; See github#361.

(require 'benchmark)

(defun eglot-move-to-column (column)
  "Move to COLUMN without closely following the LSP spec."
  ;; We cannot use `move-to-column' here, because it moves to *visual*
  ;; columns, which can be different from LSP columns in case of
  ;; `whitespace-mode', `prettify-symbols-mode', etc.  (github#296,
  ;; github#297)
  (goto-char (min (+ (line-beginning-position) column)
                  (line-end-position))))

(defun eglot-move-to-lsp-abiding-column (column)
  "Move to COLUMN abiding by the LSP spec."
  (cl-loop
   initially (move-to-column column)
   with lbp = (line-beginning-position)
   for diff = (- column
                 (/ (- (length (encode-coding-region lbp (point) 'utf-16 t))
                       2)
                    2))
   until (zerop diff)
   do (forward-char (/ (if (> diff 0) (1+ diff) (1- diff)) 2))))

(defun fix-1 (column)
  "Move to COLUMN abiding by the LSP spec."
  (save-restriction
    (narrow-to-region (line-beginning-position) (line-end-position))
    (goto-char (point-max))
    (setq column (min column
                      (/ (- (length (encode-coding-region 1 (point) 'utf-16 t))
                            2)
                         2)))
    (cl-loop
     initially (move-to-column column)
     for diff = (- column
                   (/ (- (length (encode-coding-region 1 (point) 'utf-16 t))
                         2)
                      2))
     until (zerop diff)
     do (condition-case nil
            (forward-char (/ (if (> diff 0) (1+ diff) (1- diff)) 2))
          (end-of-buffer)))))

(defun run-one (move-column-fn desc line column)
  (with-temp-buffer
    (insert line)
    (apply #'message "|%20s|%40s| %2.6f %d %d" desc move-column-fn
           (benchmark-run
               100000
             (funcall move-column-fn column)
             (goto-char (point-min))))))

(defun run ()
  (message "%s%s%s\n"
           "total elapsed time for execution, "
           "the number of garbage collections that ran, "
           "and the time taken by garbage collection.")
  (message "|test case | fn | result")
  (message "|----------|----|-------")
  (dolist
      (desc-line-column
       `(("short ascii" "asdf asdf asdf" 5)
         ("short non-ascii"
          "const char write_data[] = u8\"🚂🚃🚄🚅🚆🚈🚇🚈🚉🚊🚋🚌🚎🚝🚞🚟🚠🚡🛤🛲\";p "
          71)
         ,`("long ascii front" ,(make-string 400 ?a) 20)
         ,`("long ascii   end" ,(make-string 400 ?a) 377)
         ,`("long non-ascii front"
            ,(apply 'concat
                    (cl-loop repeat 13
                             collect
                             "write_data[] = u8\"🚂🚃🚄🚅🚆🚈🚇🚈🚉🚊\""))
            20)
         ,`("long non-ascii   end"
            ,(apply 'concat
                    (cl-loop repeat 13
                             collect
                             "write_data[] = u8\"🚂🚃🚄🚅🚆🚈🚇🚈🚉🚊\""))
            377)))
  (dolist (fn '(eglot-move-to-column
                move-to-column
                eglot-move-to-lsp-abiding-column
                fix-1))
    (apply 'run-one fn desc-line-column))))

;; Local Variables:
;; bug-reference-bug-regexp: "\\(github#\\([0-9]+\\)\\)"
;; bug-reference-url-format: "https://github.com/joaotavora/eglot/issues/%s"
;; End:

;;; eglot-benchmark.el ends here
