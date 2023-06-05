(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(let ((default-directory package-user-dir))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)
(use-package project
  :ensure t
  :config
  ;; Just set project-root to open buffer for tests
  (setq project-find-functions nil))

(use-package python-mode
  :ensure t
  :custom
  (py-prompt-on-change-p nil)
  (py-ask-about-save nil)
  :config
  (use-package with-venv
    :ensure t)
  (use-package pyvenv
    :ensure t)
  (setenv "PIPENV_VERBOSITY" "-1"))

(use-package eglot
  :ensure t
  :custom
  (read-process-output-max (* 1024 1024))
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 50000)
  (eglot-send-changes-idle-time .5)
  (eldoc-echo-area-use-multiline-p t)
  (eglot-autoreconnect nil)
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (defun my/output-resources(&rest args)
    (interactive)
    (message "%s\n%s" (make-string 42 ?v) (make-string 42 ?*))
    (message "Total count file-notify-descriptors: %s" (hash-table-count file-notify-descriptors))
    (let ((mem-usage (string-to-number
                      (shell-command-to-string
                       (concat "ps -o rss= -p " (number-to-string (emacs-pid)))))))
      (message "Total MB of memory (rss) Emacs is using: %.2f MB" (/ mem-usage 1024.0)))
    ;; (message "GC:\n%s" (garbage-collect))
    ;; (let ((counts (vconcat (memory-use-counts)))
    ;;       (meanings ["cons cells" "floats"
    ;;                  "vector slots" "symbols"
    ;;                  "string-chars" "intervals" "strings"]))
    ;;   (dotimes (i 7)
    ;;     (message "%s count: %d" (aref meanings i) (aref counts i))))
    (message "%s\n%s" (make-string 42 ?^) (make-string 42 ?*)))

  (advice-add 'eglot-register-capability :after #'my/output-resources))

(defun my/python-mode-hook()
  (when (y-or-n-p "Do you want to load the \"override\" eglot.el vs. installed package?")
    (load-file "../../eglot.el"))
  (setenv "PIPENV_VERBOSITY" "-1")
  (let ((pyvenv-found (or (with-venv-find-venv-dir-venv)
                          (with-venv-find-venv-dir-dot-venv))))
    (when pyvenv-found
      (pyvenv-activate pyvenv-found))
    ;; To trigger automatically in batch
    (call-interactively 'eglot)))

(add-hook 'python-mode-hook #'my/python-mode-hook)

(defun my/js-mode-hook()
  (call-interactively 'eglot))

(add-hook 'js-mode-hook #'my/js-mode-hook)

(defun my/rust-mode-hook()
  (call-interactively 'eglot))

(add-hook 'rust-mode-hook #'my/rust-mode-hook)

(message "Baseline stats...")
(my/output-resources)

(message "Python Mode stats...")
(find-file "python/main.py")
;; Ensure that we've received "didChangeWatchedFiles" request twice.
(sit-for 15)

;; In case wanted to see behavior of others

;; typescript-language-server only calls didChangeWatchedFiles once
;; (message "JS Mode stats...")
;; (find-file "js/main.js")
;; ;; Ensure that we've received "didChangeWatchedFiles" request twice.
;; (sit-for 15)

;; rust-analyzer calls didChangeWatchedFiles twice, but uses same id for both
;; (message "Rust Mode stats...")
;; (find-file "rust/src/main.rs")
;; ;; Ensure that we've received "didChangeWatchedFiles" request twice.
;; (sit-for 15)
