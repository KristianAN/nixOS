;;; custom.el --- Custom functions -*- no-byte-compile: t; lexical-binding: t; -*-

(defun kristian/consult-ripgrep-from-visual-selection ()
  "Send selected region to consult-ripgrep."
  (interactive)
  (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (consult-ripgrep nil selection)))

(defun kristian/consult-grep-line-from-visual-selection ()
  "Send selected region to consult-ripgrep."
  (interactive)
  (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (consult-line selection)))

(defun kristian/consult-grep-project-buffer-term ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (insert "term"))
    (call-interactively #'consult-project-buffer)))

;; Don't currently use this as doric-themes can toggle themselves
;; (custom-enabled-themes)
;; (defun kristian/my-toggle-theme ()
;;   (interactive)
;;   (let ((current (car custom-enabled-themes)))
;;     (if (string-match-p (regexp-quote "vivendi") (symbol-name current))
;;         (load-theme 'modus-operandi t)
;;       (load-theme 'modus-vivendi t))))
