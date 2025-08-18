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

