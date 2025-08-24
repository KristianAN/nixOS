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

test ("foo")
(defconst custom/wrap-symbol-pairs
  '((?\( . ("(" . ")"))
    (?\{ . ("{" . "}"))
    (?\[ . ("[" . "]"))
    (?\" . ("\"" . "\""))
    (?'  . ("'" . "'")))
  "Alist mapping opening symbol characters to their string pairs.")

(defun custom/get-wrap-symbol-pair (char)
  "Return (OPEN . CLOSE) pair for CHAR from `custom/wrap-symbol-pairs`, or CHAR itself for both."
  (or (cdr (assq char custom/wrap-symbol-pairs))
      (cons (char-to-string char) (char-to-string char))))

(defun custom/wrap-region-with-symbols (open close)
  "Wrap the region (selected text) with OPEN and CLOSE symbols."
  (interactive
   (let* ((char (read-char "Wrap with: (type opening symbol, e.g. (, {, \", [, '): "))
          (pair (custom/get-wrap-symbol-pair char)))
     (list (car pair) (cdr pair))))
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open)))))

(defun custom/wrap-region-replace-symbols (open close)
  "Replace surrounding symbols in region with OPEN and CLOSE."
  (interactive
   (let* ((char (read-char "Replace with: (type opening symbol, e.g. (, {, \", [, '): "))
          (pair (custom/get-wrap-symbol-pair char)))
     (list (car pair) (cdr pair))))
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        ;; Remove last character
        (goto-char end)
        (delete-backward-char 1)
        ;; Remove first character
        (goto-char beg)
        (delete-char 1)
        ;; Insert new symbols
        (goto-char beg)
        (insert open)
        (goto-char (+ beg (length open) (- (- end beg) 2)))
        (insert close)))))
