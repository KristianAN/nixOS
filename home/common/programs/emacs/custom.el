;;; custom.el --- Custom functions -*- no-byte-compile: t; lexical-binding: t; -*-

(defun custom/consult-ripgrep-from-visual-selection ()
  "Send selected region to consult-ripgrep."
  (interactive)
  (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (consult-ripgrep nil selection)))

(defun custom/consult-grep-line-from-visual-selection ()
  "Send selected region to consult-ripgrep."
  (interactive)
  (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (consult-line selection)))

(defun custom/consult-grep-project-buffer-term ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (insert "term"))
    (call-interactively #'consult-project-buffer)))

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

(defun custom/duckduckgo-search-eww (query)
  "Search DuckDuckGo for QUERY and display results in EWW in another window.
If another window exists, reuse it. Otherwise, create a right split and use that."
  (interactive "sDuckDuckGo search: ")
  (let ((url (concat "https://duckduckgo.com/?q=" (url-hexify-string query))))
    (let ((target-window
           (if (one-window-p)
               (split-window-right)
             (next-window))))
      (select-window target-window)
      (eww url))))

(global-set-key (kbd "C-c s d") #'custom/duckduckgo-search-eww)

(defun custom/consult-ripgrep-from-visual-selection ()
  "Send selected region to consult-ripgrep."
  (interactive)
  (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (consult-ripgrep nil selection)))

;; Define the keymap explicitly
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Built-ins
    (define-key map (kbd "C-c e m") #'mark-sexp)
    ;; Custom functions
    (define-key map (kbd "C-c e r") #'custom/wrap-region-replace-symbols)
    (define-key map (kbd "C-c e w") #'custom/wrap-region-with-symbols)
    map)
  "Keymap for `my-keys-minor-mode'.")

(define-minor-mode my-keys-minor-mode
  "Minor mode to host personal keybindings that should override major modes."
  :init-value t
  :lighter " my-keys"
  :keymap my-keys-minor-mode-map)

;; Optionally give it very high precedence (helps with Evil or other emulation layers)
;; (add-to-list 'emulation-mode-map-alists
;;              `((my-keys-minor-mode . ,my-keys-minor-mode-map)))

;; Enable globally
(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)


;;; Nixos wrappers

(defvar nixos-system-root "~/nix/nixOS/"
  "The root directory of your NixOS configuration flake.")

(defun update-nixos ()
  "Run 'sudo nixos-rebuild switch --flake .' in the directory specified by `nixos-system-root`."
  (interactive)
  (let ((default-directory nixos-system-root))
    (async-shell-command "sudo nixos-rebuild switch --flake .")))


(defun update-nixos-restart-emacs ()
  "Run 'sudo nixos-rebuild switch --flake .' in the directory specified by `nixos-system-root`."
  (interactive)
  (let ((default-directory nixos-system-root))
    (async-shell-command "sudo nixos-rebuild switch --flake . && systemctl --user restart emacs.service")))
