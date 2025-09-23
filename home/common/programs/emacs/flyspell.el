;;; flyspell.el --- Setup spellcheck for emacs -*- no-byte-compile: t; lexical-binding: t; -*-
;; Set up hunspell for spell checking
(setq ispell-program-name "hunspell")

;; Configure available dictionaries
(setq ispell-local-dictionary-alist
      '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
        ("nb_NO" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "nb_NO") nil utf-8)
        ("nn_NO" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "nn_NO") nil utf-8)))

;; Set British English as default dictionary
(setq ispell-dictionary "en_GB")
(setq ispell-local-dictionary "en_GB")

;; Enable flyspell in org-mode and text-mode
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Enable flyspell for comments and strings in programming modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Dictionary switching functions
(defun my/switch-to-english ()
  "Switch to British English dictionary."
  (interactive)
  (ispell-change-dictionary "en_GB")
  (when flyspell-mode
    (flyspell-buffer))
  (message "Switched to British English (ise) dictionary"))

(defun my/switch-to-norwegian ()
  "Switch to Norwegian Bokmål dictionary."
  (interactive)
  (ispell-change-dictionary "nb_NO")
  (when flyspell-mode
    (flyspell-buffer))
  (message "Switched to Norwegian Bokmål dictionary"))

(defun my/switch-to-nynorsk ()
  "Switch to Norwegian Nynorsk dictionary."
  (interactive)
  (ispell-change-dictionary "nn_NO")
  (when flyspell-mode
    (flyspell-buffer))
  (message "Switched to Norwegian Nynorsk dictionary"))

;; Toggle between English and Norwegian
(defun my/toggle-dictionary ()
  "Toggle between British English and Norwegian dictionaries."
  (interactive)
  (if (string= ispell-current-dictionary "en_GB")
      (my/switch-to-norwegian)
    (my/switch-to-english)))

;; Keybindings for dictionary switching
(global-set-key (kbd "C-c d e") 'my/switch-to-english)
(global-set-key (kbd "C-c d n") 'my/switch-to-norwegian)
(global-set-key (kbd "C-c d y") 'my/switch-to-nynorsk)
(global-set-key (kbd "C-c d t") 'my/toggle-dictionary)

;; Simple modeline integration - add dictionary indicator to minor modes
(defun my/ispell-dictionary-modeline ()
  "Return current dictionary for modeline."
  (when (and (bound-and-true-p ispell-current-dictionary)
             flyspell-mode)
    (pcase ispell-current-dictionary
      ("en_GB" " EN-GB")
      ("nb_NO" " NO-BM")
      ("nn_NO" " NO-NN")
      (_ ispell-current-dictionary))))

;; Only add if not already present
(unless (assoc 'flyspell-mode minor-mode-alist)
  (add-to-list 'minor-mode-alist 
               '(flyspell-mode (:eval (my/ispell-dictionary-modeline)))))

;; Ensure flyspell does not shadow C-; so the global iedit binding can win
(with-eval-after-load 'flyspell
  (when (boundp 'flyspell-mode-map)
    (define-key flyspell-mode-map (kbd "C-;") nil))
  ;; defensive: remove from any prog-mode variant if present
  (when (boundp 'flyspell-prog-mode-map)
    (define-key flyspell-prog-mode-map (kbd "C-;") nil)))
