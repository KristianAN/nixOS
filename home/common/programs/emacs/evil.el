(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  ;; Keybind-Prefixes
  (define-prefix-command 'meow-prefix-p-map)
  (define-prefix-command 'meow-prefix-t-map)
  (define-prefix-command 'meow-prefix-find)
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '(";" . meow-comment)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("b" . consult-project-buffer)
   '("p" . meow-prefix-p-map)
   '("t" . meow-prefix-t-map)
   '("f" . meow-prefix-find)
   '("o" . dirvish-dwim)
   '("w" . save-buffer)
   '("r" . query-replace)
   )
  (dolist (binding '(("s" . tabspaces-switch-or-create-workspace)
                     ("d" . tabspaces-close-workspace)
                     ("o" . tabspaces-open-or-create-project-and-workspace)
                     ("f" . project-find-file)))
    (define-key meow-prefix-p-map (kbd (car binding)) (cdr binding)))
  (dolist (binding '(("g" . consult-ripgrep)
                     ("f" . consult-find)
                     ("l" . consult-line)
                     ("L" . consult-line-multi)
                     ("v" . kristian/consult-ripgrep-from-visual-selection)))
    (define-key meow-prefix-find (kbd (car binding)) (cdr binding)))
  (dolist (binding '(("p" . eat-project-other-window)
                     ("n" . eat)
                     ("b" . kristian/consult-grep-project-buffer-term)
                     ("c" . kristian/my-toggle-theme)))
    (define-key meow-prefix-t-map (kbd (car binding)) (cdr binding)))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))


(use-package meow
  :custom
                                        ;(meow-use-cursor-position-hack t)
  (meow-use-clipboard t)
  (meow-goto-line-function 'consult-goto-line)
  :config
  ;; set colors in theme
  (setq meow-use-dynamic-face-color nil)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  (setq meow--kbd-delete-char "<deletechar>")
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (add-to-list 'meow-mode-state-list '(eat-mode . insert))
  ;; start helpful in normal
  ;; (add-to-list 'meow-mode-state-list '(helpful-mode . normal))
  (meow-global-mode 1)
  (meow-setup))
