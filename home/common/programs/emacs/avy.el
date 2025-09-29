;;; avy.el --- Avy configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package avy
  :ensure t
  :bind (("C-c a" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-:" . avy-isearch))
  :config
  (defun avy-action-kill-whole-line (pt)
    "avy action: kill the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    "avy action: copy the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    "avy action: copy the line where avy selection is and paste to current point"
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    "avy action: kill the line where avy selection is and paste to current point"
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-helpful (pt)
    "avy action: get helpful information at point"
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    ;; (select-window
    ;;  (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-mark-to-char (pt)
    "avy action: mark from current point to avy selection"
    (activate-mark)
    (goto-char pt))

  (defun avy-action-flyspell (pt)
    "avy action: flyspell the word where avy selection is"
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-correct-wrapper))))

  (defun avy-action-define (pt)
    "avy action: define the word in dictionary where avy selection is"
    (save-excursion
      (goto-char pt)
      (fanyi-dwim2)))

  (defun avy-action-embark (pt)
    "avy action: embark where avy selection is"
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-duckduckgo (pt)
    "avy action: duckduckgo the avy selection when it is a word or browse it when it is a link"
    (save-excursion
      (goto-char pt)
      (custom/duckduckgo-search-eww)))

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?H avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell
        (alist-get ?= avy-dispatch-alist) 'avy-action-define
        (alist-get ?. avy-dispatch-alist) 'avy-action-embark
        (alist-get ?G avy-dispatch-alist) 'avy-action-duckduckgo
        )

  :custom
  ;; (avy-case-fold-search t)              ; default is t
  (avy-timeout-seconds 0.5)
  (avy-all-windows t)
  (avy-background nil)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?q ?e ?r ?u ?i ?p ?n))
  )
