;;; present.el --- Presentations in emacs, really! -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(use-package org-present
  :ensure t
  :config
  (defun my/org-present-start ()
    ;; Tweak font sizes
    (setq-local face-remapping-alist '((default (:height 2.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))

    ;; Set a blank header line string to create blank space at the top
    (setq header-line-format " ")

    ;; Display inline images automatically
    (org-display-inline-images)

    ;; turn off line numbers
    (display-line-numbers-mode 0)

    ;; Center the presentation and wrap lines
    (visual-fill-column-mode 1)
    )
  
  (defun my/org-present-end ()
    ;; Reset font customisation
    (setq-local face-remapping-alist '((default variable-pitch default)))

    ;; Clear the header line string so that it isn't displayed
    (setq header-line-format nil)

    ;; Stop displaying inline images
    (org-remove-inline-images)
    
    ;; turn on line numbers
    (display-line-numbers-mode 1)

    ;; Stop centering the document
    (visual-fill-column-mode 0)
    )
  
  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)
    ;; Unfold the current entry
    (org-show-entry)
    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children))
  
  :hook
  ((org-present-mode . my/org-present-start)
   (org-present-mode-quit . my/org-present-end)))
;;   (org-present-after-navigate-functions . my/org-present-prepare-slide)))
