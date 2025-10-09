;;; eshell.el --- Eshell customizations -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package eshell
  :ensure nil
  :bind
  (
   ("C-c t e" . eshell)
   ("C-c t p" . 'custom/open-eshell-project-root-same-window)
   ("C-c t o" . 'custom/open-eshell-project-root-other-window)
   ))

(defun custom/open-eshell-project-root-same-window ()
  "Open Eshell in the project root in the current window."
  (interactive)
  (custom/open-eshell-project-root nil))

(defun custom/open-eshell-project-root-other-window ()
  "Open Eshell in the project root in another window."
  (interactive)
  (custom/open-eshell-project-root t))

(defun custom/open-eshell-project-root (other-window)
  "Open Eshell in the project root.
If OTHER-WINDOW is non-nil, open it in another window."
  (let ((project-root (vc-root-dir)))
    (if project-root
        (let ((default-directory project-root)
              (buffer-name (concat "eshell-" (file-name-nondirectory (directory-file-name project-root)))))
          (if other-window
              (custom/run-in-other-window
               (lambda ()
                 (if-let ((existing-buffer (get-buffer buffer-name)))
                     (switch-to-buffer existing-buffer)
                   (eshell t)
                   (rename-buffer buffer-name))))
            (if-let ((existing-buffer (get-buffer buffer-name)))
                (switch-to-buffer existing-buffer)
              (eshell t)
              (rename-buffer buffer-name))))
      (message "Not in a VCS directory."))))

(defun custom/run-in-other-window (func-to-run)
  "Execute FUNC-TO-RUN in another window.
If only one window is present, it splits the window.
Otherwise, it uses the next available window."
  (let ((target-window
         (if (one-window-p)
             (split-window-right)
           (next-window))))
    (with-selected-window target-window
      (funcall func-to-run))))
