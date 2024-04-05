;;; font-and-theme --- Configuration for font and theme.
;;; Commentary:
;;; Code:
(require-theme 'modus-themes)

(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t)
;; Maybe define some palette overrides, such as by using our presets
(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-warmer)

(defvar my-current-theme 'modus-operandi-tinted)

(defun my-toggle-modus-theme ()
  (interactive)
  (if (display-graphic-p)
      (if (eq my-current-theme 'modus-operandi-tinted)
          (progn
            (load-theme 'modus-vivendi-tinted t)
            (setq my-current-theme 'modus-vivendi-tinted))
        (progn
          (load-theme 'modus-operandi-tinted t)
          (setq my-current-theme 'modus-operandi-tinted)))
    (message "Cannot toggle theme in non-graphical frame")))

(add-hook 'after-make-frame-functions
  (lambda (frame)
    (with-selected-frame frame
      ;; All customizations here
      (load-theme 'modus-operandi-tinted)
      (set-frame-font "Iosevka Nerd Font 12" nil t)
      )))

;;; font-and-theme.el ends here
