;;; magit.el --- magit -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package magit
  :bind (:map magit-mode-map
              (";" . magit-discard))
  )

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo"))
)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b11100000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11100000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [#b10000000
     #b11000000
     #b11100000
     #b11110000] nil nil 'bottom)
  )

