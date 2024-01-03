;; git-gutter
(require 'git-gutter)
(add-hook 'prog-mode-hook 'git-gutter-mode)
(setq git-gutter:update-interval 0.05)

;; git-gutter-fringe
(defconst fringe-size '8 "Default fringe width.")

;;; Setting up the fringe
;; switches order of fringe and margin
;;(setq-default fringes-outside-margins t)

;; standardize fringe width
(fringe-mode fringe-size)
(push `(left-fringe  . ,fringe-size) default-frame-alist)
(push `(right-fringe . ,fringe-size) default-frame-alist)

;; colored fringe "bars"
(define-fringe-bitmap 'git-gutter-fr:added
	  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:modified
	  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:deleted
	  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
	  nil nil 'center)

