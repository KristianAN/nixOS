;;; indent-config --- Configuration for indentation guides
;;; Commentary:
;;; Code:
(dolist (indent-mode-langs
         '(
           haskell-ts
           scala-ts-mode-hook
           eglot-java-mode-hook
           clojure-mode-hook
           ))
(add-hook indent-mode-langs 'indent-bars-mode))

;;; Indent-guides
(require 'indent-bars)
(setq
    indent-bars-color '(highlight :face-bg t :blend 0.3)
    indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
    indent-bars-width-frac 0.35
    indent-bars-pad-frac 0.2
    indent-bars-highlight-current-depth nil)
;;; indent-config.el ends here
