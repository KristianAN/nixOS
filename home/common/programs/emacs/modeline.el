;;; modeline.el --- Emacs modeline -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package mood-line

  ;; Enable mood-line
  :config
  (mood-line-mode)

  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

