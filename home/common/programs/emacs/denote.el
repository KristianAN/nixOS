;;; denote.el --- Configuration for denote -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("M-n n" . denote)
   ("M-n r" . denote-rename-file)
   ("M-n l" . denote-link)
   ("M-n b" . denote-backlinks)
   ("M-n d" . denote-dired)
   ("M-n g" . denote-config))

  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-save-buffers t)
  ;; Example of template setup
  ;; (setq denote-templates
  ;;       `((report . "* Some heading\n\n* Another heading")
  ;;         (memo . ,(concat "* Some heading"
  ;;                          "\n\n"
  ;;                          "* Another heading"
  ;;                          "\n\n"))))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))
