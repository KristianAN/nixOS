;;; detatched.el --- Dtach processes -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package detached
  :init
  (detached-init)
  :bind
  (;; Replace `async-shell-command' with `detached-shell-command'
   ([remap async-shell-command] . detached-shell-command)
   ;; Replace `compile' with `detached-compile'
   ([remap compile] . detached-compile)
   ([remap recompile] . detached-compile-recompile)
   ;; Replace built in completion of sessions with `consult'
   ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-shell-program "bash")
           (detached-terminal-data-command system-type)))


(defun kristian/detached-run-command-git-root (command)
  "Run COMMAND in the project root directory using detached-shell-command."
  (interactive "sCommand: ")
  (let* ((root-dir (locate-dominating-file default-directory ".git"))
         (default-directory (or root-dir default-directory)))
    (detached-shell-command command)))
  
(defun kristian/detached-run-command-git-root-with-path (path-and-command)
  "Run command in the project root directory using detached-shell-command.
  PATH-AND-COMMAND should be in format 'path command', e.g. 'modules/app npm run dev'. 
The first argument is treated as the relative path from git root, the rest as the command."
  (interactive "sPath and Command: ")
  (let* ((parts (split-string path-and-command " " t))
         (path (car parts))
         (command (string-join (cdr parts) " "))
         (root-dir (locate-dominating-file default-directory ".git"))
         (full-path (and root-dir (expand-file-name path root-dir)))
         (default-directory (or full-path default-directory)))
    (detached-shell-command command)))
