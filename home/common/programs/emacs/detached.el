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
           (detached-terminal-data-command system-type)))

