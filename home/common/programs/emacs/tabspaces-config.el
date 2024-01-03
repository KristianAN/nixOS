;; Tabspaces
(require 'tabspaces)
(setq
    tabspaces-use-filtered-buffers-as-default t
    tabspaces-default-tab "Default"
    tabspaces-remove-to-default t
    tabspaces-include-buffers '("*scratch*")
    tabspaces-initialize-project-with-todo t
    tabspaces-todo-file-name "project-todo.org"
)

;;Tabspaces sessions
(setq tabspaces-session t)
(setq tabspaces-session-auto-restore t)

