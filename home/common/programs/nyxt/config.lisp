(defvar *my-search-engines*
  (list
   '("duckduckgo" "https://duckduckgo.com/search?q=~a" "https://www.duckduckgo.com")
   '("google" "https://google.com/search?q=~a" "https://google.com"))
  "List of search engines")

(define-configuration context-buffer
    ((search-engines
      (append
       (mapcar (lambda (engine) (apply 'make-search-engine engine)) *my-search-engines*)
       %slot-default%))
     (default-search-engine "duckduckgo")))

(define-configuration buffer
    ((default-modes
         (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration browser
    ((theme theme:+dark-theme+ :doc "Setting dark theme.
The default is theme:+light-theme+.")))

(define-configuration (web-buffer)
    ((default-modes
         (pushnew 'nyxt/mode/reduce-tracking:reduce-tracking-mode %slot-value%))))

(define-configuration (web-buffer)
    ((default-modes (pushnew 'nyxt/mode/no-script:no-script-mode %slot-value%))))

(define-configuration (web-buffer)
    ((default-modes (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

(define-configuration (web-buffer)
    ((default-modes
         (pushnew 'nyxt/mode/reduce-tracking:reduce-tracking-mode %slot-value%))))

(define-configuration (web-buffer)
    ((default-modes (pushnew 'nyxt/mode/no-script:no-script-mode %slot-value%))))

(define-configuration (web-buffer)
    ((default-modes (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))
