(defvar *my-search-engines*
  (list
   '("duckduckgo" "https://duckdduckgo.com/search?q=~a" "https://www.duckduckgo.com")
   '("google" "https://google.com/search?q=~a" "https://google.com"))
  "List of search engines")

(define-configuration context-buffer
    "Go through the search engines above and make-search-engine out of them."
  ((search-engines
    (append %slot-default%
            (mapcar
             (lambda (engine) (apply 'make-search-engine engine))
             *my-search-engines*)))))

(define-configuration buffer
    ((default-modes
         (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration browser
    ((theme theme:+dark-theme+ :doc "Setting dark theme.
The default is theme:+light-theme+.")))

(define-configuration buffer
    ((prefers-color-scheme :dark)))
