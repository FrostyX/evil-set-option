;; My primitive implementation of :set command for Evil.
;; It allows you to run e.g.
;;    :set wrap
;;    :set colorcolumn=80
;; etc, that we know from Vim.
;;
;; Currently, only a very limited subset of options is supported - only
;; those that I use regularly.
;;
;; Options manual - http://vimdoc.sourceforge.net/htmldoc/options.html
;; Let's start with some popular options
;; https://www.shortcutfoo.com/blog/top-50-vim-configuration-options/
(evil-define-command frostyx/set (arg)
  (interactive "<a>")

  (let* ((split (split-string arg "="))
         (option (car split))
         (value (car (cdr split))))

    (pcase option
      ((or "normal" "insert" "visual" "replace" "operator" "motion")
       (evil-set-option-initial-state option))
      ("wrap" (evil-set-option-wrap t))
      ("nowrap" (evil-set-option-wrap nil))
      ("number" (evil-set-option-number t))
      ("nonumber" (evil-set-option-number nil))
      ("colorcolumn" (evil-set-option-colorcolumn value))
      ("hlsearch" (evil-set-option-hlsearch t))
      ("nohlsearch" (evil-set-option-hlsearch nil))
      ("ignorecase" (evil-set-option-ignorecase t))
      ("noignorecase" (evil-set-option-ignorecase nil))
      ("incsearch" (evil-set-option-incsearch t))
      ("noincsearch" (evil-set-option-incsearch nil))
      (option (print "Unknown command")))))

(evil-ex-define-cmd "frostyx/set" 'frostyx/set)

(defun evil-set-option-initial-state (value)
  (print "Set initial state"))

(defun evil-set-option-wrap (value)
  (setq truncate-lines value))

(defun evil-set-option-number (value)
  (setq display-line-numbers value))

(defun evil-set-option-colorcolumn (value)
  (if (not (string-empty-p value))
      (progn
        (turn-on-fci-mode)
        (set-fill-column (string-to-number value)))
    (turn-off-fci-mode)))

(defun evil-set-option-hlsearch (value)
  ;; Requires https://melpa.org/#/evil-search-highlight-persist
  ;; TODO Add check that the package is available
  (global-evil-search-highlight-persist t)
  (if value
      (turn-on-search-highlight-persist)
    (turn-off-search-highlight-persist)))

(defun evil-set-option-ignorecase (value)
  ;; FIXME Nothing works for not-ignoring case
  (setq case-fold-search nil)
  (setq case-fold-search t)
  (setq evil-ex-search-case nil)
  (setq isearch-case-fold-search t)
  (setq evil-search-module 'evil-search))

(defun evil-set-option-incsearch (value)
  ;; TODO Error only for disabling
  (print "There is no way to disable incsearch"))


evil-default-state
