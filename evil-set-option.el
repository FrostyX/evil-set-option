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
      ;; There is a pre-existing implementation of the :set command used for
      ;; setting the initial evil state for a buffer. If the value is
      ;; recognized to be one of the states, fall back to the default
      ;; implementation
      ((or "normal" "insert" "visual" "replace" "operator" "motion")
       (evil-set-option-initial-state option))

      ;; Indent options
      ("expandtab" (evil-set-option-expandtab t))
      ("noexpandtab" (evil-set-option-expandtab nil))
      ("shiftwidth" (evil-set-option-shiftwidth value))
      ("autoindent" (evil-set-option-autoindent t))
      ("noautoindent" (evil-set-option-autoindent nil))
      ("tabstop" (evil-set-option-tabstop value))

      ;; Search options
      ("hlsearch" (evil-set-option-hlsearch t))
      ("nohlsearch" (evil-set-option-hlsearch nil))
      ("ignorecase" (evil-set-option-ignorecase t))
      ("noignorecase" (evil-set-option-ignorecase nil))
      ("incsearch" (evil-set-option-incsearch t))
      ("noincsearch" (evil-set-option-incsearch nil))

      ;; Text rendering options
      ("wrap" (evil-set-option-wrap t))
      ("nowrap" (evil-set-option-wrap nil))

      ;; User interface options
      ("number" (evil-set-option-number t))
      ("nonumber" (evil-set-option-number nil))
      ("colorcolumn" (evil-set-option-colorcolumn value))

      ;; Unknown command
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

(defun evil-set-option-expandtab (value)
  ;; FIXME Using tabs for >> and << doesn't work
  (setq indent-tabs-mode (not value))
  (setq evil-indent-convert-tabs value))

(defun evil-set-option-shiftwidth (value)
  (setq evil-shift-width (string-to-number value)))

(defun evil-set-option-tabstop (value)
  (setq tab-width (string-to-number value)))

(defun evil-set-option-autoindent (value)
  (setq evil-auto-indent value)
  (define-key global-map (kbd "RET")
    (if value 'newline-and-indent 'newline)))
