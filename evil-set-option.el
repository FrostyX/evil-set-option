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
(evil-define-command setoption (arg)
  (interactive "<a>")

  (if (not arg)
      (error "Missing argument, use e.g. :set wrap"))

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
      ("relativenumber" (evil-set-option-relativenumber t))
      ("norelativenumber" (evil-set-option-relativenumber nil))
      ("colorcolumn" (evil-set-option-colorcolumn value))
      ("cursorline" (evil-set-option-cursorline t))
      ("nocursorline" (evil-set-option-cursorline nil))

      ;; Code folding options
      ("foldenable" (evil-set-option-foldenable t))
      ("nofoldenable" (evil-set-option-foldenable nil))

      ;; Unknown command
      (option (error "Unknown command")))))

(evil-ex-define-cmd "set" 'setoption)
(evil-ex-define-cmd "setoption" 'setoption)

(defun evil-set-option-initial-state (value)
  (evil-set-initial-state major-mode (intern value)))

(defun evil-set-option-wrap (value)
  (setq truncate-lines value))

(defun evil-set-option-number (value)
  (display-line-numbers-mode (if value 1 0))
  (setq display-line-numbers value))

(defun evil-set-option-relativenumber (value)
  (setq display-line-numbers (if value 'relative t)))

(defun evil-set-option-colorcolumn (value)
  (if (not (string-empty-p value))
      (progn
        (turn-on-fci-mode)
        (set-fill-column (string-to-number value)))
    (turn-off-fci-mode)))

(defun evil-set-option-hlsearch (value)
  ;; In Vim, this option is global, therefore we should ideally use
  ;;     (global-evil-search-highlight-persist t)
  ;; But IMHO it doesn't work. Thus, I am implementing this option per-buffer
  (if (fboundp 'evil-search-highlight-persist)
      (evil-search-highlight-persist (if value t -1))
    (error "Install `evil-search-highlight-persist' package")))

(defun evil-set-option-ignorecase (value)
  ;; FIXME Nothing works for not-ignoring case
  (setq case-fold-search nil)
  (setq case-fold-search t)
  (setq evil-ex-search-case nil)
  (setq isearch-case-fold-search t)
  (setq evil-search-module 'evil-search))

(defun evil-set-option-incsearch (value)
  (if (not value)
      (error "There is no way to disable incsearch")))

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
  (local-set-key (kbd "RET")
    (if value 'newline-and-indent 'newline)))

(defun evil-set-option-cursorline (value)
    (if value
        (hl-line-mode t)
      (hl-line-unload-function)))

(defun evil-set-option-foldenable (value)
  (hs-minor-mode value)
  (if value
      (hs-hide-all)
    (hs-show-all)))

;; TODO This hook after-change-major-mode-hook should execute evil-vimrc
;; (or evilrc) function
