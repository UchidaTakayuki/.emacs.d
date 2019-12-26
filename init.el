;; Set the environment to Japanese, UTF-8
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems' utf-8)
(prefer-coding-system 'utf-8)

;; show line number
(global-display-line-numbers-mode)

;; Do not display startup message
(setq inhibit-startup-message t)

;; Do not make backup file
(setq make-backup-files nil)

;; Delete auto save file when finished
(setq delete-auto-save-files t)

;; use spaces in tabs
(setq-default tab-width 4 indent-tabs-mode nil)

;; make window transparent
;; Active window / Inactive window (alpha value specifies transparency)
(add-to-list 'default-frame-alist' (alpha. (0.85 0.85)))

;; Remove the menu bar
(menu-bar-mode -1)

;; Remove toolbar
(tool-bar-mode -1)

;; display the number of columns
(column-number-mode t)

;; Stop blinking the cursor
(blink-cursor-mode 0)

;; Highlight cursor line
(global-hl-line-mode t)

;; flash the corresponding parenthesis
(show-paren-mode 1)

;; Visualize spaces, tabs, etc.
(global-whitespace-mode 1)

;; scroll line by line
(setq scroll-conservatively 1)

;; straight.el setting
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; evil.el setting
(straight-use-package 'evil)
(evil-mode 1)

;; color theme setting
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)

;; all-the-icons.el setting
(straight-use-package 'all-the-icons)
