;; show line number
(global-display-line-numbers-mode)

;; not create backup file setting
(setq make-backup-files nil)
(setq auto-save-default nil)

;; trancate setting
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; Do not display startup message
(setq inhibit-startup-message t)

;; Do not make backup file
(setq make-backup-files nil)

;; Delete auto save file when finished
(setq delete-auto-save-files t)

;; use spaces in tabs
(setq-default tab-width 4 indent-tabs-mode nil)

;; window move key setting
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;; make window transparent
;; Active window / Inactive window (alpha value specifies transparency)
(add-to-list 'default-frame-alist' (alpha. (0.85 0.85)))

;; Remove the menu bar
(menu-bar-mode -1)

;; mouse setting
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; Remove toolbar
(tool-bar-mode -1)

;; Rmove scrollbar
(scroll-bar-mode -1)

;; display the number of columns
(column-number-mode t)

;; Stop blinking the cursor
(blink-cursor-mode 0)

;; Highlight cursor line
(global-hl-line-mode t)

;; flash the corresponding parenthesis
(show-paren-mode 1)

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
;; (straight-use-package 'evil)
;; (evil-mode 1)

;; color theme setting
(straight-use-package 'madhat2r-theme)
(load-theme 'madhat2r t)

;; all-the-icons.el setting
(straight-use-package 'all-the-icons)

;; bind-key setting
(straight-use-package 'bind-key)

;; character setting
;; add to 
;; need packege [emacs-mozc-bin],[emacs-mozc-bin]
(straight-use-package 'mozc)
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-input-method "japanese-mozc")
(setq mozc-candidate-style 'overlay)
(global-set-key [?\C-\ ] 'toggle-input-method)

;; indent guid setting
(straight-use-package 'indent-guide)
(indent-guide-global-mode)
(set-face-foreground 'indent-guide-face "cyan")
(setq indent-guide-recursive t)

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; recentf setting
(require 'recentf)
(straight-use-package 'recentf-ext)
(when (require 'recentf-ext nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '("~/.emacs.d/.recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))

(setq inhibit-startup-message t)
(add-hook 'after-init-hook (lambda()
    (recentf-open-files)
    ))

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; duplicate-thing setting
(straight-use-package 'duplicate-thing)
(bind-key "M-c" 'duplicate-thing)

;; mode line setting
(straight-use-package 'smart-mode-line)
(defvar sml/no-confirm-load-theme t)
(defvar sml/theme 'dark)
(defvar sml/shorten-directory -1)
(sml/setup)
(straight-use-package 'diminish)
(column-number-mode t)
(line-number-mode t)

;; which-key setting
(straight-use-package 'which-key)
(which-key-mode)

;; multiple-cursors setting
(straight-use-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; defun delete line
(defun delete-current-line ()
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))
(global-set-key (kbd "M-k") 'delete-current-line)
