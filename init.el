;; show line number
(global-display-line-numbers-mode 1)
(custom-set-variables '(display-line-numbers-width-start t))

;; not create backup file setting
(setq make-backup-files nil)
(setq auto-save-default nil)

;; truncate setting
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

;; Remove scroll bar
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
(straight-use-package 'use-package)

;; evil.el setting
;; (straight-use-package 'evil)
;; (evil-mode 1)

;; color theme setting
(straight-use-package 'madhat2r-theme)
(load-theme 'madhat2r t)

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

;; window resize change key bind
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key (kbd "C-=") 'enlarge-window)
(global-set-key (kbd "C--") 'shrink-window)

;; icons setting
(straight-use-package 'all-the-icons)

;; company setting
(straight-use-package 'company)
(use-package company
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))
(setq company-transformers '(company-sort-by-backend-importance))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(setq company-selection-wrap-around t)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "C-f") 'company-complete-selection)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;; company-box setting
(straight-use-package 'company-box)
(use-package company-box
  :hook (company-mode . company-box-mode))

;; git-gitter setting
(straight-use-package 'git-gutter)
(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:update-interval 2))

;; aspell setting
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(straight-use-package 'flyspell)
(add-hook 'find-file-hook 'flyspell-mode)
(add-hook 'find-file-hook 'flyspell-buffer)
(when (eq system-type 'darwin)
  (setq ispell-program-name "/usr/local/bin/ispell"))

;; whitespace setting
(require 'whitespace)
(setq whitespace-style '(
    face
    trailing
    tabs
    space-mark
    tab-mark
))

(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(global-whitespace-mode 1)

;; tab setting
(straight-use-package 'tabbar)
(tabbar-mode)
(tabbar-mwheel-mode nil)
(setq tabbar-buffer-groups-function nil)
(setq tabbar-use-images nil)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-n") 'tabbar-forward-tab)
(global-set-key (kbd "C-z C-p") 'tabbar-backward-tab)

(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
(setq tabbar-separator '(2.0))

(when window-system
  (set-face-attribute
   'tabbar-default nil
   :family "MeiryoKe_Gothic"
   :background "#34495E"
   :foreground "#EEEEEE"
   :height 0.85
   )
  (set-face-attribute
   'tabbar-unselected nil
   :background "#34495E"
   :foreground "#EEEEEE"
   :box nil
  )
  (set-face-attribute
   'tabbar-modified nil
   :background "#E67E22"
   :foreground "#EEEEEE"
   :box nil
  )
  (set-face-attribute
   'tabbar-selected nil
   :background "#E74C3C"
   :foreground "#EEEEEE"
   :box nil)
  (set-face-attribute
   'tabbar-button nil
   :box nil)
  (set-face-attribute
   'tabbar-separator nil
   :height 2.0)
)
(put 'set-goal-column 'disabled nil)
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ((eq (current-buffer) b) b)
                     ((equal "*scratch*" (buffer-name b)) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((char-equal ?* (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; paren setting
(use-package paren
   :ensure nil
   :hook
   (after-init . show-paren-mode)
   :custom-face
   (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
   :custom
   (show-paren-style 'mixed)
   (show-paren-when-point-inside-paren t)
   (show-paren-when-point-in-periphery t))

;; rainbow-deliters setting
(straight-use-package 'rainbow-delimiters)
(use-package rainbow-delimiters
   :hook
   (prog-mode . rainbow-delimiters-mode))

;; undohist setting
(straight-use-package 'undohist)
(use-package undohist)
(undohist-initialize)
(setq undohist-directory "~/.emacs.d/undohist")

;; undo-tree setting
(straight-use-package 'undo-tree)
(use-package undo-tree)
(global-undo-tree-mode)

;; icons setting
(straight-use-package 'all-the-icons)
(use-package all-the-icons)

;; page-break-lines setting
(straight-use-package 'page-break-lines)
(use-package page-break-lines)
(turn-on-page-break-lines-mode)

;; projectile setting
(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; dashboard setting
(straight-use-package 'dashboard)
(use-package dashboard
    :diminish
    (dashboard-mode page-break-lines-mode)
    :custom
    (dashboard-items '((recents . 15)
               (projects . 5)))
    :hook
    (after-init . dashboard-setup-startup-hook)
    :config
    )
(add-hook 'dashboard-mode-hook (lambda (&optional dummy) (display-line-numbers-mode -1)))

;; neotree setting
(straight-use-package 'neotree)
(use-package neotree
  :init
  (setq-default neo-keymap-style 'concise)
  :after
  projectile
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (bind-key "C-q" 'neotree-toggle)
  (bind-key "C-c d" 'neotree-delete-node)
  (bind-key "C-c n" 'neotree-create-node)
  (bind-key "\C-c r" 'neotree-rename-node)
  (bind-key "a" 'neotree-hidden-file-toggle neotree-mode-map))
(add-hook 'neo-after-create-hook (lambda (&optional dummy) (display-line-numbers-mode -1)))

;; swiper setting
(straight-use-package 'swiper)
(use-package swiper)
(global-set-key "\C-s" 'swiper)
(setq swiper-include-line-number-in-search t)

;; ivy setting
(straight-use-package 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 30)
(setq ivy-extra-directories nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; ivy-rich setting
(straight-use-package 'ivy-rich)
(use-package ivy-rich)

;; counsel setting
(straight-use-package 'counsel)
(counsel-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))

;; counsel-projectile setting
(straight-use-package 'counsel-projectile)
(use-package counsel-projectile)

;; lsp setting
(straight-use-package 'lsp-mode)
(straight-use-package 'company-lsp)
(straight-use-package 'lsp-ui)
(use-package lsp-mode
  :commands lsp)
(use-package company-lsp)
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; php develop setting
(straight-use-package 'php-mode)

;; python develop setting
(straight-use-package 'python-mode)
(use-package python-mode
  :config
  (add-hook 'python-mode-hook #'lsp))

;; haskell develop setting
(straight-use-package 'haskell-mode)

;; rust develop setting
(straight-use-package 'rust-mode)

;; web develop setting
(straight-use-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))
