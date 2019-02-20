;;; init.el --- Emacs Config for Root User

;; Copyright (C) 2019 Yuri MX

;; Author: Yuri MX <https://github.com/yurimx>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Emacs configuration for root user

;;; Code:

(setq gc-cons-threshold 33554432)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))

(require 'cl-lib)

;;; ELPA
(require 'package)

;; Standard elpa repositories
(add-to-list 'package-archives '("melpa" . "https://elpa.emacs-china.org/melpa/"))

;; On-demand installation of packages
(defun require-package (package)
  "Install PACKAGE if it's not installed."
  (or (package-installed-p package)
      (if (assoc package package-archive-contents)
          (package-install package)
        (progn
          (package-refresh-contents)
          (package-install package)))))

(setq package-enable-at-startup nil)
(package-initialize)

(require-package 'wgrep)
(require-package 'diminish)

;;; Core
;; Coding system
(defun yuri/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun yuri/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (yuri/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (yuri/utf8-locale-p (getenv "LC_ALL"))
      (yuri/utf8-locale-p (getenv "LC_CTYPE"))
      (yuri/utf8-locale-p (getenv "LANG"))))

(when (yuri/locale-is-utf8-p)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (setq locale-coding-system 'utf-8))

;; Some basic preferences
(setq-default
 bookmark-default-file (expand-file-name ".bookmarks" user-emacs-directory)
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position t)

;; When Emacs asks "yes" or "no", use "y" or "n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Revert buffers for changed files
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Make Dired try to guess the default target directory
(setq dired-dwim-target t)

;; Support for jumping to the beginning of search ring when exiting isearch
(defun yuri/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'yuri/isearch-exit-other-end)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur) ; to match ivy conventions

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; Setup a menu of recently opened files
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))

;; Save minibuffer history
(setq history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

;; Automatically save place in files
(add-hook 'after-init-hook 'save-place-mode)

;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

(require-package 'disable-mouse)

;; Nicer naming of buffers for files with identical names
(require 'uniquify)
(setq uniquify-ignore-buffers-re "^\\*")

(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)

;; Display ugly ^L page breaks as tidy horizontal lines
(require-package 'page-break-lines)
(add-hook 'after-init-hook 'global-page-break-lines-mode)
(with-eval-after-load 'page-break-lines
  (diminish 'page-break-lines-mode))

;; Highlight trailing whitespace
(defun yuri/show-trailing-whitespace ()
  "Highlight trailing whitespace."
  (setq show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'yuri/show-trailing-whitespace))

;;; UI
(setq inhibit-startup-screen t)

;; Window features
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Theme
(setq custom-enabled-themes '(wombat))

;; Ensure that themes will be loaded even if they have not been customized
(defun yuri/load-themes ()
  "Load and enable the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'yuri/load-themes)

;;; Editing
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(add-hook 'after-init-hook 'transient-mark-mode)

(require-package 'anzu)
(add-hook 'after-init-hook 'global-anzu-mode)
(setq anzu-mode-lighter "")
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

(require-package 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require-package 'symbol-overlay)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'symbol-overlay-mode))
(with-eval-after-load 'symbol-overlay
  (diminish 'symbol-overlay-mode)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)

(global-set-key [remap just-one-space] 'cycle-spacing)
(global-set-key (kbd "RET") 'newline-and-indent)

;;; Completion
(setq tab-always-indent 'complete)

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(require-package 'company)
(add-hook 'after-init-hook 'global-company)

;; Ivy
(require-package 'ivy)
(add-hook 'after-init-hook 'ivy-mode)
(with-eval-after-load 'ivy
  (diminish 'ivy-mode))
(setq ivy-use-virtual-buffers t
      ivy-virtual-abbreviate 'full
      ivy-initial-inputs-alist
      '((Man-completion-table . "^")
        (woman . "^")))

;; Counsel
(require-package 'counsel)
(add-hook 'after-init-hook 'counsel-mode)
(with-eval-after-load 'counsel
  (diminish 'counsel-mode))
(setq counsel-mode-override-describe-bindings t)

(cond
 ((executable-find "rg")
  (global-set-key (kbd "C-c g") 'counsel-rg))
 ((executable-find "ag")
  (global-set-key (kbd "C-c g") 'counsel-ag)))

;; Swiper
(require-package 'swiper)
(with-eval-after-load 'ivy
  (define-key ivy-mode-map (kbd "M-s s") 'swiper))

;;; Utils
(defun yuri/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun yuri/delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun yuri/rename-current-file-and-buffer (new-name)
  "Rename the current file and associated buffer to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun yuri/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;; Syntax checking
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;;; Misc
(add-hook 'after-init-hook 'xterm-mouse-mode)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-to-list 'auto-mode-alist '("\\LICENSE\\'" . text-mode))
(add-to-list 'auto-mode-alist '("^Portfile\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("^Procfile\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("hostname" . conf-space-mode))
(add-to-list 'auto-mode-alist '("hosts" . conf-mode))

(require-package 'lua-mode)
(require-package 'dotenv-mode)

;;; Variables configured via the interactive 'customize' interface
(setq custom-file (expand-file-name ".emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
