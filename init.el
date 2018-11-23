;;; init.el --- Initialization File For Root User

;; Copyright (C) 2017-2018 Mx Reimu

;; Author: MX Reimu <https://github.com/re-mx>

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

;; This is my simple Emacs config for root user.

;;; Code:

(setq gc-cons-threshold 33554432)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))

(setq custom-file (expand-file-name ".emacs-custom.el" user-emacs-directory))

(require 'cl-lib)

(require 'package)

;;; Standard elpa repositories
(add-to-list 'package-archives '("melpa" . "https://elpa.emacs-china.org/melpa/") t)

;;; On-demand installation of packages
(defun require-package (package)
  "Install PACKAGE if it's not installed."
  (or (package-installed-p package)
      (if (assoc package package-archive-contents)
          (package-install package)
        (progn
          (package-refresh-contents)
          (package-install package)))))

(defun maybe-require-package (package)
  "Try to install PACKAGE, and return non-nil if successful or PACKAGE exists.
In the event of failure, return nil and print a warning message."
  (condition-case err
      (require-package package)
    (error
     (message "Failed to install optional package `%s': %S" package err)
     nil)))

(setq package-enable-at-startup nil)
(package-initialize)

(require-package 'wgrep)
(require-package 'diminish)

(xterm-mouse-mode 1)

(setq custom-enabled-themes '(wombat))

(defun remx/reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
(add-hook 'after-init-hook 'remx/reapply-themes)

(setq inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(maybe-require-package 'disable-mouse)

(setq dired-dwim-target t)

(when (maybe-require-package 'anzu)
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur) ;; to match ivy conventions
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defun remx/isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map (kbd "C-M-w") 'remx/isearch-yank-symbol)

(defun remx/isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'remx/isearch-exit-other-end)

(setq grep-highlight-matches t
      grep-scroll-output t)

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq ag-highlight-search t))

(when (executable-find "rg")
  (require-package 'rg))

(require 'uniquify)
(setq uniquify-ignore-buffers-re "^\\*")

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(when (maybe-require-package 'smex)
  (setq-default smex-history-length 32
                smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (with-eval-after-load 'ivy
    (diminish 'ivy-mode))
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist
        '((man . "^")
          (woman . "^"))))

(when (maybe-require-package 'ivy-historian)
  (add-hook 'after-init-hook (lambda () (ivy-historian-mode t))))

(when (maybe-require-package 'counsel)
  (add-hook 'after-init-hook 'counsel-mode)
  (with-eval-after-load 'counsel
    (diminish 'counsel-mode))
  (setq counsel-mode-override-describe-bindings t)

  (when (executable-find "ag")
    (global-set-key (kbd "M-s / a") 'counsel-ag))
  (when (executable-find "rg")
    (global-set-key (kbd "M-s / r") 'counsel-rg)))

(when (maybe-require-package 'swiper)
  (with-eval-after-load 'ivy
    (defun remx/swiper-at-point (sym)
      "Use `swiper' to search for the symbol at point."
      (interactive (list (thing-at-point 'symbol)))
      (swiper sym))

    (define-key ivy-mode-map (kbd "M-s s") 'remx/swiper-at-point)))

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(global-set-key (kbd "M-/") 'hippie-expand)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode))

(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))

(setq-default
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position t
 truncate-lines nil
 truncate-partial-width-windows nil)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

(global-set-key (kbd "RET") 'newline-and-indent)

(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)

(require-package 'unfill)

(when (maybe-require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(require-package 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (diminish 'undo-tree-mode))

(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))

(add-hook 'after-init-hook 'show-paren-mode)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(defun remx/sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun remx/delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun remx/rename-current-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
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

(defun remx/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

(setq-default show-trailing-whitespace t)

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(defun remx/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'remx/no-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)

(global-set-key [remap just-one-space] 'cycle-spacing)

(when (maybe-require-package 'cmake-mode)
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode)))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-to-list 'auto-mode-alist '("Portfile\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("Procfile" . conf-mode))
(add-to-list 'auto-mode-alist '("hostname" . conf-space-mode))
(add-to-list 'auto-mode-alist '("hosts" . conf-mode))
(add-to-list 'auto-mode-alist '("\\LICENSE\\'" . text-mode))

(when (file-exists-p custom-file) (load custom-file))

(defun remx/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun remx/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (remx/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (remx/utf8-locale-p (getenv "LC_ALL"))
      (remx/utf8-locale-p (getenv "LC_CTYPE"))
      (remx/utf8-locale-p (getenv "LANG"))))

(when (or window-system (remx/locale-is-utf8-p))
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; init.el ends here
