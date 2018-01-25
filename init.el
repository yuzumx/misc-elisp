;;; init.el --- ReimuXMX's root Emacs Configuration

;; Copyright (c) 2008-2017 ReimuXMX in BLF Club

;; Author: MX Reimu <yuan_pre@outlook.com>
;; URL: https://gitee.com/ReimuXMX/XMXAConfig
;; Package-Requires: ((emacs "25.0"))
;; Version 1.0.0

;; This file is not part of GNU Emacs

;;; Commentary:

;; This file is a simply configuration for root user.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst reimuxmx-initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold reimuxmx-initial-gc-cons-threshold)))

(setq custom-file (expand-file-name ".emacs-custom.el" user-emacs-directory))

(require 'cl-lib)

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun reimuxmx/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(require 'package)

;;; Standard elpa repositories
(add-to-list 'package-archives '("melpa" . "https://elpa.emacs-china.org/melpa/"))

;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(setq package-enable-at-startup nil)
(package-initialize)

(require-package 'wgrep)
(require-package 'diminish)
(require-package 'command-log-mode)
(require-package 'dsvn)
(maybe-require-package 'regex-tool)

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new GUI frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst reimuxmx-initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when reimuxmx-initial-frame
                  (run-after-make-frame-hooks reimuxmx-initial-frame))))

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")

(defun reimuxmx/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))

(add-hook 'after-make-console-frame-hooks 'reimuxmx/console-frame-setup)

(setq-default custom-enabled-themes '(tsdh-dark))

(defun reimuxmx/reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
(add-hook 'after-init-hook 'reimuxmx/reapply-themes)

(setq-default initial-scratch-message
              (concat ";; Happy Hacking, " user-login-name " ∞ Emacs ♥ You!\n\n"))

(setq inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(maybe-require-package 'disable-mouse)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(with-eval-after-load 'dired
  (setq-default dired-dwim-target t))

(when (maybe-require-package 'anzu)
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur) ;; to match ivy conventions

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defun reimuxmx/isearch-yank-symbol ()
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

(define-key isearch-mode-map "\C-\M-w" 'reimuxmx/isearch-yank-symbol)

(defun reimuxmx/isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'reimuxmx/isearch-exit-other-end)

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t))

(require 'uniquify)
(setq uniquify-ignore-buffers-re "^\\*")

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(when (maybe-require-package 'smex)
  (setq-default smex-history-length 16
                smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (setq-default ivy-use-virtual-buffers t
                projectile-completion-system 'ivy
                ivy-initial-inputs-alist
                '((man . "^")
                  (woman . "^")))

  (with-eval-after-load 'ivy
    (diminish 'ivy-mode)))


(when (maybe-require-package 'ivy-historian)
  (add-hook 'after-init-hook (lambda () (ivy-historian-mode t))))


(when (maybe-require-package 'counsel)
  (add-hook 'after-init-hook 'counsel-mode)
  (setq-default counsel-mode-override-describe-bindings t)

  (with-eval-after-load 'counsel
    (diminish 'counsel-mode))

  (when (and (executable-find "ag")
             (maybe-require-package 'projectile))
    (defun reimuxmx/counsel-ag-project (initial-input &optional use-current-dir)
      "Search using `counsel-ag' from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
      (interactive (list (thing-at-point 'symbol)
                         current-prefix-arg))
      (let ((current-prefix-arg)
            (dir (if use-current-dir
                     default-directory
                   (condition-case err
                       (projectile-project-root)
                     (error default-directory)))))
        (counsel-ag initial-input dir)))

    (global-set-key (kbd "M-?") 'reimuxmx/counsel-ag-project)))


(when (maybe-require-package 'swiper)
  (with-eval-after-load 'ivy
    (defun reimuxmx/swiper-at-point (sym)
      "Use `swiper' to search for the symbol at point."
      (interactive (list (thing-at-point 'symbol)))
      (swiper sym))

    (define-key ivy-mode-map (kbd "M-s /") 'reimuxmx/swiper-at-point)))

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
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))

(defun reimuxmx/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'reimuxmx/maybe-adjust-visual-fill-column)

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

(defun reimuxmx/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun reimuxmx/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun reimuxmx/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(defun reimuxmx/sort-lines-random (beg end)
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

(defun reimuxmx/delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun reimuxmx/rename-current-file-and-buffer (new-name)
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

(defun reimuxmx/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(with-eval-after-load 'which-key
  (diminish 'which-key-mode)
  (which-key-setup-side-window-bottom))

(setq-default show-trailing-whitespace t)

(defun reimuxmx/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'reimuxmx/no-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)

(global-set-key [remap just-one-space] 'cycle-spacing)

(when (maybe-require-package 'cmake-mode)
  (add-auto-mode 'cmake-mode "CMakeLists\\.txt\\'" "\\.cmake\\'"))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-auto-mode 'tcl-mode "Portfile\\'")
(add-auto-mode 'conf-mode "Procfile")
(add-auto-mode 'conf-space-mode "hostname")
(add-auto-mode 'conf-mode "hosts")
(add-auto-mode 'text-mode "\\LICENSE\\'")

(when (file-exists-p custom-file)
  (load custom-file))

(defun reimuxmx/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun reimuxmx/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (reimuxmx/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (reimuxmx/utf8-locale-p (getenv "LC_ALL"))
      (reimuxmx/utf8-locale-p (getenv "LC_CTYPE"))
      (reimuxmx/utf8-locale-p (getenv "LANG"))))

(when (or window-system (reimuxmx/locale-is-utf8-p))
  (set-language-environment 'utf-8)
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

;; init.el ends here
