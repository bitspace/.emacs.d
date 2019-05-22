;;;; emacs init
;;;; Borrowed/cribbed largely from DOOM Emacs. Leaving out the vim keybindings and trying to stick only with
;;;; extensions I know about and need.

;; Some useful constants
(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD (or IS-MAC (eq system-type 'berkeley-unix)))

;; Ensure emacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(defvar cjw-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs.d directory. Must end in a slash.")

;; add local lisp dir to load path
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Package.el. This method of toggling between http and https courtesy of https://melpa.org/#/getting-started
;; This makes it more portable, as it seems the https link doesn't work in Emacs on Windows.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; better-defaults https://github.com/technomancy/better-defaults
(require 'better-defaults)

;; disable splash screen
(setq inhibit-splash-screen t)

;; Turn on line numbers everywhere
(global-linum-mode 1)

;; Turn on soft line wraps
(global-visual-line-mode 1)

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Change default font
(set-face-attribute 'default nil :font "Hack Nerd Font Mono")

;; C stuff
;; TODO `indent-tabs-mode t`? Also hippie-expand did not complete `c-default-style`
(setq-default c-basic-offset 4
              c-default-style "linux"
              tab-width 4
              indent-tabs-mode t)

;; paren automatching
(electric-pair-mode 1)

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of lisp code." t)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enale-paredit-mode)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; erc
(require 'erc)

;; server
(server-start)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")
