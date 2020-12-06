;;;; emacs init
;;;; Borrowed/cribbed largely from DOOM Emacs. Leaving out the vim keybindings and trying to stick only with
;;;; extensions I know about and need.

;; Some useful constants

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; macos native ls doesn't support dired. Also set macos-specific path info. See http://ergoemacs.org/emacs/emacs_env_var_paths.html
(cond (IS-MAC
       (setq dired-use-ls-dired nil)))

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

;; better-defaults https://github.com/technomancy/better-defaults
(require 'better-defaults)

;; disable splash screen
(setq inhibit-splash-screen t)

;; Turn on line numbers everywhere
(global-linum-mode 1)

;; soft-wrap everywhere
(global-visual-line-mode 1)

;; Load dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Change default font
;; (set-face-attribute 'default nil :font "Hack Nerd Font Regular-12")

;; C stuff
;; TODO `indent-tabs-mode t`? Also hippie-expand did not complete `c-default-style`
(setq-default c-basic-offset 4
              c-default-style "linux"
              tab-width 4
              indent-tabs-mode t)

;; paren automatching
(electric-pair-mode 1)

;; paredit - disable for now
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of lisp code." t)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit)
;; (add-hook 'ielm-mode-hook #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook #'enale-paredit-mode)

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

;; cache passwords
(require 'em-tramp)
(setq password-cache t)
(setq password-cache-expiry 3600)

;; helm customizations
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is too close to "C-x C-c" which quits emacs.
;; Must set "C-c h" globally because we cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind TAB to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t ; open helm buffer inside current window rather than spawning a new window
	  helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source
	  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
	  helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-ff-file-name-history-use-recentf t
	  helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
	(let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	  (overlay-put ov 'window (selected-window))
	  (overlay-put ov 'face
				   (let ((bg-color (face-background 'default nil)))
					 `(:background ,bg-color :foreground ,bg-color)))
	  (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook
		  'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

;; helm-system-packages: helm-based interface to OS package manager
(require 'helm-system-packages)

;; rust
(add-to-list 'load-path "~/.emacs.d/lisp/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; golang
(add-to-list 'load-path "~/.emacs.d/lisp/go-mode/")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (fish-mode csv-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
