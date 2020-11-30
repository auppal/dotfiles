; Written by Ahsen Uppal
; Copyright (C) 2015, Ahsen Uppal
; All rights reserved.
;
;; Add the melpa package repository.
;; https://github.com/melpa/melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)	
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))	
(package-initialize)

(setq homedir (concat "~" (getenv "LOGNAME")))
(setq load-path (cons "~/.emacs-custom" load-path))

(add-hook 'c-mode-hook '(lambda() (load "c-keys")))
(add-hook 'c++-mode-hook '(lambda() (load "c-keys")))

;; setting global-font-lock-mode may no longer be needed
;; (global-font-lock-mode t) ;; syntax highlighting
(show-paren-mode t) ;; paren highlighting

;; Keybindings
;; make control-h backspace
;; keyboard-translate is per terminal, so better is the translation-map
(keyboard-translate ?\C-h ?\C-?)
(define-key key-translation-map [?\C-h] [?\C-?])

;; Commented out because using save-buffers-kill-terminal (C-x C-c) is better.
;; (defun kill-buffer-and-frame () (interactive) (kill-this-buffer) (delete-frame))
;; (define-key global-map "\C-x\k" 'kill-buffer-and-frame)
(defun kill-this-buffer-safe () (interactive) (kill-buffer (current-buffer)))
(define-key global-map "\C-x\k" 'kill-this-buffer-safe)

(define-key global-map "\C-x\m" 'compile)
;;(define-key global-map "\C-x\C-b" 'bs-show)
(define-key global-map "\M-n" 'scroll-up)
(define-key global-map "\M-p" 'scroll-down)
(define-key global-map "\M-g" 'goto-line)
(define-key global-map "\M-h" 'backward-kill-word)
(define-key global-map "\M-\C-h" 'backward-kill-word)
(define-key global-map "\C-x\C-f" 'ffap)

;; And File modes
(autoload 'python-mode "python-mode.el" "Enter Python mode." t)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.m\\'" . octave-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu\\'" . c-mode) auto-mode-alist))

(setq visible-cursor nil)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
;;(require 'dict)

(require 'ibuffer)
(define-key global-map "\C-x\C-b" 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

(require 'tex-site nil t)
(with-demoted-errors
    (load "auctex.el" nil t t) (load "preview-latex.el" nil t t))


(set-default 'preview-scale-function 1.5)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun unfill-paragraph (arg)
  "Pull this whole paragraph up onto one line."
  (interactive "*p")
  (let ((fill-column (point-max)))
    (fill-paragraph arg))
  )

;; enable for backtrace on error
;; (setq debug-on-error t)
(setq inhibit-startup-message t)

;; dired
(require 'dired-aux)
(require 'dired-x)
(setq dired-guess-shell-alist-user (list
'("\\.divx$" "mplayer * >& /dev/null &!")
'("\\.avi$" "mplayer * >& /dev/null &!")
'("\\.mp4$" "mplayer * >& /dev/null &!")
'("\\.wmv$" "mplayer * >& /dev/null &!")
'("\\.pdf$" "pdftotext * /dev/stdout")
))

;; force these to open in a new frame
(setq special-display-buffer-names
    '("*Shell Command Output*" "*Python Output*" "*Help*" "*grep*"))

;; Use new frames instead of splitting windows
;; (setq pop-up-frames t)
;; (setq display-buffer-reuse-frames t)

;; From: http://lists.gnu.org/archive/html/emacs-devel/2011-07/msg00499.html
(setq pop-up-frames 'graphic-only
      display-buffer-reuse-frames t
      special-display-regexps '(("^\\*.*\\*$" pop-to-buffer-same-frame)))

;; Support for man pages
(setq special-display-regexps
    '("\*Man .*\*"))
(setq-default Man-notify-method 'pushy)
;; Colorize man pages
;; https://emacs.stackexchange.com/questions/21499/how-does-one-colorize-keywords-in-info-pages-like-in-man
(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-comment-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-function-name-face :underline t)

;; From: http://home.thep.lu.se/~karlf/emacs.html#sec-6-3
;; Remove/kill completion buffer when done
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))
))

(with-demoted-errors (require 'dtrt-indent) (dtrt-indent-mode 1))

(require 're-builder)
(setq reb-re-syntax 'string)

(setq initial-major-mode 'python-mode)
(setq initial-scratch-message '"")

;; The manual says:
;; Prior to Emacs 24, the kill and yank commands used the primary selection
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)


;; Not using this python mode anymore
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (define-key python-mode-map "\C-c\C-c" 'py-execute-buffer-ipython)))

(setq py-split-windows-on-execute-p nil)
;; And py-split-windows-on-execute-p renamed to py-split-window-on-execute in python-mode.el-6.2.0
(setq py-split-window-on-execute nil)


;; Toggle window dedication
;; (defun toggle-window-dedicated ()
;;   "Toggle whether the current active window is dedicated or not"
;;   (interactive)
;;   (message
;;    (if (let (window (get-buffer-window (current-buffer)))
;;          (set-window-dedicated-p window 
;;                                  (not (window-dedicated-p window))))
;;        "Window '%s' is dedicated"
;;      "Window '%s' is normal")
;;    (current-buffer)))

;; ;; Press [pause] key in each window you want to "freeze"
;; (global-set-key [pause] 'toggle-window-dedicated)

(require 'python)

;; interactively do things
(require 'ido)
(ido-mode t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(require 'browse-url) ; in emacs

; Based on: http://ergoemacs.org/emacs/emacs_lookup_ref.html
;
(defun wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.
This command switches you to your browser."
  (interactive)
  (let (myWord myUrl)
    (setq myWord
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'word)))
    (setq myWord (replace-regexp-in-string " " "_" myWord))
    (setq myUrl (concat "https://en.wikipedia.org/wiki/" myWord))
    (browse-url myUrl)
    ;; (eww myUrl) ; emacs's own browser
    ))

; Disabling transient-mark-mode makes use-region-p always false.
(defun google ()
  "Look up the region on Google."
  (interactive)
  (let (myWord myUrl)
    (setq myWord
          (buffer-substring-no-properties (region-beginning) (region-end)))
    (setq myUrl (concat "https://www.google.com/search?q=" myWord))
    (browse-url myUrl)
    ;; (eww myUrl) ; emacs's own browser
    ))

(defun scholar ()
  "Look up the region on Google Scholar."
  (interactive)
  (let (myWord myUrl)
    (setq myWord
          (buffer-substring-no-properties (region-beginning) (region-end)))
    (setq myUrl (concat (concat "http://scholar.google.com/scholar?q=" myWord) "&as_sdt=1"))
    (browse-url myUrl)
    ;; (eww myUrl) ; emacs's own browser
    ))

;; (with-demoted-errors (require 'ein-dev))
(with-demoted-errors (require 'ein) (require 'ein-loaddefs) (require 'ein-notebook) (require 'ein-subpackages))

;; https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
;; (with-demoted-errors (require 'ansi-color))
(require 'ansi-color)
(defun ansi-colors-buffer()
  (interactive)(ansi-color-apply-on-region (point-min) (point-max)))


;; Disable version control handlers (for speed)
;; http://shallowsky.com/blog/linux/editors/no-emacs-version-control.html
;; (setq vc-handled-backends nil)
(setq org-directory "~/org")

;; Set the appearance
; To temprarily enable transient-mark-mode, use C-SPC C-SPC
(transient-mark-mode 0)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(if (fboundp 'set-fringe-mode) (set-fringe-mode 0))
;;(set-face-attribute 'fringe nil :background nil)

;; Specify fonts here for use with gnuclient and emacsclient.
(add-to-list 'default-frame-alist '(font . "9x15"))
(add-to-list 'default-frame-alist '(cursor-color . "orange"))
(add-to-list 'default-frame-alist '(foreground-color . "grey"))
(add-to-list 'default-frame-alist '(background-color . "black"))

;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
(defun set-background-for-terminal (&optional frame)
  (or frame (setq frame (selected-frame)))
  "Unsets the background color in terminal mode."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame))
  (xterm-mouse-mode 1)
  )
(add-hook 'after-make-frame-functions 'set-background-for-terminal)
(add-hook 'window-setup-hook 'set-background-for-terminal)

;; Faces for display-line-numbers-mode
(set-face-foreground 'mode-line "#aaaa99")
(set-face-background 'mode-line "#333333")
(with-demoted-errors (set-face-foreground 'line-number "#333333"))
(with-demoted-errors (set-face-foreground 'line-number-current-line "#aaaa99"))
(set-face-attribute 'mode-line-buffer-id nil :foreground "white" :bold t)


;; Place all backups in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(define-key key-translation-map [?\H-x] [?\C-x])
(define-key key-translation-map [?\H-s] [?\C-s])
(define-key key-translation-map [?\H-c] [?\C-c])

;; Enable using xclip even in the terminal.
(with-demoted-errors (xclip-mode 1))
(with-demoted-errors (xterm-mouse-mode 1))

(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (delete-region (point-min) (point-max)) nil) t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;; Experimental speed fixes.
(setq bidi-display-reordering nil)
(setq vc-handled-backends nil)
(setq py-install-directory "/usr/share/emacs/site-lisp/python-mode")

;; Prevent python-mode from excessively adding to load-path
(add-hook 'python-mode-hook (defun py-set-load-path ()))

;; Emacs searches each directory for files .dir-locals.el which is slow for remote filesystems.
;; Tramp mode disables these by default. It should be possible to improve this to only disable
;; dir local for remote mounts.
(setq enable-dir-local-variables nil)


;; Set terminal titles
(defun xterm-title-update ()
    (interactive)
    (send-string-to-terminal (concat "\033]1; " (buffer-name) "\007"))
    (if buffer-file-name
     (send-string-to-terminal (concat "\033]2; " (buffer-file-name) "\007"))
     (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007"))))
(add-hook 'post-command-hook 'xterm-title-update)

;; org drill configuration
(with-demoted-errors (require 'org-drill))
(setq org-drill-spaced-repetition-algorithm 'simple8)
(setq org-drill-add-random-noise-to-intervals-p t)
(setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)

(defun run_drill_session ()
  (interactive)
  (find-file "~/code/mnemosyne-dump/mnemosyne.org")
  (org-drill))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default))
 '(doom-modeline-mode t)
 '(package-selected-packages
   '(org-drill doom-themes xclip vterm request-deferred px flycheck ein dtrt-indent doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
