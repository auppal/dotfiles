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

(defun up-slightly () (interactive) (scroll-up 5))
      (defun down-slightly () (interactive) (scroll-down 5))
      (global-set-key [mouse-4] 'down-slightly)
      (global-set-key [mouse-5] 'up-slightly)
      
      (defun up-one () (interactive) (scroll-up 1))
      (defun down-one () (interactive) (scroll-down 1))
      (global-set-key [S-mouse-4] 'down-one)
      (global-set-key [S-mouse-5] 'up-one)
      
      
      (defun up-a-lot () (interactive) (scroll-up))
      (defun down-a-lot () (interactive) (scroll-down))
      (global-set-key [C-mouse-4] 'down-a-lot)
      (global-set-key [C-mouse-5] 'up-a-lot)

      (modify-frame-parameters nil '((wait-for-wm . nil)))


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



(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

; To temprarily enable, use C-SPC C-SPC
(transient-mark-mode 0)

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

;; Try to use mobileorg
;; http://svn.red-bean.com/repos/main/3bits/mobile_org_3bits.txt
;; http://superuser.com/questions/645079/mobileorg-not-finding-emacs-org-file-with-ssh-server
(setq org-directory "~/org")
(setq org-mobile-directory "/mnt/hub/org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(package-selected-packages
   (quote
    (xclip doom-modeline vterm flycheck px dtrt-indent ein))))
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))

;; org-mobile-push upon save
;; http://stackoverflow.com/questions/8432108/how-to-automatically-do-org-mobile-push-org-mobile-pull-in-emacs
(add-hook 
  'after-save-hook 
  (lambda () 
    (if (string= buffer-file-name "/home/ahsen/org/tasks.org")
        (org-mobile-push))))

(if (fboundp 'set-fringe-mode) (set-fringe-mode 0))
;;(set-face-attribute 'fringe nil :background nil)

;; Specify font here for use with gnuclient and emacsclient.

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


;; Place all backups in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(define-key key-translation-map [?\H-x] [?\C-x])
(define-key key-translation-map [?\H-s] [?\C-s])
(define-key key-translation-map [?\H-c] [?\C-c])

(set-face-foreground 'mode-line "#aaaa99")
(set-face-background 'mode-line "#333333")
(set-face-foreground 'line-number "#333333")
(set-face-foreground 'line-number-current-line "#aaaa99")

(set-face-attribute 'mode-line-buffer-id nil :foreground "white" :bold t)

;; Enable using xclip even in the terminal.
(with-demoted-errors (xclip-mode 1))
(with-demoted-errors (xterm-mouse-mode 1))

(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (delete-region (point-min) (point-max)) nil) t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)
