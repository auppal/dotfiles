; Written by Ahsen Uppal
; Copyright (C) 2015, Ahsen Uppal
; All rights reserved.
;

(setq homedir (concat "~" (getenv "LOGNAME")))
(setq load-path (cons (concat homedir "/.emacs-custom") load-path))
(setq load-path (cons (concat homedir "/.emacs-custom/dtrt-indent") load-path))

       (add-hook 'c-mode-hook '(lambda() (load "c-keys")))
       (add-hook 'c++-mode-hook '(lambda() (load "c-keys")))

       ;; Allow C code to be in different colors depending on whether
       ;; it is a comment line, language command, etc.
       (global-font-lock-mode t)

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
(keyboard-translate ?\C-h ?\C-?)
;; better:
(define-key key-translation-map [?\C-h] [?\C-?])


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


(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
(set-cursor-color "orange")

(setq pop-up-frames t)
(require 'ibuffer)
;;(require 'dict)
(define-key global-map "\C-x\C-b" 'ibuffer)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(set-default 'preview-scale-function 1.5)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(load "50gnuserv-gentoo.el" nil t t)
(gnuserv-start)

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
'("\\.divx$" "mplayer * >& /dev/null")
'("\\.avi$" "mplayer * >& /dev/null")
'("\\.pdf$" "pdftotext * /dev/stdout")
))

;; force these to open in a new frame
(setq special-display-buffer-names
    '("*Shell Command Output*" "*Python Output*" "*Help*" "*grep*"))
(setq special-display-regexps
    '("\*Man .*\*"))

(transient-mark-mode 0)

(require 'dtrt-indent)
(dtrt-indent-mode 1)

(require 're-builder)
(setq reb-re-syntax 'string)

(setq initial-major-mode 'python-mode)
(setq initial-scratch-message '"")

;; The manual says:
;; Prior to Emacs 24, the kill and yank commands used the primary selection
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

