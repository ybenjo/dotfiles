;;; デフォ
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-locale-environment "utf-8")
(setenv "LANG" "ja_JP.UTF-8")

;;; みみっちい設定
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar nil)
(setq transient-mark-mode t)
(setq w32-hide-mouse-on-key t)
(setq make-backup-files nil)
(setq w32-hide-mouse-timeout 5000)
(global-font-lock-mode t)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

;;; 対応する括弧に色付け
(show-paren-mode t)
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "red")
(set-face-foreground 'show-paren-match-face "SkyBlue")

;;; C-hでbackspace、C-oで補間
(define-key global-map "\C-H" 'backward-delete-char)
(define-key global-map "\C-o" 'dabbrev-expand)

;;; ウィンドウサイズ関連
 (setq default-frame-alist
       (append (list '(foreground-color . "azure3")
 	'(background-color . "black")
 	'(border-color . "black")
 	'(mouse-color . "white")
	'(cursor-color . "white")
	'(width . 155)
	'(height . 59)
	'(width . 90)
	'(height . 50)
	'(top . 0)
	'(left . 0)
	'(alpha . (70 100 100 100))
	'(alpha . (100 100 100 100))
	)
	default-frame-alist))

;;; carbon emacsでフルスクリーン
(when (eq window-system 'mac)
(add-hook 'window-setup-hook
(lambda ()
(set-frame-parameter nil 'fullscreen 'fullboth))))

;;;突っ込んだelisp関連
(add-to-list 'load-path "~/.emacs.d/")
;;; ruby-mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
;;; ruby-electric.el --- electric editing commands for ruby files
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;;; C-c C-c で選択範囲をコメントアウト
(define-key ruby-mode-map "\C-c\C-c" 'comment-region)
;;; C-c C-u で選択範囲のコメントを解除
(define-key ruby-mode-map "\C-c\C-u" 'uncomment-region)

;;; elscreen
; (load "elscreen" "ElScreen" t)

;;; yatex
(setq auto-mode-alist(cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq load-path (cons "~/site-lisp/yatex" load-path))
(setq tex-command "/opt/local/bin/platex")
(setq dviprint-command-format "/opt/local/bin/dvipdfmx %s")
;;; latexmkのために
;;; このため，全てのtexファイルの先頭行に%#!latexmkと書かないとエラーが起きる
;(setq tex-command "latexmk -pdfdvi")
;(setq dviprint-command-format "/usr/local/bin/dvipdfmx %s")
;;; C-c b Eでeqnarrayを呼び出す
(setq yatex-mode-load-hook
      '(lambda()
  (YaTeX-define-begend-key "bE" "eqnarray")
  ))

;;; ess
(add-to-list 'load-path "~/.emacs.d/ess/lisp")
(require 'ess-site)
(setq ess-ask-for-ess-directory nil)
(setq ess-pre-run-hook
      '((lambda ()
	  (setq default-process-coding-system '(utf-8 . utf-8))
	  )))
(defun ess:format-window-1 ()
	(split-window-horizontally)
	(other-window 1)
	(split-window)
	(other-window 1))
(add-hook 'ess-pre-run-hook 'ess:format-window-1)

;;; haml-mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;;; sass-mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;; linem
(require 'linum)
(global-linum-mode)
(setq linum-format "%3d")

;;; go-lang
(require 'go-mode-load)
; (require 'color-theme-solarized) 

;;; clojure-mode
(require 'clojure-mode)
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

;;; package
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; フォント設定
; http://d.hatena.ne.jp/ground256/20101225/1293278087
(when (eq system-type 'darwin)
  (if (>= emacs-major-version 23)
      (cond (window-system
	     (create-fontset-from-ascii-font "Menlo-18:weight=normal:slant=normal" nil "menlomarugo")
	     (set-fontset-font "fontset-menlomarugo"
			       'unicode
			       (font-spec :family "Hiragino Maru Gothic ProN" :size 18)
			       nil
			       'append)
	     (add-to-list 'default-frame-alist '(font . "fontset-menlomarugo"))
	     ))))

;; Command-Key and Option-Key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;;; auto-complete
(add-to-list 'load-path "/Users/y_benjo/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/y_benjo/.emacs.d/ac-dict")
(ac-config-default)

;;; anything.el
(require 'anything-config)
(global-set-key "\C-xv" 'anything)
(setq recentf-max-saved-items 500)
(recentf-mode 1)
