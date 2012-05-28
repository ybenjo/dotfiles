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

;;; C-z潰す
(global-unset-key "\C-z")

;;; バックスラッシュを入力
(define-key global-map [?¥] [?\\])


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

;;; fullscreen
(run-with-idle-timer 0.1 nil 'ns-toggle-fullscreen)

;;; elisp関連
(add-to-list 'load-path "~/.emacs.d/")
;;; ruby-mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
;;; ruby-electric.el
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
(setq load-path (cons "~/.emacs.d/yatex" load-path))
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
(global-set-key "\C-x\C-f" 'anything-for-files)
(setq recentf-max-saved-items 500)
(recentf-mode 1)

;;; multi-term.el
(when (require 'multi-term nil t)
  (setq multi-term-program "/opt/local/bin/zsh"))

;;; quickrun.el
(require 'quickrun)
(global-set-key "\C-x\C-a" 'quickrun)

;;; tabbar.el
;;; http://d.hatena.ne.jp/plasticster/20110825/1314271209
; (require 'tabbar)
; (tabbar-mode 1)
; (setq tabbar-buffer-groups-function nil)
; (dolist (btn '(tabbar-buffer-home-button
;                tabbar-scroll-left-button
;                tabbar-scroll-right-button))
;   (set btn (cons (cons "" nil)
;                  (cons "" nil))))
; (setq tabbar-separator '(1.5))
; (global-set-key "\C-zn" 'tabbar-forward-tab)
; (global-set-key "\C-zp" 'tabbar-backward-tab)

;;; org-mode
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate)
(setq org-directory "~/workspace/orgs")
(setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ))

;;; outline-mode
(add-to-list 'auto-mode-alist '("\\.txt$" . outline-mode))

;;; rvm
(require 'rvm)
(rvm-use-default)

;;; flymake for ruby
;;; http://d.hatena.ne.jp/gan2/20080702/1214972962
(require 'flymake)
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;;: Don't want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))
    ;;; エラー行で C-c d するとエラーの内容をミニバッファで表示する
    (define-key ruby-mode-map "\C-cd" 'credmp/flymake-display-err-minibuf)))

(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))

;;; powerline.el
(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\".	c %s\",
\" 	c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\".	c %s\",
\" 	c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))


(defconst color1 "#999")
(defconst color2 "#555")

(defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
(defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
(defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
(defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

(setq-default mode-line-format
 (list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
                        (propertize " " 'display arrow-right-1)))
        '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
                        (propertize " " 'display arrow-right-2)))

        ;; Justify right by filling with spaces to right fringe - 16
        ;; (16 should be computed rahter than hardcoded)
        '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

        '(:eval (concat (propertize " " 'display arrow-left-2)
                        (propertize " %p " 'face 'mode-line-color-2)))
        '(:eval (concat (propertize " " 'display arrow-left-1)
                        (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
)) 

(make-face 'mode-line-color-1)
(set-face-attribute 'mode-line-color-1 nil
                    :foreground "#fff"
                    :background color1)

(make-face 'mode-line-color-2)
(set-face-attribute 'mode-line-color-2 nil
                    :foreground "#fff"
                    :background color2)

(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#000"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fff"
                    :background "#000")
