;;; デフォ
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-locale-environment "utf-8")
(setenv "LANG" "ja_JP.UTF-8")

;;; elisp関連
(add-to-list 'load-path "~/.emacs.d/")

;;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; みみっちい設定
; (tool-bar-mode -1)
(menu-bar-mode -1)
; (toggle-scroll-bar nil)
(setq image-load-path "")
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

;;; fullscreen
;;; (run-with-idle-timer 0.1 nil 'ns-toggle-fullscreen)

;;; ruby-mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
;;; ruby-electric.el
;;; error in emacs24
 (require 'ruby-electric)
 (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;(require 'ruby-end)
;(add-hook 'ruby-mode-hook
;  '(lambda ()
;    (abbrev-mode 1)
;    (electric-pair-mode t)
;    (electric-indent-mode t)
;    (electric-layout-mode t)))

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
(setq linum-format "%3d| ")

;;; go-lang
(require 'go-mode-load)
; (require 'color-theme-solarized) 

;;; clojure-mode
(require 'clojure-mode)
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

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

;;; quickrun.el
(require 'quickrun)
(global-set-key "\C-x\C-a" 'quickrun)

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

;;; rsense
;(setq rsense-home "/Users/y_benjo/workspace/dotfiles/rsense-0.3")
;(add-to-list 'load-path (concat rsense-home "/etc"))
;(require 'rsense)
;(add-hook 'ruby-mode-hook
;          (lambda ()
;            (add-to-list 'ac-sources 'ac-source-rsense-method)
;            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(global-auto-revert-mode 1)
(setq completion-ignore-case t)

(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")

; anzu-mode
(global-anzu-mode +1)
