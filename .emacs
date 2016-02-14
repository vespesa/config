(add-to-list 'load-path "~/.emacs.d/lisp/")

;; From better defaults. For some reason the package version did not work.
(ido-mode t)
(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)

;; (menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x !") 'eshell)
(global-set-key (kbd "C-x \"") 'shell)

(global-set-key (kbd "s-1") (lambda () (interactive)
                              (display-buffer "*eshell*" nil nil)
                              (switch-to-buffer-other-window "*eshell*" )))
(global-set-key (kbd "s-1") (lambda () (interactive)
                              (display-buffer "*eshell*" nil nil)
                              (switch-to-buffer-other-window "*eshell*" )))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region.
   http://stackoverflow.com/a/9697222/5297312"
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(global-set-key (kbd "C-c /") 'comment-or-uncomment-region-or-line)

(column-number-mode 1)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell nil
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; No beeps. Visible bell is too ugly in Mac to be an alternative.
(setq ring-bell-function 'ignore)

;; Left Alt is meta, right alt is alt...
(setq ns-right-alternate-modifier nil)

;; Less jumpy scroll.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Window management
(require 'move-border)
(global-set-key (kbd "M-s-<up>") 'move-border-up)
(global-set-key (kbd "M-s-<down>") 'move-border-down)
(global-set-key (kbd "M-s-<left>") 'move-border-left)
(global-set-key (kbd "M-s-<right>") 'move-border-right)

(windmove-default-keybindings 's)

(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun replace-scandinavian-characters ()
  (interactive)
  (let ((p (point)))
    (map 'list (lambda (a)
                 (goto-char (point-min))
                 (while (search-forward (string a) nil t)
                   (replace-match (format "\\\\u%04x" a) t))) "äöåÄÖÅ")
    (goto-char p)))



;; Tweaking of installed packages
(package-initialize)


;; Make sure that the Emacs exec path is the same as Bash.
(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(add-hook 'after-init-hook 'exec-path-from-shell-initialize)



;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)


;; Ido ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Flycheck
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-slurp-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-ä") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-Ä") 'sp-transpose-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-ö") (lambda () (interactive) (sp-transpose-sexp -1)))
(define-key smartparens-mode-map (kbd "M-<down>") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "M-<up>") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-symbol)
(define-key smartparens-mode-map (kbd "M-<right>") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "C-M-c") 'sp-copy)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-<up>") 'sp-raise-sexp)
(define-key smartparens-mode-map (kbd "C-M-<down>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-s") 'sp-split-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-join-sexp)


(sp-pair "(" ")" :wrap "M-(")
(sp-pair "[" "]" :wrap "M-[")
(sp-pair "{" "}" :wrap "M-{")


;; Paredit
;; (require 'paredit)
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;; (add-hook 'lisp-mode-hook 'paredit-mode)
;; (add-hook 'clojure-mode-hook 'paredit-mode)

;; ;; Paxedit
;; (require 'paxedit)
;; (add-hook 'emacs-lisp-mode-hook 'paxedit-mode)
;; (add-hook 'clojure-mode-hook 'paxedit-mode)

;; (eval-after-load "paxedit"
;;   '(progn (define-key paxedit-mode-map (kbd "M-ä") 'paxedit-transpose-forward)
;;           (define-key paxedit-mode-map (kbd "M-ö") 'paxedit-transpose-backward)
;;           (define-key paxedit-mode-map (kbd "M-<up>") 'paxedit-backward-up)
;;           (define-key paxedit-mode-map (kbd "M-<down>") 'paxedit-backward-end)
;;           (define-key paxedit-mode-map (kbd "M-<left>") 'paxedit-previous-symbol)
;;           (define-key paxedit-mode-map (kbd "M-<right>") 'paxedit-next-symbol)
;;           (define-key paxedit-mode-map (kbd "C-M-c") 'paxedit-copy)
;;           (define-key paxedit-mode-map (kbd "C-M-k") 'paxedit-kill)
;;           (define-key paxedit-mode-map (kbd "C-<backspace>") 'paxedit-delete)
;;           (define-key paxedit-mode-map (kbd "C-M-<up>") 'paxedit-sexp-raise)
;;           (define-key paxedit-mode-map (kbd "M-u") 'paxedit-symbol-change-case)
;;           (define-key paxedit-mode-map (kbd "C-ä") 'paxedit-symbol-copy)
;;           (define-key paxedit-mode-map (kbd "C-ö") 'paxedit-symbol-kill)
;;           (define-key paxedit-mode-map (kbd "C-,") 'paxedit-wrap-comment)
;;           (define-key paxedit-mode-map (kbd "C-M-<down>") 'paxedit-dissolve)))

;; Clojure
(require 'clojure-mode-extra-font-locking)
;;(add-hook 'clojure-mode-hook #'paredit-mode)
;; (add-hook 'clojure-mode (local-unset-key "}"))
;; (add-hook 'clojure-mode (local-unset-key "{"))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(add-hook 'after-init-hook 'global-color-identifiers-mode)

(defun my-clojure-mode-hook ()
  ;; clj-refactor loads too much ns on connect. Do not use!
  ;; (require 'clj-refactor)
  ;; (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  ;; (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; (require 'cljr-helm)
  ;; (define-key clojure-mode-map (kbd "C-c r") 'cljr-helm)
  (require 'align-cljlet)
  (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
  (define-key clojure-mode-map (kbd "C-c h") 'clojure-cheatsheet))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Cider
;;(require 'cider-eldoc)
(add-hook 'cider-mode-hook #'eldoc-mode)
;;(add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers nil)

;;(add-hook 'cider-repl-mode-hook 'paredit-mode)
(setq cider-repl-use-pretty-printing t)

(setq cider-show-error-buffer nil)
(setq nrepl-log-messages nil)

(add-hook 'cider-mode-hook
          (lambda ()
            (global-set-key (kbd "s-'")'cider-switch-to-repl-buffer)
            (global-set-key (kbd "s-å") (lambda () (interactive) (display-buffer "*cider-error*" nil nil)))
            (define-key cider-mode-map (kbd "C-c TAB") 'cider-format-defun)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (define-key cider-repl-mode-map (kbd "s-'") 'cider-switch-to-last-clojure-buffer)))

;; Magit (Git support)
(require 'magit)

;(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

(global-git-gutter+-mode t)

;; Git-flow
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; For some reason Magit does not revert buffers
;; automatically even when the MRev is on.
;; This is a known 1.4. bug.
;;(require 'revbufs)

(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t nil) )))
    (message "Refreshed open files.") )

(global-set-key (kbd "s-r") 'revert-all-buffers)
;;(global-auto-revert-mode t)

;; We'll start the server just in case to avoid
;; git complaining about EDITOR.
(or 'server-process server-start) 
(setenv "EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

;; Mercurial
;; (require 'monky)
;; (global-set-key (kbd "C-x m") 'monky-status)
;; (setq monky-process-type 'cmdserver)

;; Smerge
(add-hook 'smerge-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c s g") 'smerge-keep-other)
            (local-set-key (kbd "C-c s r") 'smerge-keep-mine)))
                              

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(global-set-key (kbd "C-c S")
                (lambda () (interactive)
                  (helm-projectile-ag)))
(global-set-key (kbd "s-.") 'projectile-find-tag)


(require 'project-explorer)
(global-set-key (kbd "M-§")
                (lambda () (interactive)
                  (project-explorer-toggle)))

;; (global-set-key (kbd "s-+")
;;                 (lambda () (interactive)
;;                   (set-face-attribute 'default nil :height 110)))
;; (global-set-key (kbd "C-s-+")
;;                 (lambda () (interactive)
;;                   (set-face-attribute 'default nil :height 105)))

;; CoffeeScript
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

;; Markdown
(require 'filladapt)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'markdown-mode-hook 'filladapt-mode)

;; Remove trailing whitespace unobtrusively
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-global-mode)

;; Better parens support
(require 'mic-paren)
(paren-activate)
;; (electric-pair-mode 1)

;; Log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))

;; Ansi colors for syslog
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max)))
  (message "Ansi colors."))

(add-hook 'syslog-mode-hook (lambda ()
                              (display-ansi-colors)
                              (auto-revert-tail-mode)
                              (read-only-mode)
                              (local-set-key (kbd "s-r") 'display-ansi-colors)
                              (add-hook 'after-revert-hook 'display-ansi-colors nil t)
                              ))


;; (add-hook 'syslog-mode-hook 'display-ansi-colors)
;; (add-hook 'syslog-mode-hook 'auto-revert-tail-mode)
;; (add-hook 'syslog-mode-hook 'read-only-mode)

          
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Solarized theme
;; (add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")

;;; Treat all themes as safe
(setq custom-safe-themes t)
(color-theme-sanityinc-solarized-light)
;; Solarized default face bg color: #fdf6e3

(setq sml/no-confirm-load-theme t)
(setq sm/theme 'light)
(sml/setup)

(require 'sunrise-commander)

;; Org
;;(require 'org)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cc" 'org-capture)
;; (setq org-log-done t)
(require 'org-journal)
(add-hook 'org-journal-mode-hook (lambda ()
                                   (local-set-key (kbd "C-c C-c") (lambda ()
                                                                    (interactive)
                                                                   (save-buffer)
                                                                   (kill-buffer)
                                                                   (delete-window)))
                                   (local-set-key (kbd "C-c C-k") (lambda ()
                                                                    (interactive)
                                                                    (es-kill-buffer-dont-ask)
                                                                    (delete-window)))
                                   (local-set-key (kbd "C-c r") (lambda ()
                                                                    (interactive)
                                                                    (org-mode-restart)))))
(global-set-key (kbd "C-c f") 'org-journal-search-forever)

;; SCSS (Sass)
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
;;(add-hook 'sccs-mode-hook 'electric-pair-mode)

;; web-mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; (setq web-mode-engines-alist
;;       '(("knockoutjs"    . "\\.html\\'")))

;; (add-hook 'web-mode-hook (lambda ()
;;                            (require 'company-web-html)
;;                            (local-set-key (kbd "C-M-n") 'sp-html-next-tag)
;;                            (local-set-key (kbd "C-M-p") 'sp-html-previous-tag)))


;; HTML: Tagedit, hl-tags

(add-hook 'html-mode-hook (lambda ()
                            (require 'hl-tags-mode)
                            (hl-tags-mode 1)
                            (require 'tagedit)
                            (tagedit-mode 1)
                            (tagedit-add-experimental-features)
                            (define-key html-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
                            (define-key html-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
                            (define-key html-mode-map (kbd "C-M-<up>") 'tagedit-raise-tag)
                            (define-key html-mode-map (kbd "C-M-<down>") 'tagedit-splice-tag)
                            (define-key html-mode-map (kbd "C-M-j") 'tagedit-join-tags)
                            (define-key html-mode-map (kbd "C-M-s") 'tagedit-split-tag)
                            (define-key html-mode-map (kbd "M-ä") 'tagedit-convolute-tags)
                            (define-key html-mode-map (kbd "C-k") 'tagedit-kill)
                            (define-key html-mode-map (kbd "C-M-k") 'tagedit-kill-attribute)
                            (define-key html-mode-map (kbd "M-k") 'kill-line)
                            (define-key html-mode-map (kbd "C-c /") 'comment-region)
                            (define-key html-mode-map (kbd "C-c 7") 'sgml-close-tag)))

(add-to-list 'auto-mode-alist '("\\.xml\\'" . html-mode ))

;; Avy
(avy-setup-default)
(global-set-key (kbd "C-c SPC") 'avy-goto-char-2)

;; Ace window
(global-set-key (kbd "s-j") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


;; JavaScript
(require 'js3-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))

(add-hook 'js3-mode-hook (lambda ()
                           (tern-mode t)
                           (ggtags-mode 1)))
(add-to-list 'company-backends 'company-tern)


;; Robot mode
(load "robot-mode")
(require 'autopair)
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))
(add-hook 'robot-mode-hook #'(lambda () (autopair-mode)))

;; imenu-anywhere
(global-set-key (kbd "C-.") #'helm-imenu-anywhere)

;; dumb-jump
(dumb-jump-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 2)
 '(avy-all-windows nil)
 '(blink-cursor-mode nil)
 '(c-basic-offset 2)
 '(calendar-today-visible-hook (quote (calendar-mark-today org-journal-mark-entries)))
 '(calendar-week-start-day 1)
 '(coffee-tab-width 2)
 '(comint-prompt-read-only t)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode js3-mode scss-mode html-mode)))
 '(company-idle-delay 0.2)
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes
   (quote
    ("ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "d1dbd38c2fef808a27bb411ecff76a0a8026856a16cb2a1fb8820bedeb45740a" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(custom-theme-load-path
   (quote
    ("/Users/vespesa/.emacs.d/elpa/color-theme-sanityinc-solarized-2.28/" "/Users/vespesa/.emacs.d/elpa/zenburn-theme-2.2" custom-theme-directory t)))
 '(exec-path
   (quote
    ("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin")))
 '(flycheck-disabled-checkers (quote (html-tidy)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "public")))
 '(helm-lisp-fuzzy-completion t)
 '(helm-split-window-default-side (quote right))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 2)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-highlight-external-variables nil)
 '(js3-include-browser-externs nil)
 '(js3-include-gears-externs nil)
 '(js3-include-rhino-externs nil)
 '(js3-mode-show-parse-errors t)
 '(js3-mode-show-strict-warnings nil)
 '(js3-pretty-vars nil)
 '(js3-strict-cond-assign-warning nil)
 '(js3-strict-inconsistent-return-warning nil)
 '(js3-strict-missing-semi-warning t)
 '(js3-strict-trailing-comma-warning nil)
 '(js3-strict-var-hides-function-arg-warning nil)
 '(js3-strict-var-redeclaration-warning nil)
 '(magit-no-confirm (quote (stage-all-changes)))
 '(magit-use-overlays nil)
 '(org-agenda-files (quote ("~/org/" "~/Documents/journal/")))
 '(org-agenda-window-setup (quote other-window))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file "~/org/notes.org")
      "* TODO %?")
     ("j" "Journal entry" entry
      (file+datetree "~/org/journal.org")
      "* %?")
     ("n" "Note in plain text" plain
      (file "~/org/notes.txt")
      ""))))
 '(org-default-notes-file "~/org/notes.org")
 '(org-journal-date-format "%A, %-e.%m.%Y")
 '(org-journal-hide-entries-p nil)
 '(org-startup-folded nil)
 '(org-support-shift-select t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("Marmalade" . "https://marmalade-repo.org/packages/")
     ("Melpa" . "https://melpa.org/packages/")
     ("Melpa Stable" . "https://stable.melpa.org/packages/"))))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-tags-command "ctags -Re --exclude=\"*.min.js\" -f \"%s\" %s")
 '(projectile-use-git-grep t)
 '(scss-compile-at-save nil)
 '(show-paren-mode nil)
 '(show-smartparens-global-mode t)
 '(smartparens-global-strict-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/no-confirm-load-theme nil)
 '(solarized-broken-srgb t)
 '(sp-hybrid-kill-excessive-whitespace nil)
 '(sp-ignore-modes-list (quote (minibuffer-inactive-mode html-mode robot-mode)))
 '(sp-navigate-close-if-unbalanced t)
 '(sp-no-reindent-after-kill-modes (quote (coffee-mode js2-mode js3-mode robot-mode)))
 '(sp-sexp-prefix (quote ((emacs-lisp-mode regexp "\\(?:,@\\|[',`]\\)"))))
 '(sp-sexp-suffix
   (quote
    ((inferior-python-mode regexp "")
     (python-mode regexp "")
     (js3-mode regexp "")
     (ruby-mode syntax "")
     (robot-mode regexp "")
     (scss-mode regexp ""))))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-current-column-highlight t)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "DejaVu Sans Mono"))))
 '(company-tooltip ((t (:background "wheat2"))))
 '(fringe ((t (:background "#fdf6e3"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "dark red" :foreground "white"))))
 '(web-mode-comment-face ((t (:foreground "dark blue" :slant normal))))
 '(web-mode-current-column-highlight-face ((t (:background "bisque")))))
