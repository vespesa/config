;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'ac-js2)
(straight-use-package 'ace-window)
(straight-use-package 'ag)
(straight-use-package 'align-cljlet)
(straight-use-package 'autopair)
(straight-use-package 'beacon)
(straight-use-package 'better-shell)
(straight-use-package 'cider)
(straight-use-package 'clojure-mode)
(straight-use-package 'clojure-mode-extra-font-locking)
(straight-use-package 'color-identifiers-mode)
(straight-use-package 'color-theme-sanityinc-solarized)
(straight-use-package 'company)
(straight-use-package 'company-quickhelp)
(straight-use-package 'copy-as-format)
(straight-use-package 'counsel)
(straight-use-package 'counsel-projectile)
(straight-use-package 'deadgrep)
(straight-use-package 'defproject)
(straight-use-package 'deft)
(straight-use-package 'docker)
(straight-use-package 'dumb-jump)
(straight-use-package 'easy-kill)
(straight-use-package 'eval-sexp-fu)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'flycheck-clj-kondo)
(straight-use-package 'flycheck-popup-tip)
(straight-use-package 'ggtags)
(straight-use-package 'git-gutter)
(straight-use-package 'groovy-mode)
(straight-use-package 'hl-line+)
;(straight-use-package 'ido-ubiquitous)
(straight-use-package 'iedit)
(straight-use-package 'imenu-anywhere)
(straight-use-package 'ivy)
(use-package ivy-rich
  :straight t
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode t))
(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)
(straight-use-package 'company-prescient)
(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'make-color)
(straight-use-package 'markdown-mode)
(straight-use-package 'mic-paren)
(straight-use-package 'multiple-cursors)
(straight-use-package 'pandoc-mode)
(straight-use-package 'php-mode)
(straight-use-package 'powerline)
(straight-use-package 'prodigy)
;;(straight-use-package 'project-explorer)
(straight-use-package 'projectile-ripgrep)
(straight-use-package 'python-mode)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'sass-mode)
(straight-use-package 'smart-mode-line)
(straight-use-package 'smartparens)
(straight-use-package 'smooth-scrolling)
(straight-use-package 'solarized-theme)
(straight-use-package 'swiper)
(straight-use-package 'syslog-mode)
(straight-use-package 'tagedit)
(straight-use-package 'tern)
(straight-use-package 'todotxt)
(straight-use-package 'tramp-term)
(straight-use-package 'undo-tree)

(use-package vterm
  :straight t
  :config (add-to-list 'vterm-eval-cmds
                       '("update-pwd" (lambda (path)
                                        (setq default-directory path)))))

(straight-use-package 'wconf)

(use-package web-mode
  :straight t
  :mode ("\\.djhtml\\'" . web-mode)
  :bind (:map web-mode-map
              ("s-o" . web-mode-fold-or-unfold))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing nil))

(straight-use-package 'xref-js2)
(straight-use-package 'xterm-color)
(straight-use-package 'yaml-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; From better defaults. For some reason the package version did not work.
;;(ido-mode t)
;;(setq ido-enable-flex-matching t)
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

;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "M-z") 'zap-up-to-char)

;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-x !") 'eshell)
(global-set-key (kbd "C-x \"") 'shell)

(global-set-key (kbd "C-<kp-7>") (lambda () (interactive)
                              (display-buffer "*eshell*" nil nil)
                              (switch-to-buffer-other-window "*eshell*" )))
(global-set-key (kbd "s-1") (lambda () (interactive)
                              (display-buffer "*eshell*" nil nil)
                              (switch-to-buffer-other-window "*eshell*" )))
(global-set-key [remap kill-ring-save] 'easy-kill)

(require 'undo-tree)
(global-undo-tree-mode)

(defun eshell/old-clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))


(add-hook 'eshell-mode-hook (lambda ()
                              (local-set-key (kbd "C-l") (lambda ()
                                                           (interactive)
                                                           (eshell/clear-scrollback)))
                              (local-set-key (kbd "C-c C-l") (lambda ()
                                                           (interactive)
                                                           (eshell/clear-scrollback)))))

;; Shell tweaks

(defun wcy-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'wcy-shell-mode-kill-buffer-on-exit))
(defun wcy-shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

(add-hook 'shell-mode-hook (lambda ()
                             (wcy-shell-mode-hook-func)
                             (ansi-color-for-comint-mode-on)
                             ;;(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
                             (local-set-key (kbd "C-l") 'comint-clear-buffer)
                             (local-set-key (kbd "C-c C-l") 'comint-clear-buffer)))


(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; Also set TERM accordingly (xterm-256color)

;; You can also use it with eshell (and thus get color output from system ls):

(require 'eshell)

(add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

;(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

(defun reverse-text (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

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
(electric-indent-mode 1)

(setq-default indent-tabs-mode nil)
;;(setq tab-always-indent 'complete)
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
                   (replace-match (format "\\\\u%04x" a) t))) "äöåÄÖÅ§€")
    (goto-char p)))



;; Tweaking of installed packages
;(package-initialize)

;; Make sure that the Emacs exec path is the same as shell.
(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(add-hook 'after-init-hook 'exec-path-from-shell-initialize)

;; Helm
;; (require 'helm)
;; (require 'helm-config)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-s") 'helm-swoop)

;; (define-key helm-map (kbd "<tab>")    'helm-execute-persistent-action)
;; (define-key helm-map (kbd "M-x") 'helm-select-action)

;; Ivy
(require 'ivy)
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x r b") 'counsel-bookmark)

; Slim down ivy display
;; (setq ivy-count-format ""
;;       ivy-display-style nil
;;       ivy-minibuffer-faces nil)

;; Use Enter on a directory to navigate into the directory, not open it with dired.
(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

;; Ido ubiquitous
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode 1)

;; Resize default font

(defun toggle-font-size ()
  (interactive)
  (set-face-attribute 'default nil
                      :height (if (= (face-attribute 'default :height nil nil) 110)
                                  130 110)))

;; Flycheck
(require 'flycheck-clj-kondo)

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'flycheck
;;   '(custom-set-variables
;;    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(with-eval-after-load 'flycheck
  (flycheck-popup-tip-mode))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)
(setq company-global-modes '(not eshell-mode))

;; Flyspell

(require 'flyspell-lazy)

(flyspell-lazy-mode 1)

;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)


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
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-delete-word)

(sp-pair "(" ")" :wrap "M-(")
(sp-pair "[" "]" :wrap "M-[")
(sp-pair "{" "}" :wrap "M-{")

(sp-local-pair 'web-mode "%" "%" :wrap "C-%")
(sp-local-pair 'web-mode "<" ">" :wrap "C->")

;; (sp-local-pair 'web-mode "<" nil :actions nil)
;; (sp-local-pair 'web-mode "{" nil :actions nil)

;; sp-kill-hybrid-sexp does not work in text-mode.
(add-to-list 'auto-mode-alist '(".*/i18n/.*\\.txt" . fundamental-mode))

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
  ;; (yas-minor-mode 1) ; for adding require/use/import
  ;; (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; (require 'cljr-helm)
  ;; (define-key clojure-mode-map (kbd "C-c r") 'cljr-helm)
  (define-key clojure-mode-map (kbd "C-c C-a") 'clojure-align)
  (define-key clojure-mode-map (kbd "C-c h") 'clojure-cheatsheet)
  (define-clojure-indent
    (fact [1])
    (fact* [1])
    (facts [1])
    (facts* [1])
    (for-frag [1])
    (pcond-> [1])
    (pcond->> [1]))
  (set-fill-column 90)
  (setq clojure-align-separator "---"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Cider
;;(require 'cider-eldoc)
(add-hook 'cider-mode-hook #'eldoc-mode)
;;(add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers nil)

(setq cider-show-error-buffer nil)
(setq nrepl-log-messages nil)

(add-hook 'cider-mode-hook
          (lambda ()
            (global-set-key (kbd "s-'")'cider-switch-to-repl-buffer)
            (global-set-key (kbd "s-å") (lambda () (interactive) (display-buffer "*cider-error*" nil nil)))
            (define-key cider-mode-map (kbd "C-c TAB") 'cider-format-defun)
            (define-key cider-mode-map (kbd "<C-tab>") 'cider-format-defun)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (eldoc-mode t)
            (define-key cider-repl-mode-map (kbd "s-'") 'cider-switch-to-last-clojure-buffer)
            (define-key cider-repl-mode-map (kbd "C-:") 'clojure-toggle-keyword-string)
            (define-key cider-repl-mode-map (kbd "<S-return>") 'cider-repl-newline-and-indent)
            (define-key cider-repl-mode-map (kbd "s-*") 'cider-repl-toggle-pretty-printing)))

(defun java8 ()
  (interactive)
  (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/zulu-8.jdk/Contents/Home"))

(defun java11 ()
  (interactive)
  (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home"))

(defun java8-cider-jack-in ()
  (interactive)
  (java8)
  (cider-jack-in nil))

(defun java8-cider-jack-in-clj&cljs ()
  (interactive)
  (java8)
  (cider-jack-in-clj&cljs nil nil))

(defun java11-cider-jack-in ()
  (interactive)
  (java11)
  (cider-jack-in nil))

(defun java11-cider-jack-in-clj&cljs ()
  (interactive)
  (java11)
  (cider-jack-in-clj&cljs nil nil))


;; Figwheel + Cider
;; (require 'cider)
;; (defun cider-check-figwheel-requirements ()
;;   "Check whether we can start a Figwheel ClojureScript REPL."
;;   t)


;; (setq cider-cljs-lein-repl
;;       "(do (require 'figwheel-sidecar.repl-api)
;;            (figwheel-sidecar.repl-api/start-figwheel!)
;;            (figwheel-sidecar.repl-api/cljs-repl))")

;; (defun cider-figwheel-repl ()
;;   (interactive)
;;   (save-some-buffers)
;;   (with-current-buffer (cider-current-repl-buffer)
;;     (goto-char (point-max))
;;     (insert "(require 'figwheel-sidecar.repl-api)
;;              (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
;;              (figwheel-sidecar.repl-api/cljs-repl)")
;;     (cider-repl-return)))


;; Magit (Git support)
(require 'magit)

;(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

(global-git-gutter-mode t)

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


;; Todo: projektor, smart-region, smooth-scrolling

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(global-set-key (kbd "C-,") 'string-rectangle)

;; Projectile

(projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)

;; (global-set-key (kbd "C-c S")
;;                 (lambda () (interactive)
;;                   (helm-projectile-ag)))
(global-set-key (kbd "s-.") 'projectile-find-tag)
(global-set-key (kbd "s-,") 'pop-tag-mark)
;; (global-set-key (kbd "H-p") 'helm-projectile-find-file)
(global-set-key (kbd "H-1") 'projectile-run-eshell)
(global-set-key (kbd "H-2") 'projectile-run-shell)
(global-set-key (kbd "H-3") 'projectile-run-vterm)
;; (global-set-key (kbd "<f13>") 'helm-projectile-find-file)
;; (global-set-key (kbd "<f16>") 'helm-projectile-find-file)
(global-set-key (kbd "C-<kp-7>") 'projectile-run-eshell)
(global-set-key (kbd "C-<kp-8>") 'projectile-run-shell)
(global-set-key (kbd "C-<kp-9>") 'projectile-run-vterm)

;(require 'project-explorer)
;(global-set-key (kbd "M-§")
;                (lambda () (interactive)
;                  (project-explorer-toggle)))

;; Counsel-projectile
(global-set-key (kbd "s-s")
                (lambda () (interactive)
                  (counsel-projectile-rg)))
(global-set-key (kbd "H-p") 'counsel-projectile-find-file)
(global-set-key (kbd "<f13>") 'counsel-projectile-find-file)
(global-set-key (kbd "<f16>") 'counsel-projectile-find-file)


;; (global-set-key (kbd "s-+")
;;                 (lambda () (interactive)
;;                   (set-face-attribute 'default nil :height 110)))
;; (global-set-key (kbd "C-s-+")
;;                 (lambda () (interactive)
;;                   (set-face-attribute 'default nil :height 105)))


;; Projector
;; (require 'projector)


;; CoffeeScript
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

;; Markdown
;(require 'filladapt)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'pandoc-mode)
;(add-hook 'markdown-mode-hook 'filladapt-mode)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; Remove trailing whitespace unobtrusively
;; (require 'ws-butler)
;; (add-hook 'prog-mode-hook 'ws-butler-mode)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Term
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)
(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
(kill-buffer))

;; (add-hook 'syslog-mode-hook 'display-ansi-colors)
;; (add-hook 'syslog-mode-hook 'auto-revert-tail-mode)
;; (add-hook 'syslog-mode-hook 'read-only-mode)


;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Solarized theme
;; (add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")

;; Shell-mode tweaks
;; https://github.com/Hawstein/my-emacs/blob/master/_emacs/shell-buffer.el

(defun clear-comint-buffer ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)
    (goto-char (point-max))))

;;; Treat all themes as safe
(setq custom-safe-themes t)
(color-theme-sanityinc-solarized-light)
;; Solarized default face bg color: #fdf6e3

(setq sml/no-confirm-load-theme t)
(setq sm/theme 'light)
(sml/setup)

;; Todotxt
(global-set-key (kbd "C-c t") 'todotxt)

;; (require 'sunrise-commander)

;; Org
;;(require 'org)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cc" 'org-capture)
;; (setq org-log-done t)

;; Deprecated. Using Deft now.
;; (require 'org-journal)
;; (add-hook 'org-journal-mode-hook (lambda ()
;;                                    (local-set-key (kbd "C-c C-c") (lambda ()
;;                                                                     (interactive)
;;                                                                    (save-buffer)
;;                                                                    (kill-buffer)
;;                                                                    (delete-window)))
;;                                    (local-set-key (kbd "C-c C-k") (lambda ()
;;                                                                     (interactive)
;;                                                                     (es-kill-buffer-dont-ask)
;;                                                                     (delete-window)))
;;                                    (local-set-key (kbd "C-c r") (lambda ()
;;                                                                     (interactive)
;;                                                                     (org-mode-restart)))))
;; (global-set-key (kbd "C-c f") 'org-journal-search-forever)
;; (global-set-key (kbd "C-c j") 'org-journal-new-entry)

;; SCSS (Sass)
;;(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))
;;(add-hook 'sccs-mode-hook 'electric-pair-mode)

;; web-mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; (setq web-mode-engines-alist
;;       '(("knockoutjs"    . "\\.html\\'")))

;; (add-hook 'web-mode-hook (lambda ()
;;                            ;;(require 'company-web-html)
;;                            (smartparens-mode 0)
;;                            ;;(local-set-key (kbd "C-M-n") 'sp-html-next-tag)
;;                            ;;(local-set-key (kbd "C-M-p") 'sp-html-previous-tag)
;;                            ))


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
                            (define-key html-mode-map (kbd "C-c 7") 'sgml-close-tag)
                            (local-unset-key (kbd "C-c C-s"))))

(add-to-list 'auto-mode-alist '("\\.xml\\'" . html-mode ))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode ))

;; Avy
(avy-setup-default)
(global-set-key (kbd "C-c SPC") 'avy-goto-char-2)

;; Ace window
(global-set-key (kbd "s-j") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defun local-set-minor-mode-key (mode key def)
  "Overrides a minor mode keybinding for the local buffer, by
   creating or altering keymaps stored in buffer-local
   `minor-mode-overriding-map-alist'.
   Source: http://stackoverflow.com/a/14769115"
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)
                       map))))
    (define-key newmap key def)))

;; JavaScript
;;(require 'js3-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun my-js ()
  (tern-mode t)
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (local-unset-key (kbd "C-c C-s"))
  (local-unset-key (kbd "M-."))
  ;; xref-js2
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  ;; Ctags
  ;; Tern
  ;;(local-set-minor-mode-key 'ggtags-mode-map (kbd "M-.") 'tern-find-definition)
  (local-set-minor-mode-key 'smartparens-mode-map (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
  (local-set-minor-mode-key 'js-mode-map (kbd "s-o") 'js2-mode-toggle-element))

(add-hook 'js2-mode-hook 'my-js )
(add-to-list 'company-backends 'company-tern)

;; TAGS
;; I never got the global/projectile work properly so let's just use ctags.
;; (global-set-key (kbd "M-.") 'helm-etags-select)
;; (global-set-key (kbd "M-,") 'pop-tag-mark)


;; Robot mode
(load "robot-mode")
(require 'autopair)
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))
(add-hook 'robot-mode-hook #'(lambda ()
                               (autopair-mode)
                               (setq c-basic-offset 2)))

;; imenu-anywhere
;;(global-set-key (kbd "C-.") #'helm-imenu-anywhere)

;; dumb-jump
;;(dumb-jump-mode)

(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)

;; Perspective
;; Local since the package is broken in 26.1.
;; (require 'perspective)
;; (persp-mode t)
;; (define-key persp-mode-map (kbd "C-s-<right>") 'persp-next)
;; (define-key persp-mode-map (kbd "C-s-<left>") 'persp-prev)

(global-set-key (kbd "C-s-<right>") 'wconf-use-next)
(global-set-key (kbd "C-s-<left>") 'wconf-use-previous)

;; Initial (superfluous) wconf
(wconf-create)

(global-set-key (kbd "C--") 'flash-line-highlight)

;; Folding
;; (require 'origami) ;; Not usable in with large files.
;; (global-origami-mode t)
;; (define-key origami-mode-map (kbd "s-o") 'origami-recursively-toggle-node)
;; (define-key origami-mode-map (kbd "s-t") 'origami-toggle-all-nodes)

(global-set-key (kbd "s-o") 'hs-toggle-hiding)
(global-set-key (kbd "s-t") 'hs-hide-all)
(global-set-key (kbd "s-y") 'hs-show-all)

;; https://stackoverflow.com/questions/944614/emacs-does-hideshow-work-with-xml-mode-sgml-mode
;; Fix XML folding
(add-to-list 'hs-special-modes-alist
             (list 'nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"
                   "<!--"
                   'nxml-forward-element
                   nil))

;; Fix HTML folding
(dolist (mode '(sgml-mode
                html-mode))
  (add-to-list 'hs-special-modes-alist
               (list mode
                     "<!--\\|<[^/>]*[^/]>"
                     "-->\\|</[^/>]*[^/]>"
                     "<!--"
                     'sgml-skip-tag-forward
                     nil)))

(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'sgml-mode-hook #'hs-minor-mode)
(add-hook 'nxml-mode-hook #'hs-minor-mode)
(add-hook 'html-mode-hook #'hs-minor-mode)

;; SSH

(setq ssh-directory-tracking-mode 'ftp)
(add-hook 'ssh-mode-hook
          (lambda ()
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))

;; Project specific configurations
(load "~/projects/config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info nil)
 '(auto-revert-interval 2)
 '(avy-all-windows nil)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-window-scrolls nil)
 '(beacon-color "DarkOrange1")
 '(beacon-mode nil)
 '(blink-cursor-mode nil)
 '(c-basic-offset 2)
 '(calendar-today-visible-hook '(calendar-mark-today org-journal-mark-entries) t)
 '(calendar-week-start-day 1)
 '(cider-pprint-fn 'zprint)
 '(cider-print-fn 'zprint)
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-in-current-window t)
 '(cider-repl-use-pretty-printing nil)
 '(clojure-align-binding-forms
   '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs" "for-frag"))
 '(clojure-docstring-fill-column 90)
 '(clojure-indent-style 'align-arguments)
 '(coffee-tab-width 2)
 '(comint-process-echoes t)
 '(comint-prompt-read-only t)
 '(company-backends
   '(company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev))
 '(company-dabbrev-code-modes
   '(prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode js3-mode js2-mode scss-mode html-mode))
 '(company-idle-delay 0.2)
 '(company-prescient-mode t)
 '(company-selection-wrap-around t)
 '(compilation-message-face 'default)
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-type 'bar)
 '(custom-enabled-themes '(sanityinc-solarized-light))
 '(custom-safe-themes
   '("ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "d1dbd38c2fef808a27bb411ecff76a0a8026856a16cb2a1fb8820bedeb45740a" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(custom-theme-load-path
   '("/Users/vespesa/.emacs.d/elpa/color-theme-sanityinc-solarized-2.28/" "/Users/vespesa/.emacs.d/elpa/zenburn-theme-2.2" custom-theme-directory t) t)
 '(deft-auto-save-interval 60.0)
 '(deft-extensions '("md" "txt" "text" "markdown" "org"))
 '(deft-markdown-mode-title-level 2)
 '(dired-dwim-target t)
 '(display-buffer-alist '(("\\*shell" display-buffer-same-window (nil))))
 '(exec-path
   '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin"))
 '(exec-path-from-shell-variables
   '("PATH" "CORE_MODE" "MANPATH" "GOOGLE_APPLICATION_CREDENTIALS" "JAVA_HOME"))
 '(explicit-shell-file-name nil)
 '(eyebrowse-wrap-around t)
 '(flycheck-disabled-checkers '(html-tidy))
 '(flyspell-issue-message-flag nil)
 '(fringe-mode '(1 . 1) nil (fringe))
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "public"))
 '(helm-buffer-max-length nil)
 '(helm-lisp-fuzzy-completion t)
 '(helm-mode nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-default-side 'same)
 '(helm-swoop-pre-input-function (lambda nil ""))
 '(helm-swoop-split-with-multiple-windows t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(ibuffer-formats
   '((mark modified read-only locked " "
           (name 40 40 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
 '(ido-cr+-auto-update-blacklist t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(ivy-height 20)
 '(ivy-prescient-mode t)
 '(ivy-rich-path-style '## nil nil "Customized with use-package ivy-rich")
 '(ivy-wrap t)
 '(js-indent-level 2)
 '(js2-bounce-indent-p nil)
 '(js2-consistent-level-indent-inner-bracket t)
 '(js2-highlight-external-variables nil)
 '(js2-include-browser-externs nil)
 '(js2-include-gears-externs nil)
 '(js2-include-rhino-externs nil)
 '(js2-mode-assume-strict t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-pretty-vars nil)
 '(js2-strict-cond-assign-warning nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning t)
 '(js2-strict-trailing-comma-warning nil)
 '(js2-strict-var-hides-function-arg-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
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
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(magit-gitflow-hotfix-finish-arguments nil)
 '(magit-no-confirm '(stage-all-changes))
 '(magit-use-overlays nil)
 '(nrepl-sync-request-timeout 60)
 '(ns-function-modifier 'hyper)
 '(org-agenda-files '("~/org/" "~/Documents/journal/"))
 '(org-agenda-window-setup 'other-window)
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "~/org/notes.org")
      "* TODO %?")
     ("j" "Journal entry" entry
      (file+datetree "~/org/journal.org")
      "* %?")
     ("n" "Note in plain text" plain
      (file "~/org/notes.txt")
      "")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-journal-date-format "%A, %-e.%m.%Y")
 '(org-journal-hide-entries-p nil)
 '(org-startup-folded nil)
 '(org-support-shift-select t)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("Melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(undo-tree todotxt copy-as-format wconf counsel counsel-projectile php-mode sass-mode docker flycheck-popup-tip flycheck-clj-kondo defproject cider yaml-mode groovy-mode make-color deadgrep iedit tramp-term projectile-ripgrep xterm-color easy-kill deft hl-line+ perspective beacon prodigy swiper ivy projector better-shell clojure-mode company magit python-mode smartparens tern ac-js2 xref-js2 web-mode syslog-mode solarized-theme smart-mode-line rainbow-delimiters powerline pandoc-mode multiple-cursors markdown-mode imenu-anywhere ido-ubiquitous ggtags exec-path-from-shell eval-sexp-fu dumb-jump color-theme-sanityinc-solarized color-identifiers-mode clojure-mode-extra-font-locking clojure-cheatsheet autopair align-cljlet ag ace-window))
 '(paradox-automatically-star nil)
 '(persp-show-modestring nil)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(prescient-persist-mode t)
 '(projectile-completion-system 'ivy)
 '(projectile-enable-idle-timer nil)
 '(projectile-indexing-method 'hybrid)
 '(projectile-mode-line nil)
 '(projectile-tags-command "")
 '(projectile-use-git-grep t)
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (eval setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/openjdk-11.0.2.jdk/Contents/Home")
     (eval setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home")
     (eval
      (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home"))
     (cider-default-cljs-repl . "figwheel")
     (cider-default-cljs-repl . figwheel)
     (cider-default-cljs-repl quote figwheel)
     (cider-default-cljs-repl . "Figwheel")
     (encoding . utf-8)))
 '(scss-compile-at-save nil)
 '(show-paren-mode nil)
 '(show-smartparens-global-mode t)
 '(smartparens-global-strict-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/no-confirm-load-theme nil)
 '(solarized-broken-srgb t)
 '(sp-hybrid-kill-excessive-whitespace nil)
 '(sp-ignore-modes-list '(minibuffer-inactive-mode html-mode robot-mode))
 '(sp-navigate-close-if-unbalanced t)
 '(sp-no-reindent-after-kill-modes '(coffee-mode js2-mode js3-mode robot-mode))
 '(sp-sexp-prefix '((emacs-lisp-mode regexp "\\(?:,@\\|[',`]\\)")))
 '(sp-sexp-suffix
   '((inferior-python-mode regexp "")
     (python-mode regexp "")
     (js3-mode regexp "")
     (js2-mode regexp "")
     (ruby-mode syntax "")
     (robot-mode regexp "")
     (scss-mode regexp "")))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil)
 '(ssh-directory-tracking-mode t t)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vterm-always-compile-module t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-current-column-highlight t)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Monaco"))))
 '(cider-error-highlight-face ((t (:inherit nil :background "MistyRose1"))))
 '(cider-result-overlay-face ((t (:background "lightgreen" :box (:line-width -1 :color "black")))))
 '(cider-test-failure-face ((t (:background "orange red" :foreground "white"))))
 '(company-preview ((t (:background "khaki1" :foreground "#839496"))))
 '(company-tooltip ((t (:background "wheat2"))))
 '(eshell-prompt ((t (:foreground "dark green" :weight normal))))
 '(flycheck-error ((t (:background "misty rose" :underline nil))))
 '(flycheck-warning ((t (:background "LightGoldenrod1" :underline nil))))
 '(fringe ((t (:background "#fdf6e3"))))
 '(git-gutter:added ((t (:foreground "light green" :weight bold))))
 '(git-gutter:modified ((t (:foreground "SkyBlue1" :weight bold))))
 '(helm-selection ((t (:background "sienna1" :foreground "White"))))
 '(hl-line ((t (:background "tan1"))))
 '(ivy-current-match ((t (:background "sienna1" :foreground "white"))))
 '(ivy-minibuffer-match-face-1 ((t nil)))
 '(ivy-minibuffer-match-face-2 ((t (:background "chocolate1" :foreground "white"))))
 '(ivy-minibuffer-match-face-3 ((t (:background "tan2" :foreground "white"))))
 '(ivy-minibuffer-match-face-4 ((t (:background "LightSalmon1" :foreground "white"))))
 '(ivy-modified-buffer ((t (:foreground "firebrick"))))
 '(ivy-virtual ((t nil)))
 '(lazy-highlight ((t (:background "paleturquoise2"))))
 '(mode-line ((t (:background "papaya whip" :foreground "black" :box (:line-width 1 :color "#657b83") :weight normal))))
 '(rainbow-delimiters-unmatched-face ((t (:background "dark red" :foreground "white"))))
 '(region ((t (:background "light cyan" :inverse-video nil))))
 '(web-mode-comment-face ((t (:foreground "dark blue" :slant normal))))
 '(web-mode-current-column-highlight-face ((t (:background "bisque")))))
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
