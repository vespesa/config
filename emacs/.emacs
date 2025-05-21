;; -*- lexical-binding: t -*-

;; Bootstrap straight.el
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
;; (straight-use-package 'company)
;; (straight-use-package 'company-quickhelp)
(straight-use-package 'copy-as-format)

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :bind ("C-x c" . copilot-chat-display)
  :custom
  (copilot-chat-default-model "claude-3.7-sonnet")
  ;;:after (request org markdown-mode)
  )

(add-hook 'org-mode-hook #'visual-line-mode)

(straight-use-package 'csv-mode)

;;(straight-use-package 'nerd-icons)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; (use-package vscode-icon
;;   :straight t
;;   :ensure t
;;   :commands (vscode-icon-for-file))

(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :straight t
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"            "Home")
     ("d" "~/Downloads/"  "Downloads")
     ("t" "~/Desktop"     "Desktop")))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-default-layout '(0 0.4 0.6))
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state all-the-icons collapse file-time file-size)
        dirvish-side-attributes
        '(vc-state all-the-icons collapse file-size))
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-x d" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

(use-package eldoc :straight (:type built-in))

(straight-use-package 'project)
;; (straight-use-package 'counsel)
;; (straight-use-package 'counsel-projectile)
;;(straight-use-package 'dash)
(straight-use-package 'deadgrep)
(straight-use-package 'defproject)
(use-package deft
  :straight t
  :custom
  (deft-auto-save-interval 60.0)
  (deft-extensions '( "org" "md" "txt" "text" "markdown"))
  (deft-markdown-mode-title-level 2))

(straight-use-package 'docker)
(straight-use-package 'dumb-jump)
(straight-use-package 'easy-kill)
(straight-use-package 'eval-sexp-fu)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'flycheck-clj-kondo)
;;(straight-use-package 'flycheck-popup-tip)
;;(straight-use-package 'flycheck-pos-tip)
(straight-use-package 'ggtags)
(straight-use-package 'git-gutter)

(straight-use-package 'groovy-mode)
(straight-use-package 'hl-line+)
;(straight-use-package 'ido-ubiquitous)
(straight-use-package 'iedit)
(straight-use-package 'imenu-anywhere)
;; (straight-use-package 'ivy)
;; (use-package ivy-rich
;;   :straight t
;;   :hook (ivy-mode . ivy-rich-mode)
;;   :config
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;;   (ivy-rich-mode t))
(straight-use-package 'prescient)
;;(straight-use-package 'corfu-prescient)
(straight-use-package 'vertico-prescient)
;; (straight-use-package 'ivy-prescient)
;; (straight-use-package 'company-prescient)
;; (use-package ivy-xref
;;   :straight t
;;   :config
;;   (setq xref-show-definitions-function #'ivy-xref-show-defs)
;;   (setq xref-show-xrefs-function #'ivy-xref-show-defs))
(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'make-color)
(straight-use-package 'markdown-mode)
(straight-use-package 'mic-paren)
;;(straight-use-package 'multiple-cursors)
(straight-use-package 'pandoc-mode)
(straight-use-package 'php-mode)
(straight-use-package 'powerline)
(straight-use-package 'prodigy)
;;(straight-use-package 'project-explorer)
(straight-use-package 'projectile-ripgrep)
(straight-use-package 'python-mode)
(straight-use-package 'rainbow-delimiters)

(use-package restclient
  :straight t
  :mode ("\\.rest\\'" . restclient-mode))

(straight-use-package 'sass-mode)
(straight-use-package 'smart-mode-line)
(straight-use-package 'smartparens)
(straight-use-package 'smooth-scrolling)
(straight-use-package 'solarized-theme)
;;(straight-use-package 'swiper)
(straight-use-package 'syslog-mode)
(straight-use-package 'tagedit)
(straight-use-package 'tern)
(straight-use-package 'todotxt)
(straight-use-package 'tramp-term)
;;(straight-use-package 'ansible-vault)
(straight-use-package 'yasnippet)
(straight-use-package 'logview)

(use-package git-link
:straight t
:custom (git-link-default-branch "develop"))

(use-package helpful
  :straight t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)))

(use-package ansible
  :straight (ansible :type git :host github :repo "k1LoW/emacs-ansible")
  :custom
  (ansible-vault-password-file "~/.vault-pass")
  :hook
  ((yaml-mode . (lambda () (ansible 1)))
   (ansible . ansible-auto-decrypt-encrypt)))

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :bind
  ("M-?" . lsp-bridge-find-references)
  :custom
  (acm-enable-capf t)
  (acm-enable-ctags t)
  (acm-candidate-match-function 'orderless-flex)
  ;;(acm-enable-lsp-workspace-symbol t)
  (acm-enable-copilot t))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (vertico-prescient-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  (setq completion-styles '(prescient basic))
  (setq vertico-sort-function #'prescient-completion-sort))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  )


;; Configure directory extension. Part of Vertico.
(use-package vertico-directory
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("s-s" . consult-ripgrep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;;(setq consult-preview-key (kbd "<right>"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :straight t
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package corfu-echo
;;   ;; :after corfu
;;   :straight nil
;;   :load-path "straight/repos/corfu/extensions/"
;;   :ensure nil
;;   ;; :init
;;   ;; (corfu-echo-mode)
;;   )

;; (use-package corfu
;;   :straight t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion

;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   :bind
;;   (:map corfu-map
;;         ("<right>" . corfu-quit)
;;         ("<left>" . corfu-quit))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :init
;;   (global-corfu-mode)
;;   (corfu-prescient-mode)
;;   (setq corfu-prescient-completion-styles '(prescient basic partial-completion))
;;   (corfu-echo-mode))

(use-package cape
  :straight t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (setq tab-always-indent 'complete)
  )

;; (use-package corfu-popupinfo
;;   :after corfu
;;   :straight nil
;;   :load-path "straight/repos/corfu/extensions/"
;;   :ensure nil
;;   :init
;;   (corfu-popupinfo-mode))

;;(straight-use-package 'undo-tree)

;; (use-package eglot
;;   :straight t
;;   :hook ((clojure-mode . eglot-ensure)
;;          (js2-mode . eglot-ensure))
;;   :init
;;   ;; Don't log every event for better performance
;;   (fset #'jsonrpc--log-event #'ignore)
;;   :custom
;;   (eglot-connect-timeout 300))

;; (use-package lsp-mode
;;   :straight t
;;   :init
;;   (setq lsp-use-plists t)
;;   (setq lsp-keymap-prefix "C-c l")
;;   (setq lsp-completion-provider :none)
;;   (setq gc-cons-threshold 100000000)
;;   (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;   (setq lsp-idle-delay 0.500)

;;   :hook ((clojure-mode . lsp)
;;          (clojurescript-mode . lsp)
;;          (js2-mode . lsp))
;;   :commands lsp)

(defun lsp-force-faces ()
  (interactive)
  (custom-set-faces
   '(lsp-flycheck-info-unnecessary-face ((t (:underline (:color "#2aa198" :style wave :position wave) :foreground "gray5"))) t)
   '(lsp-flycheck-warning-unnecessary-face ((t (:background "LightGoldenrod1" :foreground "gray30" :underline nil))) t)))

(defun lsp-organize-imports ()
  (interactive)
  (lsp-bridge-code-action "source.organizeImports"))

;; (defun corfu-lsp-setup ()
;;   (setq-local completion-styles '(orderless)
;;               completion-category-defaults nil))
;; (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package emacs
  :ensure nil
  :straight nil
  :demand t
  :hook ((text-mode prog-mode) . glyphless-display-mode)
  :config
  (set-face-background 'glyphless-char "red"))

(use-package vterm
  :straight t
  :config (add-to-list 'vterm-eval-cmds
                       '("update-pwd" (lambda (path)
                                        (setq default-directory path))))
  :custom (vterm-clear-scrollback-when-clearing t)
  :hook (vterm-mode . goto-address-mode))

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

(use-package xref-js2
  :straight t
  :config
  (setq xref-js2-search-program 'rg))

(straight-use-package 'xterm-color)
(straight-use-package 'yaml-mode)

;; (use-package all-the-icons-dired
;;   :straight t
;;   :hook (dired-mode . all-the-icons-dired-mode))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; From better defaults. For some reason the package version did not work.
;;(ido-mode t)
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)

;; (menu-bar-mode -1)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))

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
(global-set-key (kbd "C-M-.") 'xref-find-definitions-other-window)

;;(require 'undo-tree)
;;(global-undo-tree-mode)

;; (defun vertico-prescient-remember ()
;;   "Remember the chosen candidate with Prescient."
;;   (when (>= vertico--index 0)
;;     (prescient-remember
;;      (substring-no-properties
;;       (nth vertico--index vertico--candidates)))))
;; (advice-add #'vertico-insert :after #'vertico-prescient-remember)


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


(defun shell-command-on-buffer (command)
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command t))

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
;; (require 'ivy)
;; (ivy-mode 1)
;; (global-set-key (kbd "C-s") 'swiper)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "C-x r b") 'counsel-bookmark)

; Slim down ivy display
;; (setq ivy-count-format ""
;;       ivy-display-style nil
;;       ivy-minibuffer-faces nil)

;; Use Enter on a directory to navigate into the directory, not open it with dired.
;;(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

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

;; (with-eval-after-load 'flycheck
;;   (flycheck-popup-tip-mode))

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))


;; Company mode
;; (add-hook 'after-init-hook 'global-company-mode)
;; (company-quickhelp-mode 1)
;; (setq company-global-modes '(not eshell-mode))

;; Flyspell

;; (require 'flyspell-lazy)

;; (flyspell-lazy-mode 1)

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
  (yas-minor-mode 1) ; for adding require/use/import
  ;; (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; (require 'cljr-helm)
  ;; (define-key clojure-mode-map (kbd "C-c r") 'cljr-helm)
  (define-key clojure-mode-map (kbd "C-c C-a") 'clojure-align)
  (define-clojure-indent
   (fact 1)
   (fact* 1)
   (facts 1)
   (facts* 1)
   (for-frag 1)
   (pcond-> 1)
   (pcond->> 1)
   (reg-event-db 1)
   (reg-event-fx 1)
   (reg-sub 1)
   (reg-fx 1))
  (set-fill-column 90)
  ;;(setq clojure-align-separator "---")
  )

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; Cider

;;(require 'cider-eldoc)
(add-hook 'cider-mode-hook #'eldoc-mode)
;;(add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers nil)

(setq cider-show-error-buffer nil)
(setq cider-merge-sessions nil)
(setq nrepl-log-messages nil)

(setq cider-dynamic-indentation nil)

(add-hook 'cider-mode-hook
          (lambda ()
            (global-set-key (kbd "s-'")'cider-switch-to-repl-buffer)
            (global-set-key (kbd "s-å") (lambda () (interactive) (display-buffer "*cider-error*" nil nil)))
            (define-key cider-mode-map (kbd "C-c TAB") 'cider-format-defun)
            (define-key cider-mode-map (kbd "<C-tab>") 'cider-format-defun)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (eldoc-mode t)
            (cider-enable-cider-completion-style)
            (define-key cider-repl-mode-map (kbd "s-'") 'cider-switch-to-last-clojure-buffer)
            (define-key cider-repl-mode-map (kbd "C-:") 'clojure-toggle-keyword-string)
            (define-key cider-repl-mode-map (kbd "<S-return>") 'cider-repl-newline-and-indent)
            (define-key cider-repl-mode-map (kbd "s-*") 'cider-repl-toggle-pretty-printing)))

;; (defun java8 ()
;;   (interactive)
;;   (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/zulu-8.jdk/Contents/Home"))

;; (defun java11 ()
;;   (interactive)
;;   (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home"))

;; (defun java8-cider-jack-in ()
;;   (interactive)
;;   (java8)
;;   (cider-jack-in nil))

;; (defun java8-cider-jack-in-clj&cljs ()
;;   (interactive)
;;   (java8)
;;   (cider-jack-in-clj&cljs nil nil))

;; (defun java11-cider-jack-in ()
;;   (interactive)
;;   (java11)
;;   (cider-jack-in nil))

;; (defun java11-cider-jack-in-clj&cljs ()
;;   (interactive)
;;   (java11)
;;   (cider-jack-in-clj&cljs nil nil))


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
(global-set-key (kbd "H-p") 'projectile-find-file)
(global-set-key (kbd "<f13>") 'projectile-find-file)
(global-set-key (kbd "<f16>") 'projectile-find-file)

;(require 'project-explorer)
;(global-set-key (kbd "M-§")
;                (lambda () (interactive)
;                  (project-explorer-toggle)))

;; Counsel-projectile
;; (global-set-key (kbd "s-s")
;;                 (lambda () (interactive)
;;                   (counsel-projectile-rg)))
;; (global-set-key (kbd "H-p") 'counsel-projectile-find-file)
;; (global-set-key (kbd "<f13>") 'counsel-projectile-find-file)
;; (global-set-key (kbd "<f16>") 'counsel-projectile-find-file)


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
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . html-mode ))
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
  ;; (define-key tern-mode-keymap (kbd "M-.") nil)
  ;; (define-key tern-mode-keymap (kbd "M-,") nil)
  ;; (local-unset-key (kbd "C-c C-s"))
  ;; (local-unset-key (kbd "M-."))

  ;; ;; xref-js2
  ;; (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  (local-set-minor-mode-key 'smartparens-mode-map (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
  (local-set-minor-mode-key 'js-mode-map (kbd "s-o") 'js2-mode-toggle-element))

(add-hook 'js2-mode-hook 'my-js )

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
 '(all-the-icons-dired-monochrome nil)
 '(auto-revert-check-vc-info nil)
 '(auto-revert-interval 2)
 '(avy-all-windows nil)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-window-scrolls nil)
 '(beacon-color "DarkOrange1")
 '(beacon-mode nil)
 '(blink-cursor-mode nil)
 '(c-basic-offset 2)
 '(calendar-today-visible-hook '(calendar-mark-today org-journal-mark-entries))
 '(calendar-week-start-day 1)
 '(cider-download-java-sources t)
 '(cider-enrich-classpath nil)
 '(cider-offer-to-open-cljs-app-in-browser nil)
 '(cider-pprint-fn 'zprint)
 '(cider-print-fn 'fipp)
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-in-current-window t)
 '(cider-repl-use-pretty-printing nil)
 '(clojure-align-binding-forms
   '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
     "doseq" "for" "with-open" "with-local-vars" "with-redefs"
     "for-frag"))
 '(clojure-docstring-fill-column 90)
 '(clojure-indent-style 'align-arguments)
 '(coffee-tab-width 2)
 '(comint-process-echoes t)
 '(comint-prompt-read-only t)
 '(company-backends
   '(company-bbdb company-nxml company-css company-eclim company-semantic
                  company-clang company-xcode company-cmake
                  company-capf company-files
                  (company-dabbrev-code company-gtags company-etags
                                        company-keywords)
                  company-oddmuse company-dabbrev))
 '(company-dabbrev-code-modes
   '(prog-mode batch-file-mode csharp-mode css-mode erlang-mode
               haskell-mode jde-mode lua-mode python-mode js3-mode
               js2-mode scss-mode html-mode))
 '(company-idle-delay 0.2)
 '(company-prescient-mode t)
 '(company-selection-wrap-around t)
 '(compilation-message-face 'default)
 '(connection-local-criteria-alist
   '(((:application eshell) eshell-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "Ratina.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile (eshell-path-env-list))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
                         "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
                         "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . tramp-ps-time)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (ttname . string)
                                          (time . tramp-ps-time)
                                          (nice . number)
                                          (etime . tramp-ps-time)
                                          (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string)
                                          (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . number)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))))
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-type 'bar)
 '(custom-enabled-themes '(sanityinc-solarized-light))
 '(custom-safe-themes
   '("ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44"
     "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f"
     "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e"
     "d1dbd38c2fef808a27bb411ecff76a0a8026856a16cb2a1fb8820bedeb45740a"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     default))
 '(custom-theme-load-path
   '("/Users/vespesa/.emacs.d/elpa/color-theme-sanityinc-solarized-2.28/"
     "/Users/vespesa/.emacs.d/elpa/zenburn-theme-2.2"
     custom-theme-directory t) t)
 '(dired-dwim-target t)
 '(display-buffer-alist '(("\\*shell" display-buffer-same-window (nil))))
 '(eglot-code-action-indications '(mode-line))
 '(eglot-confirm-server-edits nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-events-buffer-config '(:size 0 :format short))
 '(eldoc-echo-area-use-multiline-p nil)
 '(exec-path
   '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"
     "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9"
     "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9"
     "/Applications/Emacs.app/Contents/MacOS/libexec"
     "/Applications/Emacs.app/Contents/MacOS/bin"))
 '(exec-path-from-shell-variables
   '("PATH" "CORE_MODE" "MANPATH" "GOOGLE_APPLICATION_CREDENTIALS"
     "JAVA_HOME" "LSP_USE_PLISTS"))
 '(explicit-shell-file-name nil)
 '(eyebrowse-wrap-around t)
 '(flycheck-disabled-checkers '(html-tidy))
 '(flyspell-issue-message-flag nil)
 '(fringe-mode '(1 . 1) nil (fringe))
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs"
     "{arch}" "node_modules" "public"))
 '(helm-buffer-max-length nil)
 '(helm-lisp-fuzzy-completion t)
 '(helm-mode nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-default-side 'same)
 '(helm-swoop-pre-input-function (lambda nil ""))
 '(helm-swoop-split-with-multiple-windows t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map (solarized-color-blend it "#fdf6e3" 0.25)
          '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900"
            "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50)
     ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0"
     "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"
     "#fdf6e3" "#fdf6e3"))
 '(ibuffer-formats
   '((mark modified read-only locked " " (name 40 40 :left :elide) " "
           (size 9 -1 :right) " " (mode 16 16 :left :elide) " "
           filename-and-process)
     (mark " " (name 16 -1) " " filename)))
 '(ido-cr+-auto-update-blacklist t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(insert-directory-program "gls")
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
 '(lsp-auto-guess-root t)
 '(lsp-enable-snippet nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-lens-enable nil)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(magit-gitflow-hotfix-finish-arguments nil)
 '(magit-no-confirm '(stage-all-changes))
 '(magit-use-overlays nil)
 '(manual-program "gman")
 '(nrepl-sync-request-timeout 60)
 '(ns-function-modifier 'hyper)
 '(orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes))
 '(org-agenda-files '("~/org/" "~/Documents/journal/"))
 '(org-agenda-window-setup 'other-window)
 '(org-capture-templates
   '(("t" "Todo" entry (file "~/org/notes.org") "* TODO %?")
     ("j" "Journal entry" entry (file+datetree "~/org/journal.org")
      "* %?")
     ("n" "Note in plain text" plain (file "~/org/notes.txt") "")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-journal-date-format "%A, %-e.%m.%Y")
 '(org-journal-hide-entries-p nil)
 '(org-startup-folded nil)
 '(org-support-shift-select t)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("Melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(undo-tree todotxt copy-as-format wconf counsel counsel-projectile
               php-mode sass-mode docker flycheck-popup-tip
               flycheck-clj-kondo defproject cider yaml-mode
               groovy-mode make-color deadgrep iedit tramp-term
               projectile-ripgrep xterm-color easy-kill deft hl-line+
               perspective beacon prodigy swiper ivy projector
               better-shell clojure-mode company magit python-mode
               smartparens tern ac-js2 xref-js2 web-mode syslog-mode
               solarized-theme smart-mode-line rainbow-delimiters
               powerline pandoc-mode multiple-cursors markdown-mode
               imenu-anywhere ido-ubiquitous ggtags
               exec-path-from-shell eval-sexp-fu dumb-jump
               color-theme-sanityinc-solarized color-identifiers-mode
               clojure-mode-extra-font-locking clojure-cheatsheet
               autopair align-cljlet ag ace-window))
 '(paradox-automatically-star nil)
 '(persp-show-modestring nil)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(prescient-persist-mode t)
 '(projectile-completion-system 'default)
 '(projectile-enable-idle-timer nil)
 '(projectile-indexing-method 'hybrid)
 '(projectile-mode-line nil)
 '(projectile-tags-command "")
 '(projectile-use-git-grep t)
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((cider-default-cljs-repl . shadow)
     (cider-lein-parameters . "with-profile +shadow repl :headless")
     (cider-offer-to-open-cljs-app-in-browser)
     (cider-default-cljs-repl . custom)
     (cider-shadow-watched-builds ":front")
     (cider-shadow-default-options . ":front")
     (cider-default-clj-repl . lein)
     (cider-default-cljs-repl . shadow-cljs)
     (eval font-lock-add-keywords nil
           `
           ((,(concat "("
                      (regexp-opt
                       '("sp-do-move-op" "sp-do-move-cl"
                         "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op"
                         "sp-do-del-cl")
                       t)
                      "\\_>")
             1 'font-lock-variable-name-face)))
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (eval setenv "JAVA_HOME"
           "/Library/Java/JavaVirtualMachines/openjdk-11.0.2.jdk/Contents/Home")
     (eval setenv "JAVA_HOME"
           "/Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home")
     (eval
      (setenv "JAVA_HOME"
              "/Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home"))
     (cider-default-cljs-repl . "figwheel")
     (cider-default-cljs-repl . figwheel)
     (cider-default-cljs-repl quote figwheel)
     (cider-default-cljs-repl . "Figwheel") (encoding . utf-8)))
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
   '((inferior-python-mode regexp "") (python-mode regexp "")
     (js3-mode regexp "") (js2-mode regexp "") (ruby-mode syntax "")
     (robot-mode regexp "") (scss-mode regexp "")))
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
 '(tooltip-mode nil)
 '(vterm-always-compile-module t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-current-column-highlight t)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00"
                 "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2"
                 "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83"
                 "#839496"))
 '(xref-search-program 'ripgrep))
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
 '(eglot-diagnostic-tag-unnecessary-face ((t (:background "LightGoldenrod1" :foreground "black"))))
 '(eshell-prompt ((t (:foreground "dark green" :weight normal))))
 '(flycheck-error ((t (:background "misty rose" :foreground "gray40" :underline nil))))
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
 '(lsp-bridge-ref-font-lock-function-location ((t (:foreground "green4" :weight bold))))
 '(lsp-bridge-ref-font-lock-header-line-edit-mode ((t (:foreground "dark blue" :weight bold))))
 '(lsp-bridge-ref-font-lock-header-line-text ((t (:foreground "SpringGreen4" :weight bold))))
 '(lsp-bridge-ref-font-lock-match ((t (:foreground "firebrick3" :weight bold))))
 '(lsp-flycheck-info-unnecessary-face ((t (:underline (:color "#2aa198" :style wave :position wave) :foreground "gray5"))) t)
 '(lsp-flycheck-warning-unnecessary-face ((t (:background "LightGoldenrod1" :foreground "gray30" :underline nil))) t)
 '(mode-line ((t (:background "papaya whip" :foreground "black" :box (:line-width 1 :color "#657b83") :weight normal))))
 '(rainbow-delimiters-unmatched-face ((t (:background "dark red" :foreground "white"))))
 '(region ((t (:background "light cyan" :inverse-video nil))))
 '(secondary-selection ((t (:extend t :background "LightBlue1"))))
 '(vertico-current ((t (:extend t :background "LightBlue1"))))
 '(vterm-color-white ((t (:background "DarkOrange1" :foreground "DarkOrange1"))))
 '(web-mode-comment-face ((t (:foreground "dark blue" :slant normal))))
 '(web-mode-current-column-highlight-face ((t (:background "bisque")))))
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
