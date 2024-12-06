;;; init.el --- Daw-Ran's emacs configurations -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;

;;; Code:

;; Opt out customization interface
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; package.el
;; Initialize and refresh package contents again if needed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; After init hooks
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'window-divider-mode)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(defun flash-mode-line ()
  "Flash the modeline on error or warning.
https://macowners.club/posts/custom-functions-4-ui/"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(advice-add #'kill-ring-save :before #'pulse-momentary-highlight-region)

;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun move-beginning-of-line+ (arg)
  "Move point to beginning of current line or the first non whitespace char."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Move by lines, if ARG is not 1 (the default).
  (if (/= arg 1)
      (let ((line-move-visual nil))
        (line-move (1- arg) t)))
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

(defun backward-kill-word-or-region (&optional arg)
  "When region is active, kill the region. Otherwise, kill word
backwards."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word (or arg 1))))

(defun remove-treesit-sexp-changes ()
  (keymap-unset (current-local-map) "<remap> <beginning-of-defun>")
  (keymap-unset (current-local-map) "<remap> <end-of-defun>")
  (when (eq forward-sexp-function #'treesit-forward-sexp)
    (setq forward-sexp-function nil))
  (when (eq transpose-sexps-function #'treesit-transpose-sexps)
    (setq transpose-sexps-function #'transpose-sexps-default-function))
  (when (eq forward-sentence-function #'treesit-forward-sentence)
    (setq forward-sentence-function #'forward-sentence-default-function)))

(defun fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun eshell-toggle (exit)
  "Bring up a full-screen eshell or restore previous config.
With a prefix argument, exit eshell before restoring previous config."
  (interactive "P")
  (if (string= "eshell-mode" major-mode)
      (progn
        (when exit
          (insert "exit")
          (eshell-send-input))
        (jump-to-register :eshell-fullscreen))
    (window-configuration-to-register :eshell-fullscreen)
    (eshell)
    (delete-other-windows)))

(defun find-current-file-as-root ()
  "Reopen current file as root"
  (interactive)
  (set-visited-file-name (concat "/sudoedit::" (buffer-file-name)))
  (setq buffer-read-only nil))

(add-hook 'clojure-mode-hook 'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)

(autoload #'embark-next-symbol "embark" nil t)
(autoload #'embark-previous-symbol "embark" nil t)

;;; Keybindings

(keymap-global-set "<f5>" #'eshell-toggle)
(keymap-global-set "<remap> <move-beginning-of-line>" 'move-beginning-of-line+)
(keymap-global-set "M-n" 'embark-next-symbol)
(keymap-global-set "M-p" 'embark-previous-symbol)
(keymap-global-set "M-q" #'fill-or-unfill)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-i" 'delete-other-windows)
(keymap-global-set "M-j" #'avy-goto-char-timer) ; was #'default-indent-new-line
(keymap-global-set "C-c j" #'avy-goto-line)
(keymap-global-set "C-c z" #'compile)
(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "M-Z" 'zap-to-char)
(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-x k" 'kill-this-buffer)
(keymap-global-set "C-M-r" 'raise-sexp)
(keymap-global-set "C-S-t" #'scratch-buffer)
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "C-;" 'iedit-mode)
(keymap-global-set "C-g" #'keyboard-quit-dwim)
(keymap-global-set "C-c g" 'grep-find)
(keymap-global-unset "C-z")
(keymap-global-set "C-h L" #'find-library)
(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "C-o" 'isearch-occur))
(keymap-global-set "<remap> <kill-region>" 'backward-kill-word-or-region)
(keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)
(keymap-global-set "s-{" #'tab-previous)
(keymap-global-set "s-}" #'tab-next)

;; Consult
;; C-c bindings in `mode-specific-map'
(keymap-global-set "C-c M-x" #'consult-mode-command)
(keymap-global-set "<remap> <Info-search>" #'consult-info)
(keymap-global-set "C-M-#" #'consult-register)
(keymap-global-set "C-c M-x" #'consult-mode-command)
(keymap-global-set "C-c h" #'consult-history)
(keymap-global-set "C-c i" #'consult-info)
(keymap-global-set "C-c k" #'consult-kmacro)
(keymap-global-set "C-c m" #'consult-man)

;; C-x bindings in `ctl-x-map'
(keymap-global-set "C-x 4 b" #'consult-buffer-other-window) ; orig. switch-to-buffer-other-window
(keymap-global-set "C-x 5 b" #'consult-buffer-other-frame) ; orig. switch-to-buffer-other-frame
(keymap-global-set "C-x M-:" #'consult-complex-command) ; orig. repeat-complex-command
(keymap-global-set "C-x b" #'consult-buffer)            ; orig. switch-to-buffer
(keymap-global-set "C-x p b" #'consult-project-buffer) ; orig. project-switch-to-buffer
(keymap-global-set "C-x r b" #'consult-bookmark)       ; orig. bookmark-jump
(keymap-global-set "C-x t b" #'consult-buffer-other-tab) ; orig. switch-to-buffer-other-tab

;; Custom M-# bindings for fast register access
(keymap-global-set "C-M-#" #'consult-register)
(keymap-global-set "M-#" #'consult-register-load)
(keymap-global-set "M-'" #'consult-register-store) ; orig. abbrev-prefix-mark (unrelated)
(setq register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)

;; Other custom bindings
(keymap-global-set "M-y" #'consult-yank-pop) ; was #'yank-pop

;; M-g bindings in `goto-map'
(keymap-global-set "M-g I" #'consult-imenu-multi)
(keymap-global-set "M-g M-g" #'consult-goto-line) ; orig. goto-line
(keymap-global-set "M-g e" #'consult-compile-error)
(keymap-global-set "M-g f" #'consult-flymake)   ; Alternative: consult-flycheck
(keymap-global-set "M-g g" #'consult-goto-line) ; orig. goto-line
(keymap-global-set "M-g i" #'consult-imenu)
(keymap-global-set "M-g k" #'consult-global-mark)
(keymap-global-set "M-g m" #'consult-mark)
(keymap-global-set "M-g o" #'consult-outline) ; Alternative: consult-org-heading

;; M-s bindings in `search-map'
(keymap-global-set "M-s G" #'consult-git-grep)
(keymap-global-set "M-s L" #'consult-line-multi)
(keymap-global-set "M-s c" #'consult-locate)
(keymap-global-set "M-s d" #'consult-find) ; Alternative: consult-fd
(keymap-global-set "M-s g" #'consult-grep)
(keymap-global-set "M-s k" #'consult-keep-lines)
(keymap-global-set "M-s l" #'consult-line)
(keymap-global-set "M-s o" #'consult-outline) ; was #'occur
(keymap-global-set "M-s r" #'consult-ripgrep)
(keymap-global-set "M-s u" #'consult-focus-lines)

;; Isearch integration
(keymap-global-set "M-s e" #'consult-isearch-history)
(keymap-set isearch-mode-map "M-e" #'consult-isearch-history) ; orig. isearch-edit-string
(keymap-set isearch-mode-map "M-s e" #'consult-isearch-history) ; orig. isearch-edit-string
(keymap-set isearch-mode-map "M-s l" #'consult-line) ; needed by consult-line to detect isearch
(keymap-set isearch-mode-map "M-s L" #'consult-line-multi) ; needed by consult-line to detect isearch

;; Minibuffer history
(keymap-set minibuffer-local-map "M-s" #'consult-history) ; orig. next-matching-history-element
(keymap-set minibuffer-local-map "M-r" #'consult-history) ; orig. previous-matching-history-element

(use-package dired
  :defer t
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode)))

(use-package dired-subtree
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :defer t
  :after dired
  :hook ((dired-mode . nerd-icons-dired-mode)))

(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :defer t
  :config
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq completion-styles '(orderless basic)))

(use-package consult
  :ensure t
  :defer t
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

(use-package corfu
  :ensure t
  :defer t
  :hook ((after-init . global-corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-mode-map
              ("SPC" . corfu-insert-separator))
  :config
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package cape
  :ensure t
  :defer t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package project
  :defer t
  :config
  (require 'magit-extras))

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter))

(keychain-refresh-environment)

(use-package rect
  :defer t
  :config
  ;; https://gist.github.com/jdtsmith/bfa2d692c4fbbffe06b558e4bcf9abec
  (with-eval-after-load 'rect
    (cl-loop for (key def) in
             '(("k" kill-rectangle)       ("t" string-rectangle)
               ("o" open-rectangle)       ("w" copy-rectangle-as-kill)
               ("y" yank-rectangle)       ("c" clear-rectangle)
               ("d" delete-rectangle)     ("N" rectangle-number-lines)
               (" " delete-whitespace-rectangle)
               ("=" calc-grab-sum-across) ("+" calc-grab-sum-down)
               ("#" calc-grab-rectangle)  ("n" set-mark-command)
               ("q" (lambda () (interactive) (deactivate-mark)))
               ("?" (lambda () (interactive)
                      (embark-bindings-in-keymap rectangle-mark-mode-map))))
             do (define-key rectangle-mark-mode-map key def))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package xref
  :defer t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eshell
  :defer t
  :hook ((eshell-first-time-mode . eat-eshell-visual-command-mode)
         (eshell-first-time-mode . eat-eshell-mode)))

(use-package avy
  :ensure t
  :defer t
  :config
  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select. https://github.com/ebittleman/emacs-bedrock
  (setf (alist-get ?. avy-dispatch-alist)
        (defun avy-action-embark (pt)
          (unwind-protect
              (save-excursion
                (goto-char pt)
                (embark-act))
            (select-window
             (cdr (ring-ref avy-ring 0)))
            t))))

(use-package jinx
  :ensure t
  :defer t
  :hook (after-init . global-jinx-mode))

(use-package fennel-mode
  :ensure t
  :defer t
  :bind (:map fennel-mode-map
              ("M-.")
              ("M-,")))

(use-package eglot
  :defer t
  :config
  (eglot-booster-mode)
  (jarchive-mode)
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event

  (add-to-list 'eglot-server-programs '(lua-mode "lua-language-server"))
  (add-to-list 'eglot-server-programs '(lua-ts-mode "lua-language-server"))

  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend))))
  (add-hook 'cider-mode-hook #'eglot-disable-in-cider)
  (add-hook 'eglot-managed-mode-hook #'eglot-disable-in-cider)
  )

(use-package cider
  :ensure t
  :defer t
  :config
  (cider-register-cljs-repl-type 'sci-js)

  (defun cider-setup-sci-js-cljs-repl ()
    (when (eq 'sci-js cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))

  (add-hook 'cider-connected-hook #'cider-setup-sci-js-cljs-repl))

(use-package janet-ts-mode
  :ensure t
  :defer t
  :hook (janet-ts-mode . remove-treesit-sexp-changes)
  ;; (add-hook 'janet-ts-mode-hook #'ajrepl-interaction-mode)
  ;; (add-hook 'janet-ts-mode-hook #'ajsc-interaction-mode)
  )

(use-package ajrepl
  :ensure t
  :defer t
  :config
  (defun ajrepl-send-defun ()
    (interactive)
    (save-excursion
      (mark-defun)
      (call-interactively #'ajrepl-send-region)))
  (keymap-set ajrepl-interaction-mode-map "C-c C-c" #'ajrepl-send-defun))

(provide 'init)

;;; init.el ends here
