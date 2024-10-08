;;; init.el --- Daw-Ran's emacs configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(package-activate-all)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-avoid-polling t)
 '(auto-revert-check-vc-info t)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backup/")))
 '(cider-repl-display-help-banner nil)
 '(cider-repl-pop-to-buffer-on-connect 'display-only)
 '(column-number-mode t)
 '(completion-auto-help 'always)
 '(completion-auto-select 'second-tab)
 '(completion-category-overrides '((file (styles partial-completion))))
 '(completion-cycle-threshold 1)
 '(completion-styles '(orderless basic))
 '(completions-detailed t)
 '(completions-format 'one-column)
 '(completions-group t)
 '(completions-max-height 20)
 '(consult-narrow-key "<")
 '(context-menu-mode t)
 '(copy-region-blink-delay 0)
 '(custom-enabled-themes '(standard-light))
 '(custom-safe-themes
   '("cfb0bb44259cf576124b900613628f4b589df1c3860185d24fffde6666bc88cf" default))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-aFGhlv --group-directories-first")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(dired-use-ls-dired t)
 '(display-buffer-alist
   '(("\\*Occur\\*"
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (window-height . fit-window-to-buffer)
      (dedicated . t))))
 '(ediff-split-window-function 'split-window-sensibly)
 '(eglot-connect-timeout 300)
 '(eglot-events-buffer-size 0)
 '(eglot-extend-to-xref t)
 '(enable-recursive-minibuffers t)
 '(fill-column 80)
 '(frame-inhibit-implied-resize nil)
 '(global-corfu-mode t)
 '(global-jinx-mode t)
 '(grep-find-command '("rg -n -H --no-heading --glob='' -e ''" . 37))
 '(history-length 200000)
 '(ido-enable-flex-matching nil)
 '(ido-everywhere nil)
 '(ido-mode nil nil (ido))
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "/usr/bin/sbcl")
 '(inhibit-startup-screen t)
 '(isearch-allow-motion t)
 '(isearch-allow-scroll 'unlimited)
 '(isearch-lazy-count t)
 '(isearch-wrap-pause 'no)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(lua-indent-level 2)
 '(lua-indent-nested-block-content-align t)
 '(lua-ts-indent-offset 2)
 '(magit-diff-refine-hunk 'all)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(major-mode-remap-alist '((janet-mode . janet-ts-mode)))
 '(make-backup-files t)
 '(marginalia-mode t)
 '(mode-line-compact 'long)
 '(modus-themes-mixed-fonts t)
 '(mouse-wheel-flip-direction nil)
 '(mouse-wheel-tilt-scroll t)
 '(native-comp-async-report-warnings-errors nil)
 '(org-special-ctrl-a/e 'reversed)
 '(package-archive-priorities '(("melpa" . 10) ("gnu" . 30) ("nongnu" . 20)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(eglot-booster tb-keycast jinx eat marginalia cape embark-consult consult avy standard-themes lua-ts-mode corfu rainbow-mode a-janet-spork-client ajrepl cider clojure-mode dumb-jump eglot elixir-mode embark exec-path-from-shell fennel-mode glsl-mode iedit inf-janet janet-mode janet-ts-mode jarchive keychain-environment lua-mode magit mlscroll modus-themes orderless sly vertico which-key yaml-mode zig-mode))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")
     (tb-keycast :vc-backend Git :url "https://github.com/ir33k/tb-keycast.git")
     (lua-ts-mode :url "https://git.sr.ht/~johnmuhl/lua-ts-mode" :vc-backend Git)
     (janet-ts-mode :url "https://github.com/sogaiu/janet-ts-mode.git" :vc-backend Git)
     (ajrepl :url "https://github.com/sogaiu/ajrepl.git" :vc-backend Git)
     (a-janet-spork-client :url "https://github.com/sogaiu/a-janet-spork-client.git" :vc-backend Git)))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook '(toggle-truncate-lines electric-pair-mode))
 '(register-preview-delay 0.5)
 '(repeat-mode t)
 '(ring-bell-function 'flash-mode-line)
 '(save-place-mode t)
 '(savehist-mode t)
 '(savehist-save-minibuffer-history t)
 '(scroll-preserve-screen-position t)
 '(search-whitespace-regexp ".*?")
 '(selectrum-mode nil)
 '(show-paren-context-when-offscreen 'overlay)
 '(show-trailing-whitespace t)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(tab-bar-auto-width nil)
 '(tab-bar-close-button-show nil)
 '(tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
 '(tab-bar-new-tab-choice "*scratch*")
 '(tab-bar-tab-hints t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(version-control t)
 '(vertico-mode t)
 '(wgrep-auto-save-buffer t)
 '(which-key-mode t)
 '(windmove-default-keybindings '([ignore] control))
 '(x-stretch-cursor t)
 '(x-underline-at-descent-line nil)
 '(xref-search-program 'ripgrep)
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(defun flash-mode-line ()
  "Flash the modeline on error or warning.
https://macowners.club/posts/custom-functions-4-ui/"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(advice-add #'kill-ring-save :before #'pulse-momentary-highlight-region)

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
(keymap-global-set "C-c c" #'compile)
(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "M-Z" 'zap-to-char)
(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-x k" 'kill-this-buffer)
(keymap-global-set "C-M-r" 'raise-sexp)
(keymap-global-set "C-S-t" #'scratch-buffer)
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "C-;" 'iedit-mode)
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

(with-eval-after-load 'consult
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

(with-eval-after-load 'corfu
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (keymap-set corfu-mode-map "SPC" #'corfu-insert-separator)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)

(with-eval-after-load 'project
  (require 'magit-extras))

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(keychain-refresh-environment)

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
           do (define-key rectangle-mark-mode-map key def)))

(with-eval-after-load 'xref
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(with-eval-after-load 'eshell
  (add-hook 'eshell-first-time-mode-hook
            #'eat-eshell-visual-command-mode)
  (add-hook 'eshell-first-time-mode-hook
            #'eat-eshell-mode))

;; After invoking avy-goto-char-timer, hit "." to run embark at the next
;; candidate you select. https://github.com/ebittleman/emacs-bedrock
(with-eval-after-load 'avy
  (setf (alist-get ?. avy-dispatch-alist)
        (defun avy-action-embark (pt)
          (unwind-protect
              (save-excursion
                (goto-char pt)
                (embark-act))
            (select-window
             (cdr (ring-ref avy-ring 0)))
            t))))

(with-eval-after-load 'fennel-mode
  (keymap-unset fennel-mode-map "M-.")
  (keymap-unset fennel-mode-map "M-,"))

(with-eval-after-load 'eglot
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

(with-eval-after-load 'cider
  (cider-register-cljs-repl-type 'sci-js)

  (defun cider-setup-sci-js-cljs-repl ()
    (when (eq 'sci-js cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))

  (add-hook 'cider-connected-hook #'cider-setup-sci-js-cljs-repl))

(with-eval-after-load 'janet-mode
  ;; (require 'inf-janet)
  ;; (remove-hook 'janet-mode-hook #'inf-janet-minor-mode)
  ;; (keymap-set inf-janet-minor-mode-map "C-c C-b" #'inf-janet-eval-buffer)

  ;; (require 'ajrepl)
  ;; (add-hook 'janet-mode-hook #'ajrepl-interaction-mode)
  ;; (require 'ajsc)
  )

(with-eval-after-load 'janet-ts-mode
  (add-hook 'janet-ts-mode-hook #'remove-treesit-sexp-changes)
  ;; (add-hook 'janet-ts-mode-hook #'ajrepl-interaction-mode)
  ;; (add-hook 'janet-ts-mode-hook #'ajsc-interaction-mode)
  )

(with-eval-after-load 'ajrepl
  (defun ajrepl-send-defun ()
    (interactive)
    (save-excursion
      (mark-defun)
      (call-interactively #'ajrepl-send-region)))
  (keymap-set ajrepl-interaction-mode-map "C-c C-c" #'ajrepl-send-defun))

(with-eval-after-load 'lua-mode
  (add-hook 'lua-mode-hook #'eglot-ensure))

(with-eval-after-load 'lua-ts-mode
  (add-hook 'lua-ts-mode-hook #'eglot-ensure))

(add-hook 'focus-out-hook #'garbage-collect)

(provide 'init)

;;; init.el ends here
