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
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backup/")))
 '(before-save-hook '(whitespace-cleanup))
 '(cider-repl-display-help-banner nil)
 '(cider-repl-pop-to-buffer-on-connect 'display-only)
 '(column-number-mode t)
 '(completion-category-overrides '((file (styles partial-completion))))
 '(completion-styles '(orderless basic))
 '(context-menu-mode t)
 '(custom-enabled-themes '(alabaster))
 '(custom-safe-themes
   '("3846f91527bdc9505142b195726d59aeeabb9ecb236267d3cbf94a5235b34bd3"))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-aFGhlv --group-directories-first")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(dired-use-ls-dired t)
 '(ediff-split-window-function 'split-window-sensibly)
 '(eglot-connect-timeout 300)
 '(enable-recursive-minibuffers t)
 '(fill-column 80)
 '(frame-inhibit-implied-resize nil)
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
 '(make-backup-files t)
 '(mode-line-compact 'long)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-tilt-scroll t)
 '(native-comp-async-report-warnings-errors nil)
 '(org-special-ctrl-a/e 'reversed)
 '(package-archive-priorities '(("melpa" . 30) ("gnu" . 20) ("nongnu" . 10)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(standard-themes lua-ts-mode corfu rainbow-mode a-janet-spork-client ajrepl cider clojure-mode dumb-jump eglot elixir-mode embark exec-path-from-shell fennel-mode glsl-mode iedit inf-clojure inf-janet janet-mode janet-ts-mode jarchive keychain-environment lua-mode magit mlscroll modus-themes orderless rg sly vertico which-key yaml-mode zig-mode))
 '(package-vc-selected-packages
   '((lua-ts-mode :vc-backend Git :url "https://git.sr.ht/~johnmuhl/lua-ts-mode")
     (janet-ts-mode :vc-backend Git :url "https://github.com/sogaiu/janet-ts-mode.git")
     (ajrepl :vc-backend Git :url "https://github.com/sogaiu/ajrepl.git")
     (a-janet-spork-client :vc-backend Git :url "https://github.com/sogaiu/a-janet-spork-client.git")
     (inf-janet :vc-backend Git :url "https://github.com/velkyel/inf-janet.git")))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook '(toggle-truncate-lines electric-pair-mode))
 '(repeat-mode t)
 '(ring-bell-function 'flash-mode-line)
 '(save-place-mode t)
 '(savehist-mode t)
 '(savehist-save-minibuffer-history t)
 '(scroll-preserve-screen-position t)
 '(search-whitespace-regexp ".*?")
 '(selectrum-mode nil)
 '(show-paren-context-when-offscreen 'overlay)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(version-control t)
 '(vertico-mode t)
 '(which-key-mode t)
 '(x-underline-at-descent-line t)
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

(defun completing-read-at-point (start end col &optional pred)
  "`completing-at-point' using `completing-read' (inside minibuffer).
Inspired by https://github.com/katspaugh/ido-at-point"
  (if (minibufferp) (completion--in-region start end col pred)
    (let* ((init (buffer-substring-no-properties start end))
           (all (completion-all-completions init col pred (length init)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all))) (car all))
                        (t (completing-read "Completions: " col pred t init)))))
      (if completion
          (progn
            (delete-region start end)
            (insert completion)
            t)
        (message "No completions") nil))))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq completion-in-region-function #'completing-read-at-point)

(add-hook 'clojure-mode-hook 'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)

(autoload #'embark-next-symbol "embark" nil t)
(autoload #'embark-previous-symbol "embark" nil t)

(global-set-key [remap move-beginning-of-line] 'move-beginning-of-line+)
(keymap-global-set "M-n" 'embark-next-symbol)
(keymap-global-set "M-p" 'embark-previous-symbol)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-i" 'delete-other-windows)
(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "M-Z" 'zap-to-char)
(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-x k" 'kill-this-buffer)
(keymap-global-set "C-M-r" 'raise-sexp)
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "C-;" 'iedit-mode)
(keymap-global-set "C-c g" 'grep-find)
(keymap-global-unset "C-z")
(keymap-global-set "C-h L" #'find-library)
(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "C-o" 'isearch-occur))
(global-set-key [remap kill-region] 'backward-kill-word-or-region)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(with-eval-after-load 'project
  (require 'magit-extras))

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

(with-eval-after-load 'fennel-mode
  (keymap-unset fennel-mode-map "M-.")
  (keymap-unset fennel-mode-map "M-,"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(lua-mode "lua-language-server"))
  (add-to-list 'eglot-server-programs '(lua-ts-mode "lua-language-server"))
  )

(with-eval-after-load 'cider
  (cider-register-cljs-repl-type 'sci-js)

  (defun cider-setup-sci-js-cljs-repl ()
    (when (eq 'sci-js cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))

  (add-hook 'cider-connected-hook #'cider-setup-sci-js-cljs-repl))

(with-eval-after-load 'lua-mode
  (add-hook 'lua-mode-hook #'eglot-ensure))

(with-eval-after-load 'lua-ts-mode
  (add-hook 'lua-ts-mode-hook #'eglot-ensure))

(provide 'init)

;;; init.el ends here
