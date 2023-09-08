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
 '(backup-by-copying t)
 '(backup-directory-alist
   `(("." \,
      (expand-file-name "backup/" user-emacs-directory))))
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
 '(default-frame-alist '((font . "Iosevka-12")))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-aFGhlv --group-directories-first")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(dired-use-ls-dired t)
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
 '(magit-diff-refine-hunk 'all)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(major-mode-remap-alist
   '((sh-mode . bash-ts-mode)
     (clojurec-mode . clojure-ts-mode)
     (clojurescript-mode . clojure-ts-mode)
     (clojure-mode . clojure-ts-mode)
     (css-mode . css-ts-mode)
     (go-mode . go-ts-mode)
     (go-dot-mod-mode . go-mod-ts-mode)
     (mhtml-mode . html-ts-mode)
     (sgml-mode . html-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (yaml-mode . yaml-ts-mode)))
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
   '(jinx clojure-ts-mode racket-mode dumb-jump olivetti zig-mode fennel-mode lua-mode mlscroll keychain-environment emmet-mode which-key eglot cider clojure-mode vertico sly embark iedit magit markdown-mode orderless rainbow-mode rg yaml-mode))
 '(package-vc-selected-packages
   '((clojure-ts-mode :vc-backend Git :url "https://github.com/clojure-emacs/clojure-ts-mode")))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook '(toggle-truncate-lines))
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
(add-hook 'clojure-ts-mode-hook 'eglot-ensure)
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
(keymap-global-set "<remap> <ispell-word>" #'jinx-correct) ; M-$
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

;; https://robbmann.io/posts/emacs-treesit-auto/
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure.git")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (commonlisp "https://github.com/thehamsta/tree-sitter-commonlisp")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (r "https://github.com/r-lib/tree-sitter-r")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (zig "https://github.com/GrayJack/tree-sitter-zig")))

(defun install-latest-known-treesitter-grammars ()
  "Install/upgrade all latest Tree-sitter grammars."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (message "Downloading %s treesitter grammar from %s" (car grammar) (cadr grammar))
    (treesit-install-language-grammar (car grammar))))

(with-eval-after-load 'clojure-ts-mode
  (require 'clojure-mode)
  (setq clojure-ts-mode-syntax-table clojure-mode-syntax-table)
  (add-hook 'clojure-ts-mode-hook #'clojure-mode-variables)

  ;; Copied from cider.el
  (keymap-set clojure-ts-mode-map "C-c M-x" #'cider)
  (keymap-set clojure-ts-mode-map "C-c M-j" #'cider-jack-in-clj)
  (keymap-set clojure-ts-mode-map "C-c M-J" #'cider-jack-in-cljs)
  (keymap-set clojure-ts-mode-map "C-c M-c" #'cider-connect-clj)
  (keymap-set clojure-ts-mode-map "C-c M-C" #'cider-connect-cljs)
  (keymap-set clojure-ts-mode-map "C-c C-x" 'cider-start-map)
  (keymap-set clojure-ts-mode-map "C-c C-s" 'sesman-map)
  (require 'sesman)
  (sesman-install-menu clojure-ts-mode-map)
  (add-hook 'clojure-ts-mode-hook
            (lambda ()
              (setq-local sesman-system 'CIDER)))
  (add-hook 'clojure-ts-mode-hook #'remove-treesit-sexp-changes)
  (add-hook 'clojure-ts-mode-hook #'clojure-mode-variables)
  (add-hook 'clojure-ts-mode-hook #'cider-mode)

  ;; Support for C-c C-z repl switching
  (defun cider-repl-type-for-buffer-in-clojure-ts-mode (&optional buffer)
    "Determine repl type for clojure-ts-mode buffers."
    (with-current-buffer (or buffer (current-buffer))
      (when (and buffer-file-name (derived-mode-p 'clojure-ts-mode))
        (pcase (file-name-extension buffer-file-name)
          ("cljs" 'cljs)
          ("cljc" 'multi)
          ("clj" 'clj)))))

  (advice-add #'cider-repl-type-for-buffer
              ;; Fallback to the advice when cider fails to find it
              :after-until
              #'cider-repl-type-for-buffer-in-clojure-ts-mode)
  (add-hook 'clojure-ts-mode-hook #'clojure-mode-variables)

  ;; Share same lsp instance for all clojure major modes
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((clojure-mode
                                           clojurescript-mode
                                           clojurec-mode
                                           clojure-ts-mode)
                                          . ("clojure-lsp")))))

(provide 'init)

;;; init.el ends here
