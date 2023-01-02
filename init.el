;;; init.el --- Daw-Ran's emacs configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

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
 '(completion-styles '(orderless))
 '(context-menu-mode t)
 '(default-frame-alist '((font . "Iosevka-12")))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-lAXhv --group-directories-first")
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
 '(isearch-wrap-pause 'no)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(magit-diff-refine-hunk 'all)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(make-backup-files t)
 '(mode-line-compact 'long)
 '(mode-line-format
   '("" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
     (:propertize " %b" face mode-line-buffer-id)
     " %l:%c (%p)" " ["
     ("" mode-name)
     "]" mode-line-misc-info mode-line-end-spaces))
 '(native-comp-async-report-warnings-errors nil)
 '(org-special-ctrl-a/e 'reversed)
 '(package-archive-priorities '(("melpa" . 30) ("gnu" . 20) ("nongnu" . 10)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(zig-mode fennel-mode lua-mode mlscroll keychain-environment emmet-mode which-key eglot cider clojure-mode vertico sly embark iedit magit markdown-mode orderless rainbow-mode rg yaml-mode))
 '(pixel-scroll-precision-mode t)
 '(repeat-mode t)
 '(ring-bell-function #'ignore)
 '(save-place-mode t)
 '(savehist-mode t)
 '(savehist-save-minibuffer-history t)
 '(search-whitespace-regexp ".*?")
 '(selectrum-mode nil)
 '(tool-bar-mode nil)
 '(vertico-mode t)
 '(which-key-mode t)
 '(xref-search-program 'ripgrep))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch-serif ((t (:height 1.4 :family "Monospace Serif")))))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

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

(setq completion-in-region-function #'completing-read-at-point)

(add-hook 'clojure-mode-hook 'eglot-ensure)

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
(keymap-global-set "C-x k" 'kill-this-buffer)
(keymap-global-set "C-M-r" 'raise-sexp)
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "C-c r" 'rg)
(keymap-global-set "C-;" 'iedit-mode)
(keymap-global-set "C-c g" 'grep-find)
(keymap-global-unset "C-z")
(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "C-o" 'isearch-occur))
(global-set-key [remap kill-region] 'backward-kill-word-or-region)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(with-eval-after-load 'project
  (require 'magit-extras))

(keychain-refresh-environment)

(provide 'init)

;;; init.el ends here
