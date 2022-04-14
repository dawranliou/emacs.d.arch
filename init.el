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
 '(corfu-global-mode t)
 '(custom-enabled-themes '(modus-operandi))
 '(default-frame-alist '((font . "Iosevka-11") (width . 80) (height . 35)))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(dired-use-ls-dired t)
 '(eglot-connect-timeout 300)
 '(enable-recursive-minibuffers t)
 '(fill-column 80)
 '(frame-inhibit-implied-resize nil)
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
 '(menu-bar-mode nil)
 '(mode-line-compact 'long)
 '(native-comp-async-report-warnings-errors nil)
 '(package-archive-priorities '(("melpa" . 30) ("gnu" . 20) ("nongnu" . 10)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(emmet-mode which-key eglot cider clojure-mode vertico sly embark iedit magit markdown-mode orderless rainbow-mode rg smartscan yaml-mode corfu))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook '(electric-pair-mode))
 '(recentf-max-saved-items 200)
 '(repeat-mode t)
 '(ring-bell-function #'ignore)
 '(save-place-mode t)
 '(savehist-mode t)
 '(savehist-save-minibuffer-history t)
 '(selectrum-mode nil)
 '(tool-bar-mode nil)
 '(vertico-mode t)
 '(which-key-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(add-hook 'clojure-mode-hook 'eglot-ensure)

(global-set-key [remap move-beginning-of-line] 'move-beginning-of-line+)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-i" 'delete-other-windows)
(keymap-global-set "C-x k" 'kill-this-buffer)
(keymap-global-set "C-M-r" 'raise-sexp)
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "C-c r" 'rg)
(keymap-global-set "C-w" 'backward-kill-word-or-region)

(with-eval-after-load 'project
  (require 'magit-extras))

(provide 'init)

;;; init.el ends here
