;;; early-init.el  -*- no-byte-compile: t; lexical-binding: t; -*-

;;; GC
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (setq gc-cons-threshold (* 16 1024 1024))))

;;; Misc

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; Determine the state of bundled libraries using calc-loaddefs.el.
     ;; If compressed, retain the gzip handler in `file-name-handler-alist`.
     ;; If compiled or neither, omit the gzip handler during startup for
     ;; improved startup and package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Ensure the new value persists through any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist
                                file-name-handler-alist)
    ;; Remember the old value to reset it as needed.
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite to preserve any changes made
                 ;; since startup.
                 (delete-dups (append file-name-handler-alist old-value))))
              101))

  (unless noninteractive
    (setq-default inhibit-message t)

    ;; Reset the above variables to prevent Emacs from appearing frozen or
    ;; visually corrupted after startup or if a startup error occurs.
    (defun minimal-emacs--reset-inhibited-vars-h ()
      ;; (setq-default inhibit-redisplay nil) ; Can cause artifacts
      (setq-default inhibit-message nil)
      (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibited-vars-h))

    (add-hook 'post-command-hook
              #'minimal-emacs--reset-inhibited-vars-h -100)

    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format nil)))

    (put 'mode-line-format 'initial-value
         (default-toplevel-value 'mode-line-format))
    (setq-default mode-line-format nil)

    (defun minimal-emacs--startup-load-user-init-file (fn &rest args)
      "Advice for startup--load-user-init-file to reset mode-line-format."
      (unwind-protect
          (progn
            ;; Start up as normal
            (apply fn args))
        ;; If we don't undo inhibit-{message, redisplay} and there's an
        ;; error, we'll see nothing but a blank Emacs frame.
        (setq-default inhibit-message nil)
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format
                        (get 'mode-line-format 'initial-value)))))

    (advice-add 'startup--load-user-init-file :around
                #'minimal-emacs--startup-load-user-init-file)

    ;; Without this, Emacs will try to resize itself to a specific column size
    (setq frame-inhibit-implied-resize t)

    ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
    ;; No second pass of case-insensitive search over auto-mode-alist.
    (setq auto-mode-case-fold nil)

    ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
    ;; dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for a modest performance boost.
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)

    ;; Give up some bidirectional functionality for slightly faster re-display.
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless nil ; minimal-emacs-debug
      ;; Unset command line options irrelevant to the current OS. These options
      ;; are still processed by `command-line-1` but have no effect.
      (unless (eq system-type 'darwin)
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil)))))

;;; Supress warnings
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;;; UI
(setq tool-bar-mode nil)
(setq-default default-frame-alist '(;(font . "Iosevka-12")
                                    (height . 30)
                                    (tool-bar-lines . 0)
                                    (alpha-background . 90)))
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(set-face-attribute 'default nil
                    :family "Iosevka" :height 140 :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka" :height 1.0 :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :family "Sans Serif" :height 1.0 :weight 'regular)

;;; package.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("stable" . 70)
                                                      ("melpa"  . 0)))
