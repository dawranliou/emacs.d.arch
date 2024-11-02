;; Quick startup
(when (file-exists-p (locate-user-emacs-file "package-quickstart.el"))
  (setf package-enable-at-startup nil)
  (defvar package-quickstart)
  (setf package-quickstart t))

;;; GC
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (setq gc-cons-threshold (* 16 1024 1024))))


;;; Supress warnings
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;;; UI
(setq inhibit-splash-screen t)

(setq-default default-frame-alist '((font . "Iosevka-12")
                                    (height . 30)))
(setq frame-inhibit-implied-resize t)
;; (set-face-attribute 'default nil
;;                     :family "Iosevka" :height 120 :weight 'regular)
;; (set-face-attribute 'fixed-pitch nil
;;                     :family "Iosevka" :height 120 :weight 'regular)
;; (set-face-attribute 'variable-pitch nil
;;                     :family "Iosevka Etoile" :height 120 :weight 'regular)

