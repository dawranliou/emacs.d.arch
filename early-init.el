(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.7
      frame-inhibit-implied-resize nil
      default-frame-alist '((font . "Iosevka-14")
                            ;; (width . 80)
                            (height . 30)
                            ;; (menu-bar-lines . 1)
                            )
      )

;; (set-face-attribute 'default nil
;;                     :family "Iosevka" :height 140 :weight 'regular)
;; (set-face-attribute 'fixed-pitch nil
;;                     :family "Iosevka" :height 140 :weight 'regular)
;; (set-face-attribute 'variable-pitch nil
;;                     :family "Iosevka Etoile" :height 140 :weight 'regular)

(when (file-exists-p (locate-user-emacs-file "package-quickstart.el"))
  (setf package-enable-at-startup nil)
  (defvar package-quickstart)
  (setf package-quickstart t))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (setq gc-cons-threshold (* 256 1024 1024)
                  gc-cons-percentage 0.3)))
