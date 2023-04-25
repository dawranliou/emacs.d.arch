(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.7
      frame-inhibit-implied-resize nil
      default-frame-alist '((font . "Iosevka-12")
                            (vertical-scroll-bars . nil)
                            (width . 80)
                            (height . 35)))

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
