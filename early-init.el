(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.7
      frame-inhibit-implied-resize nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (setq gc-cons-threshold (* 256 1024 1024)
                  gc-cons-percentage 0.3)))
