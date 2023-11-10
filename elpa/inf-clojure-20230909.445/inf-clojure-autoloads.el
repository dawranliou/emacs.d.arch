;;; inf-clojure-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from inf-clojure.el

(defvar inf-clojure-mode-line '(:eval (format " inf-clojure[%s]" (inf-clojure--modeline-info))) "\
Mode line lighter for cider mode.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for details
about mode line templates.

Customize this variable to change how inf-clojure-minor-mode
displays its status in the mode line.  The default value displays
the current REPL.  Set this variable to nil to disable the
mode line entirely.")
(custom-autoload 'inf-clojure-mode-line "inf-clojure" t)
(autoload 'inf-clojure-minor-mode "inf-clojure" "\
Minor mode for interacting with the inferior Clojure process buffer.

The following commands are available:

\\{inf-clojure-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Inf-Clojure minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `inf-clojure-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(autoload 'inf-clojure "inf-clojure" "\
Run an inferior Clojure process, input and output via buffer `*inf-clojure*'.
If there is a process already running in `*inf-clojure*', just
switch to that buffer.

CMD is a string which serves as the startup command or a cons of
host and port.

 Prompts user for repl startup command and repl type if not
inferrable from startup command.  Uses `inf-clojure-custom-repl-type'
and `inf-clojure-custom-startup' if those are set.
Use a prefix to prevent using these when they
are set.

Prints a message that it has connected to the host and port
unless SUPPRESS-MESSAGE is truthy.

 Runs the hooks from `inf-clojure-mode-hook' (after the
`comint-mode-hook' is run).  (Type \\[describe-mode] in the
process buffer for a list of commands.)

(fn CMD &optional SUPPRESS-MESSAGE)" t)
(autoload 'inf-clojure-socket-repl "inf-clojure" "\
Start a socket REPL server and connects to it via `inf-clojure-connect'.
CMD is the command line instruction used to start the socket
REPL.  It should be a string with \"%d\" in it to take a random
port.  Set `inf-clojure-custom-startup' or choose from the
defaults provided in `inf-clojure-socket-repl-startup-forms'.

(fn CMD)" t)
(register-definition-prefixes "inf-clojure" '("inf-clojure-"))

;;; End of scraped data

(provide 'inf-clojure-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; inf-clojure-autoloads.el ends here
