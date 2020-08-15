;; init-const.el --- Define constants.	-*- lexical-binding: t -*-
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")


;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen."
  (and sys/mac-cocoa-p
       emacs/>=26p
       (boundp ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

;; set coding system
(defun semgilo/set-coding-system (coding)
  "Set coding-system."
  (prefer-coding-system coding)
  (setq locale-coding-system coding)
  (set-language-environment coding)
  (set-default-coding-systems coding)
  (set-buffer-file-coding-system coding)
  (set-clipboard-coding-system coding)
  (set-file-name-coding-system coding)
  (set-keyboard-coding-system coding)
  (set-terminal-coding-system coding)
  (set-selection-coding-system coding)
  (modify-coding-system-alist 'process "*" coding)
  )

(defun show-in-explorer (path)
  "Show path in explorer (window platform)"
  (call-process-shell-command (format "explorer.exe %s" path)))

(defun show-current-buffer-in-explorer ()
  "Show current buff in explorer"
  (progn
    (setq path (file-name-directory (buffer-file-name)))
    (message path)
    (show-in-explorer path)
    ))

(provide 'init-define)
