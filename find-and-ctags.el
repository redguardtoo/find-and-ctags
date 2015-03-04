;;; find-and-ctags.el --- use `find' and `ctags' for code navigation

;; Copyright (C) 2014 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/find-and-ctags
;; Keywords: find ctags
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This file is free software (GPLv3 License)

;; Set up:
;;
;; https://github.com/redguardtoo/find-and-ctags/blob/master/README.org for use cases

;;; Code:

(defvar fac-gnu-find-executable nil
  "The path of GNU Find. If it's nil, it will be automatically detected.")

(defvar fac-ctags-executable nil
  "The path of Ctags. If it's nil, it will be automatically detected.")

(defun fac--guess-gnu-find ()
  (let (rlt)
    (if (eq system-type 'windows-nt)
        (cond
         ((executable-find "c:\\\\cygwin64\\\\bin\\\\find")
          (setq rlt "c:\\\\cygwin64\\\\bin\\\\find"))
         ((executable-find "d:\\\\cygwin64\\\\bin\\\\find")
          (setq rlt "d:\\\\cygwin64\\\\bin\\\\find"))
         ((executable-find "e:\\\\cygwin64\\\\bin\\\\find")
          (setq rlt "e:\\\\cygwin64\\\\bin\\\\find"))
         ((executable-find "c:\\\\cygwin\\\\bin\\\\find")
          (setq rlt "c:\\\\cygwin\\\\bin\\\\find"))
         ((executable-find "d:\\\\cygwin\\\\bin\\\\find")
          (setq rlt "d:\\\\cygwin\\\\bin\\\\find"))
         ((executable-find "e:\\\\cygwin\\\\bin\\\\find")
          (setq rlt "e:\\\\cygwin\\\\bin\\\\find"))
         (t (setq rlt "find"))))
    rlt))

(defun fac--guess-ctags ()
  (let (rlt "ctags")
    (if (eq system-type 'windows-nt)
        (cond
         ((executable-find "c:\\\\cygwin64\\\\bin\\\\ctags")
          (setq rlt "c:\\\\cygwin64\\\\bin\\\\ctags"))
         ((executable-find "d:\\\\cygwin64\\\\bin\\\\ctags")
          (setq rlt "d:\\\\cygwin64\\\\bin\\\\ctags"))
         ((executable-find "e:\\\\cygwin64\\\\bin\\\\ctags")
          (setq rlt "e:\\\\cygwin64\\\\bin\\\\ctags"))
         ((executable-find "c:\\\\cygwin\\\\bin\\\\ctags")
          (setq rlt "c:\\\\cygwin\\\\bin\\\\ctags"))
         ((executable-find "d:\\\\cygwin\\\\bin\\\\ctags")
          (setq rlt "d:\\\\cygwin\\\\bin\\\\ctags"))
         ((executable-find "e:\\\\cygwin\\\\bin\\\\ctags")
          (setq rlt "e:\\\\cygwin\\\\bin\\\\ctags"))
         (t (setq rlt "ctags"))))
    rlt))

;;;###autoload
(defun fac-run-ctags-if-needed (SRC-DIR FIND-OPTS CTAGS-OPTS &optional FORCE)
  "Ask ctags to create TAGS and return the full path of TAGS"
  ;; TODO save the CTAGS-OPTS into hash
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
        (find-exe (if fac-gnu-find-exe fac-gnu-find-exe (fac--guess-gnu-find)))
        (ctags-exe (if fac-ctags-exe fac-find-exe (fac--guess-ctags)))
        old-dir
        file
        cmd)
    (setq file (concat dir "TAGS"))
    (when (or FORCE (not (file-exists-p file)))
      (setq old-dir default-directory)
      ;; "cd dir && find . -name blah | ctags" will NOT work on windows cmd window
      (setq default-directory dir)
      ;; use relative directory because TAGS is shared between Cygwin and Window
      (setq cmd (format "%s . -type f -not -name 'TAGS' %s | %s -e %s -L - &"
                        find-exe
                        FIND-OPTS
                        ctags-exe
                        CTAGS-OPTS))
      (shell-command cmd)
      ;; restore default directory
      (setq default-directory old-dir))
    file))

;;;###autoload
(defun fac-current-path-match-pattern-p (REGEX)
  "Is current directory match the REGEX?"
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))

(provide 'find-and-ctags)

;;; find-and-ctags.el ends here
