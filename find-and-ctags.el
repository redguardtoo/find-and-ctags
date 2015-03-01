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

;;;###autoload
(defun fac-create-tags-if-needed (SRC-DIR FIND-OPTS CTAGS-OPTS &optional FORCE)
  "Create TAGS if need and return the full path it"
  ;; TODO save the CTAGS-OPTS into hash
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
        (gnufind (if *win32* "c:\\\\cygwin64\\\\bin\\\\find" "find"))
        old-dir
        file
        cmd)
    (setq file (concat dir "TAGS"))
    (if (string= "" FIND-OPTS) (setq FIND-OPTS "-not -size +24k"))
    (when (or FORCE (not (file-exists-p file)))
      (setq old-dir default-directory)
      ;; "cd dir && find . -name blah | ctags" will NOT work on windows
      (setq default-directory dir)
      ;; use relative directory because TAGS is shared between cygwin and win32 Emacs
      (setq cmd (format "%s . -type f -not -name 'TAGS' %s | ctags -e %s -L -" gnufind FIND-OPTS CTAGS-OPTS))
      (shell-command cmd)
      ;; restore default directory
      (setq default-directory old-dir))
    file))

(provide 'find-and-ctags)

;;; find-and-ctags.el ends here
