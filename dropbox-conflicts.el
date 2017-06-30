;;; dropbox-conflicts.el --- Detect dropbox conflicting copies when opening a file  -*- lexical-binding: t; -*-

;; Copyright (c) 2016 Jez Cope <j.cope@erambler.co.uk>

;; Author: Jez Cope <j.cope@erambler.co.uk>
;; URL: https://github.com/jezcope/dropbox-conflicts
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (f "0.19.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'cl))

(require 'f)
(require 'subr-x)

(defgroup dropbox-conflicts nil
  "Detect dropbox conflicting copies when opening a file"
  :prefix "dropbox-conflicts-"
  :group 'applications)

(defun dropbox-conflicts-find-copies (path)
  "Find potential conflicted copies of the file given by PATH."
  (let ((folder (f-parent path))
        (base (f-base path))
        (ext (f-ext path)))
    (f-glob (concat base " (*'s conflicted copy *)." ext) folder)))

(defun dropbox-conflicts-extract-conflict-info (path)
  "Extract hostname and date from PATH."
  (assert (string-match "(\\([[:alnum:].-]+\\)'s conflicted copy \\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\))" path))
  (list (match-string 1 path)
        (match-string 2 path)))

(defun dropbox-conflicts-warn-if-conflicted-copies (path)
  "Warn if there are conflicted copies of the file given by PATH."
  (let ((copies (dropbox-conflicts-find-copies path)))
    (when copies
      (message "Conflicting copies of this file exist in dropbox:\n  %s"
               (let ((details (mapcar 'dropbox-conflicts-extract-conflict-info copies)))
                 (mapconcat (lambda (x) (concat (second x) " " (first x)))
                            details "\n  "))))))

(defun dropbox-conflicts-check-for-conflicts ()
  "Check if there are conflicted copies of the current file."
  (interactive)
  (dropbox-conflicts-warn-if-conflicted-copies (buffer-file-name)))

;;;###autoload
(define-minor-mode dropbox-conflicts-mode
  :init-value nil
  :lighter nil
  :keymap nil
  :global t
  (if dropbox-conflicts-mode
      (add-hook 'find-file-hook 'dropbox-conflicts-check-for-conflicts)
    (remove-hook 'find-file-hook 'dropbox-conflicts-check-for-conflicts)))

(provide 'dropbox-conflicts)

;; Local Variables:
;; nameless-current-name: "dropbox-conflicts"
;; End:

;;; dropbox-conflicts.el ends here
