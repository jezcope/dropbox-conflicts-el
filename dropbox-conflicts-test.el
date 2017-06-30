;;; dropbox-conflicts-test.el --- dropbox-conflicts: Unit test suite -*- lexical-binding: t -*-

;; Copyright (c) 2016 Jez Cope <j.cope@erambler.co.uk>

;; Author: Jez Cope <j.cope@erambler.co.uk>
;; URL: https://github.com/jezcope/dropbox-conflicts
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

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

;; The unit test suite of dropbox-conflicts

;; Contents of ./fixtures/ is as follows:

;; no-conflicts.txt
;; single-conflict (fred's conflicted copy 2016-11-08).txt
;; single-conflict.txt
;; three-conflicts (erwin's conflicted copy 2016-11-04).txt
;; three-conflicts (fred's conflicted copy 2016-10-23).txt
;; three-conflicts (rebecca's conflicted copy 2016-09-03).txt
;; three-conflicts.txt

;;; Code:

(require 'dropbox-conflicts)
(require 'undercover nil t)

(when (fboundp 'undercover)
  (undercover "dropbox-conflicts.el"))

(ert-deftest dropbox-conflicts-test-find-conflicts ()
  "Tests that conflicted copies are correctly found"
  (should (eq nil (dropbox-conflicts-find-copies "./fixtures/no-conflicts.txt")))
  (should (eq 1 (length (dropbox-conflicts-find-copies "./fixtures/single-conflict.txt"))))
  (should (eq 3 (length (dropbox-conflicts-find-copies "./fixtures/three-conflicts.txt")))))

(ert-deftest dropbox-conflicts-test-extract-conflict-info ()
  "Tests that information is parsed from conflict filenames correctly"
  (should (equal '("fred-home.example.com" "2016-11-08")
                 (dropbox-conflicts-extract-conflict-info "/tmp/single-conflict (fred-home.example.com's conflicted copy 2016-11-08).txt"))))

(ert-deftest dropbox-conflicts-test-warn ()
  "Tests that warnings are correctly given"
  (save-excursion
    (dropbox-conflicts-warn-if-conflicted-copies "./fixtures/no-conflicts.txt")
    (switch-to-buffer "*Messages*")
    (goto-char (point-min))
    (should (not (search-forward "Conflicting copies" nil t)))

    (dropbox-conflicts-warn-if-conflicted-copies "./fixtures/single-conflict.txt")
    (switch-to-buffer "*Messages*")
    (goto-char (point-min))
    (should (search-forward "Conflicting copies" nil t))
    (goto-char (point-min))
    (should (search-forward "single-conflict.txt" nil t))
    (goto-char (point-min))
    (should (search-forward "fred"))
    (goto-char (point-min))
    (should (search-forward "2016-11-08"))))

(provide 'dropbox-conflicts-test)

;;; dropbox-conflicts-test.el ends here
