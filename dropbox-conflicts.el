;;; dropbox-conflicts.el --- Detect dropbox conflicting copies when opening a file -*- lexical-binding: t -*-

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

;;; Code:
(eval-when-compile (require 'cl-lib))

(defgroup dropbox-conflicts nil
  "Detect dropbox conflicting copies when opening a file"
  :prefix "dropbox-conflicts-"
  :group 'applications)

(defcustom dropbox-conflicts-foo ""
  "Description of variable."
  :type 'string
  :group 'dropbox-conflicts)

(provide 'dropbox-conflicts)

;;; dropbox-conflicts.el ends here
