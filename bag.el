;;; bag.el --- Help maintain URL bookmarks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 https://dwim.nl

;; Author: Vrind <vrind@dwim.nl>
;; Created: July 10, 2021
;; Version: 0.0.3
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, hypermedia
;; URL: https://github.com/vrind-nl/bag.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  This Emacs package helps to maintain URL bookmarks by finding a title and tags, given a URL,
;;  and writing a org-mode link with tags to a designated file.
;;
;;  A bag is a collection of links
;;  A bag entry is an org section title with optional tags. The title is either an
;;  org link, or a URL.
;;
;;  Tip: bind to "C-c l" key with (define-key mode-specific-map "l" 'bag-lap)

;;; Code:


;;; Customization

(defgroup bag nil
  "Tools to maintain a library (bag) of URL links"
  :prefix "bag-"
  :group 'comm)


(defcustom bag-tags '("gnu"
                      ("editors" ("emacs" "vi"))
                      ("dev" (nil "lisp" "python")))
  "A list of tags or tags and keywords that indicate that tag.
By default the tag itself is also a keyword, unless the first keyword is nil."
  :group 'bag
  :type 'list)


;;; Get data for URL

(defun bag-get-element (element)
  "Return text between first <ELEMENT> and </ELEMENT> in current HTML buffer"
  (goto-char (point-min))
  (let ((start (re-search-forward (format "<%s[^>]*>" element) nil t))
        (end (re-search-forward (format "</%s>" element) nil t)))
    ;; return text in element if start and end are defined
    (and start end (buffer-substring-no-properties start
                                                   (- end (length element) 3)))))


(defun bag-get-title ()
  "Derive a title from the current HTML buffer, or nil if no title was found"
  ;; get from title or first h1 element
  (or (bag-get-element "title") (bag-get-element "h1")))


(defun bag-check-keyword (keyword)
  "Return position of first keyword in current HTML buffer"
  (goto-char (point-min))
  (re-search-forward (format "\s%s\s" keyword) nil t)) ;; separate words only


(defun bag-check-keywords (keywords)
  "Return non-nil if at least one of KEYWORDS exists in current HTML buffer "
  (let ((found  nil))
    (catch 'here
      (dolist (keyword keywords)
        (when (setq found (bag-check-keyword keyword))
          (throw 'here found))))
    found))


(defun bag-check-tag (tag)
  "Return TAG name if TAG keywords occur in the current HTML buffer"
  ;; if tag is string, use it as keyword
  (if (stringp tag) (if (bag-check-keywords (list tag)) tag nil)
    ;; else split tag and keywords
    (let ((keywords (nth 1 tag)))
      (setq tag (car tag))
      ;; if first keyword is non-nil, add tag as keyword, else remove nil
      (if (car keywords) (push tag keywords) (pop keywords))
      ;; return tag if keywords occur, nil otherwise
      (if (bag-check-keywords keywords) tag nil))))


(defun bag-get-tags ()
  "Return list of tags in the current HTML buffer, or nil when no tags match"
  (let ((tags (remove nil (mapcar 'bag-check-tag bag-tags))))
    (if (not (null tags))
        (format ":%s:" (mapconcat 'identity tags ":")))))


(defun bag-get (url)
  "Take a URL and return a bag entry."
  (with-current-buffer (url-retrieve-synchronously url)
    (let* ((title (bag-get-title))
           (result (if title (format "[[%s][%s]]" url title) url))
           (tags (bag-get-tags)))
      (if tags (setq result (format "%s %s" result tags)))
      result)))


;; Handle URLs in buffer

(defun bag-find-url ()
  "Find the next URL in the current buffer. Returns the end position and the URL, or nil if no URL was found."
  (let ((end (re-search-forward "http[^[:blank:][:cntrl:]]*" (point-max) t)))
    (if end
        (list end (string-trim (match-string 0)))
      (progn
        (message "Could not find URL")
        nil))))


(defun bag-find-and-replace ()
  "Find the next URL in the current buffer and replace it with a full link. Returns nil if no URL was found."
  (let ((result (bag-find-url)))
    (if result
        (let* ((end (car result))
               (url (nth 1 result))
               (beg (- end (length url)))
               (link (bag-get url)))
          (kill-region beg end)
          (insert link)
          end))))


;; Interactive functions

;;;###autoload
(defun bag-lap ()
  "Replace link-at-point"
  (interactive)
  (save-excursion
    (let ((beg (re-search-backward "[:blank:]" nil t)))
      (if beg
          (progn
            (forward-char)
            (bag-find-and-replace))
        (message "Could not find beginning of URL")))))


;;;###autoload
(defun bag-replace-all ()
  "Replace all URLS in current buffer with org-mode link"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (bag-find-and-replace) ())))

(provide 'bag)
;;; bag.el ends here
