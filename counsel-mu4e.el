;;; counsel-mu4e.el --- Search emails in Mu4e asynchronously with Ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sean Farley

;; Author: Sean Farley <sean@farley.io>
;; URL: https://bitbucket.org/seanfarley/counsel-mu4e
;; Keywords: mail
;; Version: 0.1
;; Package-Requires: ((emacs "24") (ivy "0.10.0") (mu4e "0.21"))

;; This program is free software; you can redistribute it and/or modify
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

;; This package provides `counsel-mu4e' function which is inspired by
;; `counsel-notmuch'.
;; Simply call `counsel-mu4e' function and input your mu4e query.

;;; Code:
;;; License: GPLv3
;;
;;
;;; Code:
;;

(require 'counsel)
(require 'mu4e)
(require 'subr-x)

(defgroup counsel-mu4e nil
  "Options for counsel-mu4e."
  :group 'mu4e)

(defcustom counsel-mu4e-path "mu"
  "Path to mu4e executable."
  :type 'string
  :group 'counsel-mu4e)

(defcustom counsel-mu4e-flags "-n 5 --skip-dups --sortfield=date"
  "Path to mu4e executable."
  :type 'string
  :group 'counsel-mu4e)

(defface counsel-mu4e-date-face
  '((t :inherit mu4e-header-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defface counsel-mu4e-count-face
  '((t :inherit mu4e-header-highlight-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defface counsel-mu4e-people-face
  '((t :inherit mu4e-contact-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defface counsel-mu4e-subject-face
  '((t :inherit mu4e-header-face :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'counsel-mu4e)

(defvar counsel-mu4e-history nil
  "History for `counsel-mu4e'.")

;; the delimiter used in the --fields string only looks like two spaces; it
;; is actually U+205F (MEDIUM MATHEMATICAL SPACE) followed by U+2006
;; (SIX-PER-EM SPACE) in the hope that no one has those two characters in a
;; row in their subject
(defvar counsel-mu4e-delimiter "  "
  "Delimiter for fields in mu output.")

(defun counsel-mu4e-cmd (input)
  "Form mu4e query command using INPUT."
  (counsel-require-program counsel-mu4e-path)
  (format "%s find %s --fields 'i%ss' --nocolor %s"
          counsel-mu4e-path
          counsel-mu4e-flags
          counsel-mu4e-delimiter
          (shell-quote-argument input)))

(defun counsel-mu4e--parse (str pos)
  "Parse a single sexp from STR starting as POS."
  (when (or (string-prefix-p "(\n" str)
            (string-prefix-p "(:docid" str))
    (condition-case nil
        (read-from-string str pos)
      ((debug error) nil))))

(defun counsel-mu4e--parse-single (str)
  "Convience method to only parse a known single item from STR."
  (car (counsel-mu4e--parse str 0)))

(defun counsel-mu4e--parse-sexp (str)
  "Return a list of sexp's from mu output STR."
  (let* ((pos 0)
         (end (1- (length str)))
         (item nil)
         (items nil))
    ;; loop over parsing the string
    (while (< pos end)
      (setq item (counsel-mu4e--parse str pos))
      (if (plist-get (car item) :docid)
          (progn
            (setq pos (cdr item))
            (push (format "%S" (car item)) items))
        (setq pos (length str))))
    ;; return list of items
    items))

(defun counsel--mu4e-async-sentinel (process event)
  "Sentinel function for an asynchronous counsel PROCESS.
EVENT is a string describing the change."
  (let ((cands
         (cond ((string= event "finished\n")
                (with-current-buffer (process-buffer process)
                  (counsel-mu4e--parse-sexp (buffer-string))))
               ((string-match "exited abnormally with code \\([0-9]+\\)\n" event)
                (let* ((exit-code-plist (plist-get counsel--async-exit-code-plist
                                                   (ivy-state-caller ivy-last)))
                       (exit-num (read (match-string 1 event)))
                       (exit-code (plist-get exit-code-plist exit-num)))
                  (list
                   (or exit-code
                       (format "error code %d" exit-num))))))))
    (cond ((string= event "finished\n")
           (ivy--set-candidates
            (ivy--sort-maybe
             ;; (mapcar (lambda (i) (format "thread: %s" (plist-get i :subject))) cands)
             cands
             ))
           (setq counsel-grep-last-line nil)
           (when counsel--async-start
             (setq counsel--async-duration
                   (time-to-seconds (time-since counsel--async-start))))
           (let ((re (funcall ivy--regex-function ivy-text)))
             (unless (stringp re)
               (setq re (caar re)))
             (if (null ivy--old-cands)
                 (unless (ivy-set-index
                          (ivy--preselect-index
                           (ivy-state-preselect ivy-last)
                           ivy--all-candidates))
                   (ivy--recompute-index
                    ivy-text re ivy--all-candidates))
               (ivy--recompute-index
                ivy-text re ivy--all-candidates)))
           (setq ivy--old-cands ivy--all-candidates)
           (if (null ivy--all-candidates)
               (ivy--insert-minibuffer "")
             (ivy--exhibit)))
          ((string-match "exited abnormally with code \\([0-9]+\\)\n" event)
           (setq ivy--all-candidates cands)
           (setq ivy--old-cands ivy--all-candidates)
           (ivy--exhibit)))))

(defun counsel--mu4e-process-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
`counsel-async-filter-update-time' microseconds since the last update."
  (with-current-buffer (process-buffer process)
    (insert str))
  ;; boiler code for updating the time
  (when (time-less-p
         `(0 0 ,counsel-async-filter-update-time 0)
         (time-since counsel--async-time))
    ;; switch to the output of the process
    (with-current-buffer (process-buffer process)
      ;; put the entire buffer into a variable (should be fine without calling
      ;; `buffer-substring-no-properties' since this this the direct output of a
      ;; process)
      (let ((items (counsel-mu4e--parse-sexp (buffer-string))))
        ;; add the items to the candidates
        (ivy--set-candidates items)

        ;; update the ivy prompt with the size
        (let ((ivy--prompt (format
                            (concat "%d++ " (ivy-state-prompt ivy-last))
                            (length items))))
          (ivy--insert-minibuffer
           (ivy--format ivy--all-candidates)))))
    (setq counsel--async-time (current-time))))

(defun counsel-mu4e-function (input)
  "Get mail from mu4e using INPUT."
  (if (< (length input) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (counsel-mu4e-cmd input))
    '("" "working...")))

(defun counsel-mu4e-action-show (sexp)
  "Open resulting SEXP in ‘mu4e-show’ view."
  (mu4e-view-message-with-message-id
   (car (split-string sexp counsel-mu4e-delimiter))))

(defun counsel-mu4e--get-match-face (needle haystack)
  "Return the nth match face if NEEDLE is in HAYSTACK.
Otherwise return default face."
  (let ((tail (member needle haystack)))
    (if (not tail)
        'counsel-mu4e-subject-face
      (nth (1+ (mod (- (length haystack) (length tail))
                    (1- (length ivy-minibuffer-faces))))
           ivy-minibuffer-faces))))

(defun counsel-mu4e-transformer (str)
  "Transform STR to mu4e display style."
  (let ((fields (split-string str counsel-mu4e-delimiter)))
    (when (> (length fields) 1)
      (let* ((keys (mapcar (lambda (i) (downcase i)) (split-string ivy-text)))
             (subject (string-join (mapcar (lambda (i)
                                             (propertize i 'face
                                                         (counsel-mu4e--get-match-face
                                                          (downcase i) keys)))
                                           (split-string (nth 1 fields)))
                                   " ")))
        (format "%s" subject)))))

;;;###autoload
(defun counsel-mu4e (&optional initial-input)
  "Search for your email in mu4e with INITIAL-INPUT."
  (interactive)
  (ivy-read "mu4e search: " 'counsel-mu4e-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-mu4e-history
            :action #'counsel-mu4e-action-show
            :unwind #'counsel-delete-process
            :caller #'counsel-mu4e))

(ivy-set-display-transformer 'counsel-mu4e 'counsel-mu4e-transformer)

(provide 'counsel-mu4e)

;;; counsel-mu4e.el ends here
