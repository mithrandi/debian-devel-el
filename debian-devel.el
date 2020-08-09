;;; debian-devel.el --- Debian development tools     -*- lexical-binding: t; -*-

;; © 2020  Tristan Seligmann

;; Author: Tristan Seligmann <mithrandi@mithrandi.net>
;; Keywords: Debian

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'comint)
(require 'projectile)
(require 'bui)
(require 'dash)
(require 'f)
(require 's)
(require 'parsec)
(eval-when-compile
  (require 'cl-lib))

(defun dd--dpmt-root ()
  (f-full "~/debian/packages/python-modules"))

(defun dd--build-area ()
  (f-full "~/debian/build-area"))

(cl-defsubst dd--ensure-package (dir)
  "Ensure that DIR is non-nil."
  (if dir
      dir
    (error "No package found")))

(cl-defun dd-find-package (&optional (dir default-directory))
  (f--traverse-upwards (f-dir? (f-expand "debian" it)) dir))

(defmacro dd-with-package-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory (dd--ensure-package (dd-find-package ,dir))))
     ,@body))

(defun dd-clone-package-dpmt (package-name)
  "Clone a package maintained by DPMT.

PACKAGE-NAME: the package to clone."
  (interactive "s")
  (projectile-with-default-dir (dd--dpmt-root)
    (process-file (f-expand "checkout" (dd--dpmt-root)) nil nil nil package-name)
    (dired (f-expand package-name (dd--dpmt-root)))))

(defun dd-lintian-brush ()
  (interactive)
  (dd-with-package-dir nil
    (let* ((prev-rev (magit-rev-parse "HEAD"))
           (name (concat
                  "dd-lintian-brush: "
                  (f-abbrev default-directory)))
           (proc (start-file-process
                  name nil
                  "lintian-brush" "--modern" "--uncertain")))
      (set-process-sentinel
       proc
       (lambda (p _e)
         (when (= 0 (process-exit-status p))
           (magit-log-other (list (concat prev-rev "..HEAD")))))))))

(defun dd-build-binary ()
  "Run a binary build of the package."
  (interactive)
  (dd-with-package-dir nil
    (-when-let* ((name (concat
                        "dd-sbuild: "
                        (f-abbrev default-directory)))
                 (buf (make-comint
                       name
                       "gbp" nil "buildpackage" "--git-builder=sbuild" "-d" "unstable"
                       "--no-clean-source")))
      (display-buffer buf)
      (with-current-buffer buf
        (comint-clear-buffer)
        (goto-char (point-max))))))

(defun dd--changes->entry (changes-file)
  (let ((fields (dd-parse-changes changes-file)))
    `((id . ,changes-file)
      (name . ,(f-filename changes-file))
      (file-name . ,changes-file)
      (date . ,(cdr (assoc-string "Date" fields t)))
      (dist . ,(cdr (assoc-string "Distribution" fields t)))
      (arch . ,(cdr (assoc-string "Architecture" fields t)))
      (signed . ,(cdr (assoc-string "*signed*" fields t))))))

(defun dd--changes-get-entries ()
  (-map #'dd--changes->entry
        (f--files (dd--build-area) (s-ends-with? ".changes" it))))

(bui-define-interface
 dd-changes list
 :buffer-name "*Changes*"
 :get-entries-function #'dd--changes-get-entries
 :format '((name nil 40 t)
           (file-name bui-list-get-file-name 20 t)
           (date bui-list-get-time 20 t)
           (dist nil 10 t)
           (arch nil 25 t)
           (signed nil 5 t))
 :sort-key '(date . t))

(defun dd-list-changes ()
  (interactive)
  (bui-get-display-entries 'dd-changes 'list))

(defun dd-parse-changes (changes-file)
  (parsec-with-input (f-read-text changes-file)
    (parsec-or
     (dd-deb822-file1-signed)
     (dd-deb822-file1))))

(cl-defsubst dd-deb822-file ()
  (parsec-return (parsec-many (dd-deb822-para))
    (parsec-eof)))

(cl-defsubst dd-deb822-file1 ()
  (parsec-return (dd-deb822-para)
                 (parsec-eof)))

(cl-defsubst dd-deb822-file1-signed ()
  (cons
   '("*signed*" . t)
   (parsec-between
    (dd-deb822-gpg-header)
    (dd-deb822-gpg-trailer)
    (dd-deb822-para))))

(cl-defsubst dd-deb822-para ()
  (parsec-return (parsec-many (dd-deb822-field))
    (parsec-many (parsec-eol))))

(cl-defsubst dd-deb822-field ()
  (cons
   (parsec-return
       (parsec-many1-s (parsec-re "[!-9;-~]"))
     (parsec-str ":")
     (parsec-many (parsec-one-of ?\s ?\t)))
   (parsec-until-s
    (parsec-or
     (parsec-eof)
     (parsec-try
      (parsec-and
       (parsec-eol)
       (parsec-not-followed-by (parsec-one-of ?\s ?\t))))))))

(cl-defsubst dd-deb822-gpg-header ()
  (parsec-and
   (parsec-str "-----BEGIN PGP SIGNED MESSAGE-----")
   (parsec-eol)
   (dd-deb822-gpg-fields)
   (parsec-eol)))

(cl-defsubst dd-deb822-gpg-fields ()
  (parsec-many
    (parsec-and
     (parsec-many1 (parsec-none-of ?- ?\n ?\r))
     (parsec-eol))))

(cl-defsubst dd-deb822-gpg-trailer ()
  (parsec-and
   (parsec-str "-----BEGIN PGP SIGNATURE-----")
   (parsec-eol)
   (dd-deb822-gpg-fields)
   (parsec-eol)
   (parsec-many
    (parsec-and
     (parsec-many (parsec-none-of ?- ?\n ?\r))
     (parsec-eol)))
   (parsec-str "-----END PGP SIGNATURE-----")
   (parsec-eol)
   (parsec-eof)))

(provide 'debian-devel)
;;; debian-devel.el ends here
