;;; dd-deb822.el --- deb822 control file parsing     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tristan Seligmann

;; Author: Tristan Seligmann <mithrandi@lorien.mithrandi.net>
;; Keywords: data

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

(eval-when-compile
  (require 'cl-lib))
(require 'parsec)

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
   '(*signed* . t)
   (parsec-between
    (dd-deb822-gpg-header)
    (dd-deb822-gpg-trailer)
    (dd-deb822-para))))

(cl-defsubst dd-deb822-para ()
  (parsec-return (parsec-many (dd-deb822-field))
    (parsec-many (parsec-eol))))

(cl-defsubst dd-deb822-field ()
  (parsec-or
   (parsec-between
    (parsec-one-of ?\s ?\t)
    (parsec-eol-or-eof)
    (parsec-many-s (parsec-none-of ?\n ?\r)))
   (cons
    (intern
     (s-downcase
      (parsec-return (parsec-many1-s (parsec-re "[!-9;-~]"))
        (parsec-str ":")
        (parsec-many (parsec-one-of ?\s ?\t)))))
    (parsec-return (parsec-many-s (parsec-none-of ?\n ?\r))
      (parsec-eol-or-eof)))))

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

(provide 'dd-deb822)
;;; dd-deb822.el ends here
