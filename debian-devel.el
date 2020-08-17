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
(require 'bui)
(require 'dash)
(require 'f)
(require 's)
(require 'parsec)
(eval-when-compile
  (require 'cl-lib))
(require 'aio)
(require 'request)
(require 'url)
(require 'magit)
(require 'transient)

(defun dd--packages-root ()
  (f-full "~/debian/packages"))

(defun dd--build-area ()
  (f-full "~/debian/build-area"))

(defmacro dd-with-default-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

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

(cl-defun dd-get-package-name (&optional (dir default-directory))
  (with-temp-buffer
    (insert-file-contents-literally
     (f-expand "debian/control" (dd--ensure-package (dd-find-package dir))))
    (s-trim
     (buffer-substring-no-properties
      (re-search-forward "^Source:")
      (line-end-position)))))

(defun dd-find-named-package (package-name)
  (interactive "sPackage: ")
  (-first-item
   (f--directories
    (dd--packages-root)
    (and (s-equals? (f-filename it) package-name)
         (f-dir? (f-expand "debian" it))))))

(aio-defun dd-gbp-clone (package-name)
  (interactive "sPackage: ")
  (let ((dir (f-expand package-name (dd--packages-root))))
    (aio-await
     (dd-aio-run-in-comint
      (s-concat "dd-clone: " package-name)
      "gbp" "clone " (s-concat "vcsgit:" package-name) dir))
    dir))

(aio-defun dd-clone-package (package-name)
  "Clone a package, or switch to the existing clone.

PACKAGE-NAME: the package to clone."
  (interactive "sPackage: ")
  (dired
   (or (dd-find-named-package package-name)
       (aio-await (dd-debcheckout package-name)))))

(defun dd-lintian-brush ()
  (interactive)
  (dd-with-package-dir nil
    (let* ((prev-rev (magit-rev-parse "HEAD"))
           (name (concat "dd-lintian-brush: " (f-abbrev default-directory)))
           (proc (start-file-process
                  name nil
                  "lintian-brush" "--modern" "--uncertain")))
      (set-process-sentinel
       proc
       (lambda (p _e)
         (when (= 0 (process-exit-status p))
           (magit-log-other
            (list (concat prev-rev "..HEAD"))
            '("--patch")
            '("debian/"))))))))

(defun dd-build-binary (&optional args)
  "Run a binary build of the package."
  (interactive (list (transient-args 'dd-build)))
  (dd-with-package-dir nil
    (let ((name (concat "dd-sbuild: " (f-abbrev default-directory))))
      (apply #'dd-aio-run-in-comint
             name "gbp" "buildpackage" "--git-builder=sbuild" args))))

(defun dd-build-source (&optional args)
  "Run a source build of the package."
  (interactive (list (transient-args 'dd-build)))
  (dd-with-package-dir nil
    (let ((name (concat "dd-sbuild: " (f-abbrev default-directory))))
      (apply #'dd-aio-run-in-comint
             name "gbp" "buildpackage"
             "--git-tag"
             "--git-retag"
             "--git-builder=dpkg-buildpackage"
             "-nc" "-S" args))))

(defun dd-push ()
  (interactive)
  (dd-with-package-dir nil
    (let ((name (concat "dd-gbp: " (f-abbrev default-directory))))
      (dd-aio-run-in-comint name "gbp" "push"))))

(defun dd-import-new-upstream ()
  (interactive)
  (dd-with-package-dir nil
    (let ((name (concat "dd-gbp: " (f-abbrev default-directory))))
      (dd-aio-run-in-comint name "gbp" "import-orig" "--uscan"))))

(defun dd-changes-since-last ()
  (interactive)
  (request
    (concat
     "https://api.ftp-master.debian.org/dsc_in_suite/unstable/"
     (url-hexify-string (dd-get-package-name)))
    :parser 'json-read
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (-when-let* ((latest (-max-by
                             'string-greaterp
                             (--map (alist-get 'version it) data))))
         (magit-log-other
          (list (concat "debian/" latest "..HEAD"))
          '("--patch" "--irreversible-delete")
          '("debian/")))))))

(defun dd-aio-run-in-comint (name program &rest args)
  "Run a command in a comint buffer, returning a promise."
  (-when-let* ((promise (aio-promise))
               (buf (apply #'make-comint (append (list name program nil) args))))
    (set-process-sentinel
     (get-buffer-process buf)
     (lambda (p _e)
       (when (memq (process-status p) '(exit signal))
         (aio-resolve promise (lambda () nil)))))
    (display-buffer buf)
    (with-current-buffer buf
      (comint-clear-buffer)
      (goto-char (point-max)))
    promise))

(defun dd-upload-changes (changes-file)
  "Upload a .changes file with dput."
  (interactive)
  (let ((name (concat "dd-upload: " (f-abbrev changes-file))))
    (dd-aio-run-in-comint name "dput" changes-file)))

(defun dd-sign-changes (changes-file)
  "Sign a .changes file with debsign."
  (interactive)
  (let ((name (concat "dd-sign: " (f-abbrev changes-file))))
    (dd-aio-run-in-comint name "debsign" "--no-re-sign" changes-file)))

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
  :filter-predicates
  '(dd-changes-signed?
    dd-changes-source-only?)
  :hint #'dd--changes-list-hint
  :sort-key '(date . t)
  :revert-confirm? nil)

(let ((map dd-changes-list-mode-map))
  (define-key map (kbd "u") #'dd-changes-list-upload)
  (define-key map (kbd "s") #'dd-changes-list-sign))

(defvar dd-changes-list-default-hint
  '(("\\[dd-changes-list-upload]") " Upload;\n"
    ("\\[dd-changes-list-sign]") " Sign;\n"))

(defun dd--changes-list-hint ()
  "Return a hint string to display in the echo area."
  (bui-format-hints
   dd-changes-list-default-hint
   (bui-default-hint)))

(defun dd-changes-list-upload ()
  (interactive)
  (dd-upload-changes (bui-list-current-id)))

(aio-defun dd-changes-list-sign ()
  (interactive)
  (let ((buf (current-buffer)))
    (aio-await (dd-sign-changes (bui-list-current-id)))
    (with-current-buffer buf
      (revert-buffer nil t))))

(defun dd-changes-signed? (entry)
  "Return non-nil, if .changes ENTRY is signed."
  (bui-entry-non-void-value entry 'signed))

(defun dd-changes-source-only? (entry)
  "Return non-nil, if .changes ENTRY is source-only."
  (s-equals? "source" (bui-entry-non-void-value entry 'arch)))

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
  (parsec-or
   (parsec-between
    (parsec-one-of ?\s ?\t)
    (parsec-eol-or-eof)
    (parsec-many-s (parsec-none-of ?\n ?\r)))
   (cons
    (parsec-return (parsec-many1-s (parsec-re "[!-9;-~]"))
      (parsec-str ":")
      (parsec-many (parsec-one-of ?\s ?\t)))
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

(defun dd-magit-debcommit (&optional args)
  (interactive (list (transient-args 'magit-commit)))
  (dd-with-package-dir
   (magit-with-editor
     (apply #'magit-start-process "debcommit" nil args))))

(defun dd-gbp-pq-run (action &optional args)
  "Invoke gbp-pq.
\n(gbp pq ARGS ACTION)"
  (dd-with-package-dir nil
    (apply #'process-file "gbp" nil nil nil "pq" (-concat args (list action)))))

(defun dd-gbp-pq-export (&optional args)
  (interactive (list (transient-args 'dd-gbp-pq)))
  (dd-gbp-pq-run "export" args))

(defun dd-gbp-pq-import (&optional args)
  (interactive (list (transient-args 'dd-gbp-pq)))
  (dd-gbp-pq-run "import" args))

(defun dd-gbp-pq-rebase (&optional args)
  (interactive (list (transient-args 'dd-gbp-pq)))
  (dd-gbp-pq-run "rebase" args))

(defun dd-gbp-pq-drop (&optional args)
  (interactive (list (transient-args 'dd-gbp-pq)))
  (dd-gbp-pq-run "drop" args))

(defun dd-gbp-pq-switch (&optional args)
  (interactive (list (transient-args 'dd-gbp-pq)))
  (dd-gbp-pq-run "switch" args))

(transient-append-suffix 'magit-commit "c"
  '("d" "debcommit" dd-magit-debcommit))

(transient-define-prefix dd-dispatch ()
  "Invoke a command that acts on the Debian package of the visited file."
  ["Actions"
   [("b" "Build" dd-build)
    ("c" "Log since last version" dd-changes-since-last)
    ("d" "dch" dd-dch-dispatch)
    ("i" "Import" dd-import-new-upstream)
    ("l" "Lintian Brush" dd-lintian-brush)
    ("P" "Push" dd-push)
    ("Q" "Patch queue" dd-gbp-pq)
    ("s" "Switch / clone" dd-clone-package)
    ("u" "List .changes" dd-list-changes)]])

(transient-define-prefix dd-gbp-pq ()
  :man-page "gbp-pq"
  ["Arguments"
   ("-d" "Drop patch-queue after export" "--drop")
   ("-t" "Try applying onto earlier commits" "--time-machine=" transient-read-number-N+)]
  ["gbp pq"
   ("e" "Export" dd-gbp-pq-export)
   ("i" "Import" dd-gbp-pq-import)
   ("k" "Drop" dd-gbp-pq-drop)
   ("r" "Rebase" dd-gbp-pq-rebase)
   ("s" "Switch" dd-gbp-pq-switch)])

;;;###autoload
(with-eval-after-load 'magit-mode
  (define-key magit-mode-map "Q" 'dd-gbp-pq))

(transient-define-prefix dd-build ()
  :man-page "gbp-buildpackage"
  [["dpkg-buildpackage Arguments"
    ("-v" "Include changelogs since version" "--debbuildopt=-v" :class transient-option)]
   ["sbuild Arguments"
    (dd-build:--build-dep-resolver)]]
  ["gbp buildpackage"
   ("b" "Binary (testing)" dd-build-binary)
   ("s" "Source (upload)" dd-build-source)])

(transient-define-argument dd-build:--build-dep-resolver ()
  :description "Build-dep resolver"
  :class 'transient-option
  :key "-r"
  :argument "--build-dep-resolver="
  :choices '("apt" "aptitude" "aspcud" "xapt" "null"))

(defun dd-dch (&optional args)
  "Invoke dch.
\n(dch ARGS)"
  (interactive (list (transient-args 'dd-dch-dispatch)))
  (dd-with-package-dir nil
    (with-editor
      (message "%S" args)
      ;; (apply #'start-file-process "dch" "*dch*" "dch" args)
      )))

(defclass dd-suffix-switch (transient-suffix)
  ((argument :initarg :argument)))

(cl-defmethod transient-infix-value ((obj dd-suffix-switch))
  (and (eq this-command (transient--suffix-command obj))
       (oref obj argument)))

(transient-define-prefix dd-dch-dispatch ()
  :man-page "debchange"
  ["Arguments"]
  ["debchange"
   ("a" "Append" dd-dch :class dd-suffix-switch :argument "--append")
   ("b" "Backport" dd-dch :class dd-suffix-switch :argument "--bpo")
   ("i" "Increment" dd-dch :class dd-suffix-switch :argument "--increment")
   ("r" "Release" dd-dch :class dd-suffix-switch :argument "--release")])

(provide 'debian-devel)
;;; debian-devel.el ends here
