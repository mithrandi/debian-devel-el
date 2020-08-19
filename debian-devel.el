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

(eval-when-compile
  (require 'cl-lib))
(require 'aio)
(require 'bui)
(require 'comint)
(require 'dash)
(require 'f)
(require 'magit)
(require 'request)
(require 's)
(require 'transient)
(require 'url)

(require 'dd-deb822)

(defun dd--packages-root ()
  "The directory into which packages are cloned.

TODO: Stop hardcoding this."
  (f-full "~/debian/packages"))

(defun dd--build-area ()
  "The directory into which .changes files are placed.

TODO: Stop hardcoding this."
  (f-full "~/debian/build-area"))

(cl-defsubst dd--ensure-package (dir)
  "Ensure that DIR is non-nil."
  (if dir
      dir
    (error "No package found")))

(cl-defun dd-find-package (&optional (dir default-directory))
  "Look upward from DIR to find something that looks like a
Debian package."
  (f--traverse-upwards (f-exists? (f-expand "debian/control" it)) dir))

(defmacro dd-with-package-dir (dir &rest body)
  "Invoke BODY in the root of the package containing DIR."
  (declare (debug t) (indent 1))
  `(let ((default-directory (dd--ensure-package (dd-find-package ,dir))))
     ,@body))

(cl-defun dd-get-package-name (&optional (dir default-directory))
  "Get the name of the package containing DIR."
  (with-temp-buffer
    (insert-file-contents-literally
     (f-expand "debian/control" (dd--ensure-package (dd-find-package dir))))
    (s-trim
     (buffer-substring-no-properties
      (re-search-forward "^Source:")
      (line-end-position)))))

(defun dd-find-named-package (package-name)
  "Find an existing checkout of PACKAGE-NAME."
  (interactive "sPackage: ")
  (-first-item
   (f--directories
    (dd--packages-root)
    (and (s-equals? (f-filename it) package-name)
         (f-exists? (f-expand "debian/control" it)))
    t)))

(aio-defun dd-gbp-clone (package-name)
  "Clone PACKAGE-NAME."
  (interactive "sPackage: ")
  (let ((dir (f-expand package-name (dd--packages-root))))
    (aio-await
     (dd-aio-run-in-comint
      (s-concat "dd-clone: " package-name)
      "gbp" "clone" (s-concat "vcsgit:" package-name) dir))
    dir))

(aio-defun dd-clone-package (package-name)
  "Clone PACKAGE-NAME, or find an existing clone."
  (interactive "sPackage: ")
  (dired
   (or (dd-find-named-package package-name)
       (aio-await (dd-gbp-clone package-name)))))

(aio-defun dd-lintian-brush ()
  "Run lintian-brush and log any commits made."
  (interactive)
  (let ((prev-rev (magit-rev-parse "HEAD")))
    (aio-await
     (dd-with-package-dir nil
       (dd-aio-run-in-comint
        (s-concat "dd-lintian-brush: " (f-abbrev default-directory))
        "lintian-brush" "--modern" "--uncertain")))
    (magit-log-other
     (list (s-concat prev-rev "..HEAD"))
     '("--patch")
     '("debian/"))))

(defun dd-gbp-build (gbp-args)
  "Run gbp buildpackage with default settings.

GBP-ARGS: A list of strings to pass to gbp-buildpackage."
  (interactive (list (dd--args-for-program 'gbp)))
  (dd-with-package-dir nil
    (let ((name (s-concat "dd-gbp-buildpackage: " (f-abbrev default-directory))))
      (apply #'dd-aio-run-in-comint
             name "gbp" "buildpackage" gbp-args))))

(defun dd-gbp-sbuild (gbp-args dbp-args sbuild-args profiles dbo)
  "Run sbuild via gbp.

GBP-ARGS: A list of strings to pass to gbp-buildpackage.
DBP-ARGS: A list of strings to pass to dpkg-buildpackage.
SBUILD-ARGS: A list of strings to pass to sbuild.
PROFILES: The build profiles to activate.
DBO: Options to pass in the DEB_BUILD_OPTIONS envvar."
  (interactive (list (dd--args-for-program 'gbp)
                     (dd--args-for-program 'dpkg-buildpackage)
                     (dd--args-for-program 'sbuild)
                     (dd--build-profiles)
                     (dd--build-options)))
  (dd-with-package-dir nil
    (let* ((name (s-concat "dd-sbuild: " (f-abbrev default-directory)))
           (args
            (-concat
             gbp-args
             sbuild-args
             (list (s-concat "--profiles=" profiles))
             (--map (s-prepend "--debbuildopt=" it) dbp-args)))
           (process-environment
            (cons (s-concat "DEB_BUILD_OPTIONS=" dbo)
                  process-environment)))
      (apply #'dd-aio-run-in-comint
             name "gbp" "buildpackage" "--git-builder=sbuild" args))))

(defun dd-gbp-push ()
  (interactive)
  (dd-with-package-dir nil
    (let ((name (s-concat "dd-gbp: " (f-abbrev default-directory))))
      (dd-aio-run-in-comint name "gbp" "push"))))

(defun dd-gbp-pull ()
  (interactive)
  (dd-with-package-dir nil
    (let ((name (s-concat "dd-gbp: " (f-abbrev default-directory))))
      (dd-aio-run-in-comint name "gbp" "pull"))))

(defun dd-gbp-tag ()
  (interactive)
  (dd-with-package-dir nil
    (let ((name (s-concat "dd-gbp: " (f-abbrev default-directory))))
      (dd-aio-run-in-comint name "gbp" "tag"))))

(defun dd-import-new-upstream ()
  (interactive)
  (dd-with-package-dir nil
    (let ((name (s-concat "dd-gbp: " (f-abbrev default-directory))))
      (dd-aio-run-in-comint name "gbp" "import-orig" "--uscan"))))

(defun dd-changes-since-last ()
  (interactive)
  (request
    (s-concat
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
          (list (s-concat "debian/" latest "..HEAD"))
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
  (let ((name (s-concat "dd-upload: " (f-abbrev changes-file))))
    (dd-aio-run-in-comint name "dput" changes-file)))

(defun dd-sign-changes (changes-file)
  "Sign a .changes file with debsign."
  (interactive)
  (let ((name (s-concat "dd-sign: " (f-abbrev changes-file))))
    (dd-aio-run-in-comint name "debsign" "--no-re-sign" changes-file)))

(defun dd--changes->entry (changes-file)
  (let ((fields (dd-parse-changes changes-file)))
    `((id . ,changes-file)
      (name . ,(f-filename changes-file))
      (file-name . ,changes-file)
      (date . ,(file-attribute-modification-time (file-attributes changes-file)))
      (dist . ,(alist-get 'distribution fields))
      (arch . ,(alist-get 'architecture fields))
      (signed . ,(alist-get '*signed* fields)))))

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
            (dist nil 20 t)
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

(transient-append-suffix 'magit-tag "r"
  '("g" "gbp tag" dd-gbp-tag))

(transient-append-suffix 'magit-pull "e"
  '("g" "gbp pull" dd-gbp-pull))

(transient-append-suffix 'magit-push "e"
  '("g" "gbp push" dd-gbp-push))

(transient-define-prefix dd-dispatch ()
  "Invoke a command that acts on the Debian package of the visited file."
  ["Actions"
   [("b" "gbp buildpackage" dd-build)
    ("c" "Log since last version" dd-changes-since-last)
    ("d" "dch" dd-dch-dispatch)
    ("F" "gbp pull" dd-gbp-pull)
    ("i" "gbp import-orig" dd-import-new-upstream)
    ("l" "Lintian Brush" dd-lintian-brush)
    ("P" "gbp push" dd-gbp-push)
    ("Q" "gbp pq" dd-gbp-pq)
    ("s" "Switch / clone" dd-clone-package)
    ("t" "gbp tag" dd-gbp-tag)
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

(defclass dd-infix ()
  ((program :initarg :program))
  "An infix argument for a specific program.

This may be ignored if not applicable, or transformed if being
passed through another program.

Mix this class in with one of the other transient-* infix
classes."
  :abstract t)

(defclass dd-option (transient-option dd-infix) ()
  "An option for a specific program.")

(defclass dd-switches (transient-switches dd-infix) ()
  "Switches for a specific program.")

(defclass dd-on-off-switch (dd-switches) ()
  "A switch with --foo and --no-foo forms.")

(cl-defmethod initialize-instance :after ((obj dd-on-off-switch) &rest _)
  (pcase-let ((`(,arg-p . ,arg-s) (oref obj argument)))
    (oset obj argument-format (s-concat arg-p "%s" arg-s))
    (oset obj argument-regexp
          (rx-to-string `(group ,arg-p (group (? "no-")) ,arg-s)))
    (oset obj choices '("" "no-"))))

(cl-defmethod transient-format-value ((obj dd-on-off-switch))
  (with-slots (value argument argument-format) obj
    (format (propertize argument-format
                        'face (if value
                                  'transient-value
                                'transient-inactive-value))
            (concat
             (propertize "[" 'face 'transient-inactive-value)
             (propertize "no-" 'face
                         (if (s-equals? (format argument-format "no-") value)
                             'transient-value
                           'transient-inactive-value))
             (propertize "]" 'face 'transient-inactive-value)))))

(defun dd--args-for-program (program)
  "Get the transient infix arguments for PROGRAM."
  (--keep
   (when (and (object-of-class-p it 'dd-infix)
              (eq program (oref it program)))
     (transient-infix-value it))
   transient-current-suffixes))

(transient-define-prefix dd-build ()
  :man-page "gbp-buildpackage"
  ["git-buildpackage Arguments"
   (dd-build:--git-ignore-branch)]
  [["dpkg-buildpackage Arguments"
    ("-v" "Include changelogs since version" "-v"
     :class dd-option :program dpkg-buildpackage)
    (dd-build:--build-profiles)]
   ["sbuild Arguments"
    (dd-build:--build-dep-resolver)
    (dd-build:--run-autopkgtest)]]
  ["gbp buildpackage"
   ("b" "Default" dd-gbp-build)
   ("s" "Sbuild" dd-gbp-sbuild)])

(transient-define-argument dd-build:--git-ignore-branch ()
  :description "Ignore not running on the default branch"
  :class 'dd-on-off-switch
  :program 'gbp
  :key "-gi"
  :argument '("--git-" . "ignore-branch"))

(transient-define-argument dd-build:--build-dep-resolver ()
  :description "Build-dep resolver"
  :class 'dd-option
  :program 'sbuild
  :key "-r"
  :argument "--build-dep-resolver="
  :choices '("apt" "aptitude" "aspcud" "xapt" "null"))

(transient-define-argument dd-build:--build-profiles ()
  :description "Build profiles, comma-separated"
  :class 'dd-option
  :program nil
  :key "-P"
  :argument "--build-profiles=")

(transient-define-argument dd-build:--run-autopkgtest ()
  :description "Run autopkgtest after build"
  :class 'dd-on-off-switch
  :program 'sbuild
  :key "-a"
  :argument '("--" . "run-autopkgtest"))

(defun dd--build-profiles ()
  (or (oref
       (--first
        (eq (transient--suffix-command it)
            'dd-build:--build-profiles)
        transient-current-suffixes)
       value)
      ""))

(defun dd--build-options ()
  (let ((profiles (s-split "," (dd--build-profiles))))
    (s-join
     " "
     (-concat
      (when (-contains? profiles "nocheck")
        '("nocheck"))
      (when (-contains? profiles "nodoc")
        '("nodoc"))))))

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
  (message "%S : %S" obj (transient-suffix-object))
  (and (eq obj (transient-suffix-object))
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
