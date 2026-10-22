;;; use-package-vc.el --- Review and upgrade :vc packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Maintainer: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, package
;; URL: https://github.com/tzz/use-package-vc

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

;; Packages installed with `use-package' `:vc' (or `package-vc-install')
;; do not appear as upgradable in `list-packages', because there is no
;; archive version to compare against, and `package-vc-upgrade-all'
;; pulls every repository without showing what changed.  This file adds
;; the missing review step: a buffer listing every VC package with how
;; far behind its upstream it is, the incoming commits, and an upgrade
;; command that pulls one package at a time, honouring
;; `package-review-policy' (Emacs 31) before each pull.  The review is
;; Emacs's own `package-review', run on a temporary checkout of the
;; upstream head against the installed one, so anything that wraps
;; `package-review' (efrit's model-assisted review, for one) applies to
;; VC upgrades exactly as it does to archive installs.
;;
;;   M-x use-package-vc-list
;;
;; In the *VC Packages* buffer:
;;
;;   g    fetch every repository and refresh the table
;;   RET  show the incoming commits for the package at point
;;   d    show the incoming diff
;;   U    mark every package that is behind its upstream
;;   m/u  mark / unmark the package at point
;;   x    upgrade the marked packages, one after another
;;   q    bury the buffer
;;
;; Only Git repositories are inspected for "behind" status; packages
;; under other backends are listed and can be upgraded but show no
;; commit count.  All VCS work goes through `vc-git-command'; nothing
;; is run through a shell.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'package-vc)
(require 'tabulated-list)
(require 'vc)
(require 'vc-git)
(require 'seq)

(defgroup use-package-vc nil
  "Review and upgrade packages installed from version control."
  :group 'use-package
  :version "31.1")

(defcustom use-package-vc-fetch-on-open t
  "Non-nil means `use-package-vc-list' fetches every repository when it opens.
Set to nil to open instantly and fetch on demand with `g'."
  :type 'boolean)

(defcustom use-package-vc-upgrade-timeout 120
  "Seconds to wait for one package's pull and rebuild before giving up."
  :type 'natnum)

(defcustom use-package-vc-fetch-timeout 60
  "Seconds to wait for all fetches started by `g' before giving up on the rest."
  :type 'natnum)

(defcustom use-package-vc-review-policy 'inherit
  "When to show incoming commits and ask before pulling a package.
`inherit' follows `package-review-policy' (Emacs 31; on older Emacs it
means `never').  t reviews every package that is behind, nil none."
  :type '(choice (const :tag "Follow package-review-policy" inherit)
                 (const :tag "Always" t)
                 (const :tag "Never" nil)))

(defcustom use-package-vc-reload-after-upgrade 'ask
  "What to do with an upgraded package's already-loaded code.
`package-vc-upgrade' pulls, byte-compiles and re-activates the
autoloads, but functions already loaded in this session keep their old
definitions until the files are loaded again.  If the package defines
a reload command named NAME-reload, it is always called.  Otherwise:

  ask     ask once per `x' whether to unload and re-require the rest;
          answering `a' switches to `always' for the rest of the session
  always  unload each upgraded feature and require it again
  never   leave loaded code alone and say so

Unloading with `unload-feature' can drop state a package keeps in
variables; packages that care ship their own NAME-reload."
  :type '(choice (const :tag "Ask once per run" ask)
                 (const :tag "Always unload and require" always)
                 (const :tag "Never" never)))

(defface use-package-vc-behind
  '((t :inherit warning))
  "Face for the commit count of a package that is behind its upstream.")

(defface use-package-vc-current
  '((t :inherit shadow))
  "Face for a package that is at its upstream head.")

(defface use-package-vc-failed
  '((t :inherit error))
  "Face for a package whose fetch or upgrade failed.")

;;;; Repository queries

(defun use-package-vc--packages ()
  "Installed package descriptors that came from version control."
  (let (descs)
    (dolist (entry package-alist)
      (dolist (desc (cdr entry))
        (when (package-vc-p desc)
          (push desc descs))))
    (sort descs (lambda (a b)
                  (string< (package-desc-name a) (package-desc-name b))))))

(defun use-package-vc--dir (desc)
  "Checkout directory of DESC, or nil."
  (let ((dir (package-vc--checkout-dir desc)))
    (and dir (file-directory-p dir) dir)))

(defun use-package-vc--git-p (dir)
  "Non-nil if DIR is a Git working tree."
  (eq (vc-responsible-backend dir t) 'Git))

(defun use-package-vc--git (dir &rest args)
  "Run git with ARGS in DIR; return trimmed stdout, or nil on failure.
OKSTATUS t makes `vc-git-command' return the exit status instead of
signalling, so a missing upstream is a nil result, not an error."
  (let ((default-directory dir))
    (with-temp-buffer
      (when (eql 0 (apply #'vc-git-command (current-buffer) t nil args))
        (string-trim (buffer-string))))))

(defun use-package-vc--upstream (dir)
  "The upstream ref of DIR's current branch, such as \"origin/main\", or nil."
  (use-package-vc--git dir "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}"))

(defun use-package-vc--branch (dir)
  "Current branch name in DIR, or the short HEAD when detached."
  (let ((b (use-package-vc--git dir "rev-parse" "--abbrev-ref" "HEAD")))
    (if (or (null b) (string= b "HEAD"))
        (concat "detached@" (or (use-package-vc--git dir "rev-parse" "--short" "HEAD") "?"))
      b)))

(defun use-package-vc--behind (dir)
  "Commits DIR's HEAD is behind its upstream, or nil if unknown."
  (when-let* ((up (use-package-vc--upstream dir))
              (n (use-package-vc--git dir "rev-list" "--count" (format "HEAD..%s" up))))
    (string-to-number n)))

(defun use-package-vc--last-upstream-commit (dir)
  "One line describing the newest upstream commit of DIR, or nil."
  (when-let* ((up (use-package-vc--upstream dir)))
    (use-package-vc--git dir "log" "-1" "--format=%cr  %s" up)))

(defun use-package-vc--fetch-all (dirs)
  "Fetch every repository in DIRS at once; return an alist (DIR . ERROR-OR-NIL).
Fetches only touch their own .git, so they run in parallel: one
`git fetch' process per DIR through `vc-git-command' in async mode, a
sentinel per process, and one wait loop until all have exited or
`use-package-vc-fetch-timeout' passes.  A DIR still running at the
deadline is reported as an error and its process killed."
  (let ((pending nil) (results nil) (total (length dirs)))
    (dolist (dir dirs)
      (let* ((default-directory dir)
             (buf (generate-new-buffer (format " *use-package-vc fetch %s*" (file-name-nondirectory
                                                                              (directory-file-name dir)))))
             (proc (with-current-buffer buf
                     (vc-git-command buf 'async nil "fetch" "--quiet"))))
        (if (not (processp proc))
            (push (cons dir "could not start git fetch") results)
          (push (cons dir proc) pending)
          (set-process-sentinel
           proc
           (lambda (p _event)
             (unless (process-live-p p)
               (let ((text (with-current-buffer (process-buffer p)
                             (string-trim (buffer-string)))))
                 (push (cons dir (and (/= 0 (process-exit-status p))
                                      (if (string-empty-p text)
                                          (format "git fetch exited %d" (process-exit-status p))
                                        text)))
                       results)
                 (kill-buffer (process-buffer p)))))))))
    (let ((deadline (+ (float-time) use-package-vc-fetch-timeout)))
      (while (and (< (length results) total) (< (float-time) deadline))
        (message "use-package-vc: fetching %d/%d..." (length results) total)
        (accept-process-output nil 0.1))
      ;; anything still running is late
      (pcase-dolist (`(,dir . ,proc) pending)
        (unless (assoc dir results)
          (when (process-live-p proc)
            (delete-process proc)
            (ignore-errors (kill-buffer (process-buffer proc))))
          (push (cons dir (format "fetch did not finish in %ds" use-package-vc-fetch-timeout))
                results))))
    (message "use-package-vc: fetched %d repositories" total)
    results))

;;;; The table

(defvar-local use-package-vc--rows nil
  "Alist (NAME . PLIST) of the last scan.
PLIST holds :desc :dir :git :branch :behind :last :error :marked :state.")

(defun use-package-vc--scan (&optional fetch)
  "Inspect every VC package; with FETCH, fetch all repositories first.
Fetches run in parallel.
Fill `use-package-vc--rows'."
  (let* ((descs (use-package-vc--packages))
         (git-dirs (delq nil (mapcar (lambda (d)
                                       (let ((dir (use-package-vc--dir d)))
                                         (and dir (use-package-vc--git-p dir) dir)))
                                     descs)))
         (fetch-results (and fetch git-dirs (use-package-vc--fetch-all git-dirs)))
         (rows nil))
    (dolist (desc descs)
      (let* ((name (package-desc-name desc))
             (dir (use-package-vc--dir desc))
             (git (and dir (use-package-vc--git-p dir)))
             (old (cdr (assq name use-package-vc--rows)))
             (err (and git (cdr (assoc dir fetch-results)))))
        (push (cons name
                    (list :desc desc :dir dir :git git
                          :branch (and git (use-package-vc--branch dir))
                          :behind (and git (use-package-vc--behind dir))
                          :last (and git (use-package-vc--last-upstream-commit dir))
                          :error err
                          :marked (plist-get old :marked)
                          :state (or (and err 'failed) (plist-get old :state))))
              rows)))
    (setq use-package-vc--rows (nreverse rows))))

(defun use-package-vc--entries ()
  "`tabulated-list-entries' from `use-package-vc--rows'."
  (mapcar
   (lambda (row)
     (pcase-let* ((`(,name . ,p) row)
                  (behind (plist-get p :behind))
                  (state (plist-get p :state))
                  (err (plist-get p :error)))
       (list name
             (vector
              (if (plist-get p :marked) "*" " ")
              (symbol-name name)
              (or (plist-get p :branch) (if (plist-get p :dir) "non-git" "missing"))
              (cond ((null behind) "")
                    ((zerop behind) (propertize "0" 'face 'use-package-vc-current))
                    (t (propertize (number-to-string behind) 'face 'use-package-vc-behind)))
              (cond (err (propertize (car (split-string err "\n")) 'face 'use-package-vc-failed))
                    ((eq state 'upgraded) (propertize "upgraded" 'face 'use-package-vc-current))
                    ((eq state 'failed) (propertize "failed" 'face 'use-package-vc-failed))
                    ((eq state 'skipped) "skipped")
                    (t ""))
              (or (plist-get p :last) "")))))
   use-package-vc--rows))

(defun use-package-vc--row (&optional name)
  "The row plist for NAME, default the package at point."
  (let ((name (or name (tabulated-list-get-id))))
    (unless name (user-error "No package on this line"))
    (cdr (assq name use-package-vc--rows))))

(defun use-package-vc--goto (name)
  "Move point to the line for package NAME, if shown."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (eq (tabulated-list-get-id) name)))
    (forward-line 1)))

(defun use-package-vc--set (name &rest props)
  "Merge PROPS into NAME's row and redraw, keeping point on that row."
  (let ((cell (assq name use-package-vc--rows)))
    (while props
      (setcdr cell (plist-put (cdr cell) (pop props) (pop props))))
    (tabulated-list-revert)
    (use-package-vc--goto name)))

;;;; Commands in the buffer

(defun use-package-vc-refresh (&optional no-fetch)
  "Fetch every repository and redraw.  With NO-FETCH (prefix arg), only redraw."
  (interactive "P")
  (use-package-vc--scan (not no-fetch))
  (tabulated-list-revert))

(defun use-package-vc-show-log ()
  "Show the commits the package at point would receive on upgrade."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (p (use-package-vc--row name))
         (dir (plist-get p :dir))
         (up (and (plist-get p :git) (use-package-vc--upstream dir))))
    (unless up (user-error "%s: no Git upstream to compare with" name))
    (use-package-vc--show-popup
     (format "*VC Packages: %s log*" name)
     (or (use-package-vc--git dir "log" "--stat" "--format=%h %cr  %an%n    %s%n"
                              (format "HEAD..%s" up))
         "")
     name)))

(defun use-package-vc-show-diff ()
  "Show the diff the package at point would receive on upgrade."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (p (use-package-vc--row name))
         (dir (plist-get p :dir))
         (up (and (plist-get p :git) (use-package-vc--upstream dir))))
    (unless up (user-error "%s: no Git upstream to compare with" name))
    (let ((buf (get-buffer-create (format "*VC Packages: %s diff*" name))))
      (with-current-buffer buf
        (let ((inhibit-read-only t) (default-directory dir))
          (erase-buffer)
          (vc-git-command buf t nil "diff" (format "HEAD..%s" up))
          (diff-mode)
          (goto-char (point-min))
          (setq buffer-read-only t)))
      (pop-to-buffer buf))))

(defun use-package-vc--show-popup (bufname text name)
  "Display TEXT in read-only buffer BUFNAME about package NAME."
  (let ((buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (string-empty-p text)
                    (format "%s is at its upstream head.\n" name)
                  text))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buf)))

(defun use-package-vc-mark ()
  "Mark the package at point for upgrade and move down."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (unless name (user-error "No package on this line"))
    (use-package-vc--set name :marked t)
    (forward-line 1)))

(defun use-package-vc-unmark ()
  "Unmark the package at point and move down."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (unless name (user-error "No package on this line"))
    (use-package-vc--set name :marked nil)
    (forward-line 1)))

(defun use-package-vc-mark-upgrades ()
  "Mark every package that is behind its upstream."
  (interactive)
  (let ((n 0))
    (dolist (row use-package-vc--rows)
      (let ((behind (plist-get (cdr row) :behind)))
        (when (and behind (> behind 0))
          (setcdr row (plist-put (cdr row) :marked t))
          (cl-incf n))))
    (tabulated-list-revert)
    (message "%d package%s marked" n (if (= n 1) "" "s"))))

(defun use-package-vc-unmark-all ()
  "Remove every mark."
  (interactive)
  (dolist (row use-package-vc--rows)
    (setcdr row (plist-put (cdr row) :marked nil)))
  (tabulated-list-revert))

;;;; Review and upgrade

(defvar package-review-policy)          ; Emacs 31
(declare-function package-review "package" (pkg-desc pkg-dir old-desc))

(defun use-package-vc--review-p (desc)
  "Non-nil if DESC must be reviewed before pulling.
With `inherit', apply `package-review-policy' through the same selector
matcher `package-unpack' uses for archive packages; a VC package has no
archive, so `(archive ...)' rules never match it and t or
`(package NAME)' rules do."
  (pcase use-package-vc-review-policy
    ('inherit (and (boundp 'package-review-policy)
                   (fboundp 'package-matches-selector-p)
                   (package-matches-selector-p package-review-policy desc)))
    (policy policy)))

(defun use-package-vc--incoming-tree (dir)
  "Check out DIR's upstream head into a temporary directory; return it.
Uses `git worktree' so the checkout shares DIR's object store.  The
caller removes it with `use-package-vc--drop-incoming-tree'."
  (let* ((up (use-package-vc--upstream dir))
         (tmp (make-temp-file "use-package-vc-incoming-" t)))
    (unless up (error "No upstream for %s" dir))
    ;; the temp dir exists and is empty; worktree add wants it absent
    (delete-directory tmp)
    (unless (use-package-vc--git dir "worktree" "add" "--detach" "--quiet" tmp up)
      (error "Git worktree add failed in %s" dir))
    tmp))

(defun use-package-vc--drop-incoming-tree (dir tmp)
  "Remove worktree TMP registered with DIR."
  (ignore-errors (use-package-vc--git dir "worktree" "remove" "--force" tmp))
  (ignore-errors (delete-directory tmp t)))

(defun use-package-vc--review (name p)
  "Review NAME's incoming change (row plist P) and ask.
Return `yes', `skip' or `abort'.
On Emacs 31 this runs `package-review' on a temporary checkout of the
upstream head against the installed checkout, so the same diff prompt
and any advice on `package-review' (efrit) apply.  Elsewhere it shows
the incoming log and asks directly."
  (let* ((dir (plist-get p :dir))
         (desc (plist-get p :desc)))
    (if (not (fboundp 'package-review))
        (use-package-vc--review-fallback name p)
      (let ((tmp (use-package-vc--incoming-tree dir)))
        (unwind-protect
            ;; `package-review' throws `review-failed' on "no" and
            ;; deletes the dir it was given; that is our temp tree.
            (save-window-excursion
              (use-package-vc-show-log)
              (if (eq (catch 'review-failed
                        (package-review desc tmp desc)
                        'accepted)
                      'accepted)
                  'yes
                (pcase (car (read-multiple-choice
                             (format "%s not upgraded; continue with the rest?" name)
                             '((?y "yes" "Skip this package and go on")
                               (?n "no" "Stop upgrading"))))
                  (?y 'skip)
                  (_ 'abort))))
          (use-package-vc--drop-incoming-tree dir tmp))))))

(defun use-package-vc--review-fallback (name p)
  "Review prompt for NAME (row plist P) on an Emacs without `package-review'."
  (let ((dir (plist-get p :dir)))
    (save-window-excursion
      (use-package-vc-show-log)
      (pcase (car (read-multiple-choice
                   (format "Upgrade %s (%d behind)?" name (or (plist-get p :behind) 0))
                   '((?y "yes" "Pull and rebuild this package")
                     (?s "skip" "Leave this package, continue with the rest")
                     (?d "diff" "Show the full incoming diff")
                     (?b "browse" "Open the checkout in Dired")
                     (?a "abort" "Stop upgrading"))))
        (?y 'yes)
        (?s 'skip)
        (?a 'abort)
        (?d (use-package-vc-show-diff) (use-package-vc--review-fallback name p))
        (?b (dired dir) (use-package-vc--review-fallback name p))))))

(defun use-package-vc--upgrade-one (name p)
  "Run `package-vc-upgrade' for NAME (row plist P) synchronously.
Return `upgraded' or `failed'.
`package-vc-upgrade' pulls asynchronously and rebuilds from a
`vc-post-command-functions' hook; wait for that hook to fire."
  (let* ((desc (plist-get p :desc))
         (dir (plist-get p :dir))
         (done nil)
         (watcher (lambda (_command _files _flags)
                    (when (file-equal-p dir default-directory)
                      (setq done t)))))
    (add-hook 'vc-post-command-functions watcher 90)
    (unwind-protect
        (condition-case err
            (progn
              (package-vc-upgrade desc)
              (let ((deadline (+ (float-time) use-package-vc-upgrade-timeout)))
                (while (and (not done) (< (float-time) deadline))
                  (accept-process-output nil 0.2)))
              (if done 'upgraded 'failed))
          (error
           (message "use-package-vc: %s: %s" name (error-message-string err))
           'failed))
      (remove-hook 'vc-post-command-functions watcher))))

(defun use-package-vc-execute ()
  "Upgrade the marked packages one after another, reviewing when required."
  (interactive)
  (let ((marked (seq-filter (lambda (r) (plist-get (cdr r) :marked))
                            use-package-vc--rows))
        (results nil)
        (aborted nil))
    (unless marked (user-error "No packages marked; U marks those behind"))
    (catch 'abort
      (pcase-dolist (`(,name . ,p) marked)
        (let ((decision (if (use-package-vc--review-p (plist-get p :desc))
                            (use-package-vc--review name p)
                          'yes)))
          (pcase decision
            ('abort (setq aborted t) (throw 'abort nil))
            ('skip (push (cons name 'skipped) results)
                   (use-package-vc--set name :marked nil :state 'skipped))
            ('yes
             (message "use-package-vc: upgrading %s..." name)
             (let ((state (use-package-vc--upgrade-one name p)))
               (push (cons name state) results)
               (use-package-vc--set name :marked nil :state state)))))))
    ;; re-read revs so "behind" reflects the pulls
    (use-package-vc--scan nil)
    (tabulated-list-revert)
    (let ((upgraded (mapcar #'car (seq-filter (lambda (r) (eq (cdr r) 'upgraded)) results))))
      (when upgraded
        (use-package-vc--reload-packages upgraded)))
    (message "use-package-vc: %s%s"
             (mapconcat (lambda (r) (format "%s %s" (car r) (cdr r)))
                        (nreverse results) ", ")
             (if aborted "  (aborted)" ""))))

;;;; Reloading what was already in memory

(defun use-package-vc--reload-command (name)
  "The reload command of package NAME (NAME-reload) if it defines one, else nil."
  (let ((sym (intern (format "%s-reload" name))))
    (and (fboundp sym) (commandp sym) sym)))

(defun use-package-vc--reload-packages (names)
  "Bring the loaded code of upgraded NAMES up to date.
Per `use-package-vc-reload-after-upgrade': packages with a NAME-reload
command get it called; the rest are unloaded and required again when
the option says so, and only if the feature is loaded at all."
  (let (with-cmd without)
    (dolist (name names)
      (if (use-package-vc--reload-command name)
          (push name with-cmd)
        (when (featurep name) (push name without))))
    (dolist (name (nreverse with-cmd))
      (message "use-package-vc: %s-reload" name)
      (condition-case err
          (call-interactively (use-package-vc--reload-command name))
        (error (message "use-package-vc: %s-reload failed: %s" name (error-message-string err)))))
    (when without
      (setq without (nreverse without))
      (let ((do-it (pcase use-package-vc-reload-after-upgrade
                     ('always t)
                     ('never nil)
                     (_ (pcase (car (read-multiple-choice
                                     (format "Unload and re-require %s so the new code is live?"
                                             (mapconcat #'symbol-name without ", "))
                                     '((?y "yes" "Unload and require these packages again")
                                       (?n "no" "Leave the loaded code as it is")
                                       (?a "always"
                                           "Yes, and do not ask again this session.  This sets `use-package-vc-reload-after-upgrade' to `always' until Emacs exits; to make it permanent, customize that option or put (setopt use-package-vc-reload-after-upgrade 'always) in your init file"))))
                          (?y t)
                          (?a (setq use-package-vc-reload-after-upgrade 'always) t)
                          (_ nil))))))
        (if (not do-it)
            (message "use-package-vc: loaded code of %s is from before the pull; restart or load-library to refresh"
                     (mapconcat #'symbol-name without ", "))
          (dolist (name without)
            (condition-case err
                (progn
                  (unload-feature name t)
                  (require name)
                  (message "use-package-vc: reloaded %s" name))
              (error
               (message "use-package-vc: could not reload %s: %s (restart to pick it up)"
                        name (error-message-string err))))))))))

;;;; Mode

(defvar-keymap use-package-vc-mode-map
  :parent tabulated-list-mode-map
  "g"   #'use-package-vc-refresh
  "RET" #'use-package-vc-show-log
  "d"   #'use-package-vc-show-diff
  "U"   #'use-package-vc-mark-upgrades
  "m"   #'use-package-vc-mark
  "u"   #'use-package-vc-unmark
  "M-DEL" #'use-package-vc-unmark-all
  "x"   #'use-package-vc-execute)

(define-derived-mode use-package-vc-mode tabulated-list-mode "VC-Packages"
  "Major mode listing packages installed from version control.
\\{use-package-vc-mode-map}"
  (setq tabulated-list-format
        [("" 1 nil)
         ("Package" 24 t)
         ("Branch" 14 t)
         ("Behind" 6 (lambda (a b)
                       (let ((x (plist-get (cdr (assq (car a) use-package-vc--rows)) :behind))
                             (y (plist-get (cdr (assq (car b) use-package-vc--rows)) :behind)))
                         (> (or x -1) (or y -1))))
          :right-align t)
         ("State" 10 t)
         ("Upstream" 0 nil)]
        tabulated-list-padding 1
        tabulated-list-sort-key '("Behind" . nil)
        tabulated-list-entries #'use-package-vc--entries)
  (tabulated-list-init-header))

;;;###autoload
(defun use-package-vc-list (&optional no-fetch)
  "List packages installed from version control, with upgrade status.
Fetches every repository first unless NO-FETCH (a prefix argument) or
`use-package-vc-fetch-on-open' is nil.  See `use-package-vc-mode' for
the keys."
  (interactive "P")
  (let ((buf (get-buffer-create "*VC Packages*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'use-package-vc-mode)
        (use-package-vc-mode))
      (use-package-vc--scan (and use-package-vc-fetch-on-open (not no-fetch)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'use-package-vc)
;;; use-package-vc.el ends here
