;;; use-package-vc-testdrive.el --- Live drive for use-package-vc  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

;; A self-contained check of `use-package-vc' against a throwaway Git
;; repository, run inside the live Emacs:
;;
;;   M-x use-package-vc-testdrive
;;
;; It builds an "upstream" repository under a temp directory with a tiny
;; package in it, installs that package with `package-vc-install' into a
;; private `package-user-dir', adds two commits upstream, and then drives
;; the *VC Packages* buffer: fetch, read the Behind column, `U`, `RET`
;; for the log, `x` with the review prompt pre-answered, and checks that
;; the checkout moved to the new head and the row says "upgraded".
;; The fixture is loaded before the upgrade, so the drive also checks
;; that `use-package-vc-reload-after-upgrade' brings the new definition
;; into memory, and that a NAME-reload command wins over unloading.
;; Every check reads buffer text or files; nothing asks you to look.
;;
;; Nothing outside the temp directory is touched or read: `package-user-dir',
;; `package-alist', `package-vc-selected-packages' and `custom-file'
;; are rebound for the duration, so your real packages and custom.el
;; are neither read nor written.  Every `use-package-vc-*' option that
;; can change an outcome is bound in the drive too, so the result does
;; not depend on the running configuration: keep it that way when
;; adding steps.  The temp directory is removed at the end unless a
;; step failed, in which case its path is in the report.
;;
;; The report is a Markdown buffer *use-package-vc testdrive*: PASS/FAIL
;; summary at the top, one line per step, state dumps under failures.
;; `q' closes it.  The whole drive takes a few seconds.
;;
;; A short "tour" section at the end lists what only eyes can judge:
;; open the real *VC Packages* buffer and look at those.

;;; Code:

(require 'use-package-vc)
(require 'package-vc)
(require 'vc-git)
(require 'seq)

(defvar use-package-vc-testdrive--steps nil
  "Reversed list of (NAME STATUS DETAIL) for the report.")

(defvar use-package-vc-testdrive--dump nil
  "Reversed list of (NAME . TEXT) state dumps recorded under failures.")

(defun use-package-vc-testdrive--step (name ok &optional detail)
  "Record step NAME as passed when OK, else failed, with DETAIL."
  (push (list name (if ok 'pass 'fail) detail) use-package-vc-testdrive--steps)
  (message "use-package-vc testdrive: %s %s" (if ok "ok  " "FAIL") name)
  ok)

(defun use-package-vc-testdrive--dump (name text)
  "Attach TEXT to the report under NAME."
  (push (cons name text) use-package-vc-testdrive--dump))

(defun use-package-vc-testdrive--git (dir &rest args)
  "Run git ARGS in DIR; signal on failure; return trimmed output."
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (eql 0 (apply #'vc-git-command (current-buffer) t nil args))
        (error "git %s failed in %s: %s" (string-join args " ") dir
               (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun use-package-vc-testdrive--make-upstream (dir)
  "Create a Git repository in DIR holding package `upvc-fixture' with one commit."
  (make-directory dir t)
  (use-package-vc-testdrive--git dir "init" "-q" "-b" "main")
  (use-package-vc-testdrive--git dir "config" "user.email" "testdrive@example.com")
  (use-package-vc-testdrive--git dir "config" "user.name" "Testdrive")
  (with-temp-file (expand-file-name "upvc-fixture.el" dir)
    (insert ";;; upvc-fixture.el --- fixture  -*- lexical-binding: t; -*-\n"
            ";; Version: 0.1\n"
            ";;; Code:\n"
            "(defun upvc-fixture-value () 1)\n"
            "(provide 'upvc-fixture)\n"
            ";;; upvc-fixture.el ends here\n"))
  (use-package-vc-testdrive--git dir "add" "upvc-fixture.el")
  (use-package-vc-testdrive--git dir "commit" "-q" "-m" "Initial fixture"))

(defun use-package-vc-testdrive--advance-upstream (dir)
  "Add two commits to DIR that change the fixture's value to 3."
  (dolist (n '(2 3))
    (with-temp-file (expand-file-name "upvc-fixture.el" dir)
      (insert ";;; upvc-fixture.el --- fixture  -*- lexical-binding: t; -*-\n"
              (format ";; Version: 0.%d\n" n)
              ";;; Code:\n"
              (format "(defun upvc-fixture-value () %d)\n" n)
              "(provide 'upvc-fixture)\n"
              ";;; upvc-fixture.el ends here\n"))
    (use-package-vc-testdrive--git dir "commit" "-q" "-am" (format "Bump to %d" n))))

(defun use-package-vc-testdrive--row (name)
  "The row plist for NAME in the current *VC Packages* buffer."
  (cdr (assq name use-package-vc--rows)))

(defun use-package-vc-testdrive--buffer-text (buf)
  "Text of BUF without properties."
  (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))

(defun use-package-vc-testdrive--wait (pred &optional seconds)
  "Run the event loop until PRED returns non-nil or SECONDS pass."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (funcall pred)))

;;;###autoload
(defun use-package-vc-testdrive ()
  "Drive `use-package-vc' against a temporary upstream and report."
  (interactive)
  (setq use-package-vc-testdrive--steps nil
        use-package-vc-testdrive--dump nil)
  (let* ((root (make-temp-file "upvc-testdrive-" t))
         (upstream (expand-file-name "upstream" root))
         (user-dir (expand-file-name "elpa" root))
         (name 'upvc-fixture)
         (failed nil)
         ;; keep the drive out of the real package state and custom.el
         (package-user-dir user-dir)
         (package-alist nil)
         (package-activated-list nil)
         (package-vc-selected-packages nil)
         (custom-file (expand-file-name "custom.el" root))
         (use-package-vc-fetch-on-open nil)
         (use-package-vc-review-policy t)
         (use-package-vc-reload-after-upgrade 'always)
         (reload-calls 0)
         ;; answer the review prompt with "yes" without a human
         (answers '(?y))
         (asked nil))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (prompt choices &rest _)
                 (push prompt asked)
                 (assq (pop answers) choices)))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val &rest _) (set sym val))))
      (unwind-protect
          (condition-case err
              (progn
                ;; 1. upstream with one commit
                (use-package-vc-testdrive--make-upstream upstream)
                (use-package-vc-testdrive--step
                 "create upstream repository"
                 (file-exists-p (expand-file-name ".git" upstream)))

                ;; 2. install from it as a VC package
                (make-directory user-dir t)
                (package-vc-install (list name :url upstream :vc-backend 'Git))
                (let ((desc (cadr (assq name package-alist))))
                  (use-package-vc-testdrive--step
                   "package-vc-install from local path"
                   (and desc (package-vc-p desc))
                   (format "package-alist: %S" (mapcar #'car package-alist))))

                ;; 3. the list shows it, at 0 behind
                (use-package-vc-list 'no-fetch)
                (let* ((buf (get-buffer "*VC Packages*"))
                       (row (with-current-buffer buf (use-package-vc-testdrive--row name))))
                  (use-package-vc-testdrive--step
                   "listed with branch main and 0 behind"
                   (and row (equal (plist-get row :branch) "main")
                        (eql (plist-get row :behind) 0))
                   (format "row: %S" row))
                  (unless (and row (eql (plist-get row :behind) 0))
                    (use-package-vc-testdrive--dump "buffer after install"
                                                    (use-package-vc-testdrive--buffer-text buf)))

                  ;; 4. upstream moves on
                  (use-package-vc-testdrive--advance-upstream upstream)
                  (with-current-buffer buf (use-package-vc-refresh))
                  (let ((row (with-current-buffer buf (use-package-vc-testdrive--row name))))
                    (use-package-vc-testdrive--step
                     "after fetch: 2 behind, upstream subject shown"
                     (and (eql (plist-get row :behind) 2)
                          (string-match-p "Bump to 3" (or (plist-get row :last) "")))
                     (format "behind=%S last=%S" (plist-get row :behind) (plist-get row :last))))
                  (let ((text (use-package-vc-testdrive--buffer-text buf)))
                    (use-package-vc-testdrive--step
                     "Behind column renders \"2\""
                     (string-match-p "upvc-fixture.*\\b2\\b" text)
                     nil)
                    (unless (string-match-p "upvc-fixture.*\\b2\\b" text)
                      (use-package-vc-testdrive--dump "buffer after fetch" text)))

                  ;; 5. RET shows the two incoming commits
                  (with-current-buffer buf
                    (use-package-vc--goto name)
                    (use-package-vc-show-log))
                  (let* ((log (get-buffer (format "*VC Packages: %s log*" name)))
                         (text (and log (use-package-vc-testdrive--buffer-text log))))
                    (use-package-vc-testdrive--step
                     "RET log lists both incoming commits"
                     (and text (string-match-p "Bump to 2" text) (string-match-p "Bump to 3" text))
                     nil)
                    (when log (kill-buffer log)))

                  ;; 6. U marks it; load the old fixture so the reload has work to do
                  (require name)
                  (with-current-buffer buf (use-package-vc-mark-upgrades))
                  (use-package-vc-testdrive--step
                   "U marks the package"
                   (plist-get (with-current-buffer buf (use-package-vc-testdrive--row name)) :marked))

                  ;; 7. x: review asked once, answered yes, pull done, row upgraded
                  (with-current-buffer buf (use-package-vc-execute))
                  (use-package-vc-testdrive--step
                   "x asked the review question once"
                   (= (length asked) 1)
                   (format "prompts: %S" (nreverse (copy-sequence asked))))
                  (let* ((checkout (package-vc--checkout-dir (cadr (assq name package-alist))))
                         (ok (use-package-vc-testdrive--wait
                              (lambda ()
                                (equal (use-package-vc-testdrive--git checkout "rev-parse" "HEAD")
                                       (use-package-vc-testdrive--git upstream "rev-parse" "HEAD")))
                              use-package-vc-upgrade-timeout)))
                    (use-package-vc-testdrive--step
                     "checkout HEAD equals upstream HEAD"
                     ok
                     (format "checkout %s upstream %s"
                             (use-package-vc-testdrive--git checkout "rev-parse" "--short" "HEAD")
                             (use-package-vc-testdrive--git upstream "rev-parse" "--short" "HEAD"))))
                  (let ((row (with-current-buffer buf (use-package-vc-testdrive--row name))))
                    (use-package-vc-testdrive--step
                     "row state is upgraded and 0 behind"
                     (and (eq (plist-get row :state) 'upgraded)
                          (eql (plist-get row :behind) 0))
                     (format "state=%S behind=%S" (plist-get row :state) (plist-get row :behind)))
                    (unless (eq (plist-get row :state) 'upgraded)
                      (use-package-vc-testdrive--dump "buffer after execute"
                                                      (use-package-vc-testdrive--buffer-text buf))))

                  ;; 8. `always': the unload+require made the new definition live
                  (use-package-vc-testdrive--step
                   "reload-after-upgrade always: loaded fixture now returns 3"
                   (eql (ignore-errors (funcall 'upvc-fixture-value)) 3)
                   (format "upvc-fixture-value => %S, featurep %S"
                           (ignore-errors (funcall 'upvc-fixture-value)) (featurep name)))

                  ;; 9. a NAME-reload command is preferred and no question is asked
                  (cl-letf (((symbol-function 'upvc-fixture-reload)
                             (lambda () (interactive) (cl-incf reload-calls)))
                            ((symbol-function 'read-multiple-choice)
                             (lambda (&rest _) (error "prompt shown despite NAME-reload"))))
                    (use-package-vc--reload-packages (list name)))
                  (use-package-vc-testdrive--step
                   "NAME-reload command is called instead of unload-feature"
                   (= reload-calls 1)
                   (format "upvc-fixture-reload calls: %d" reload-calls))
                  (kill-buffer buf)))
            (error
             (use-package-vc-testdrive--step "drive aborted" nil (error-message-string err))))
        (setq failed (seq-some (lambda (s) (eq (nth 1 s) 'fail))
                               use-package-vc-testdrive--steps))
        (unless failed
          (ignore-errors (delete-directory root t)))
        (use-package-vc-testdrive--report (and failed root))))))

(defun use-package-vc-testdrive--report (kept-dir)
  "Show the Markdown report; KEPT-DIR is the temp root if it was kept."
  (let* ((steps (reverse use-package-vc-testdrive--steps))
         (fails (seq-count (lambda (s) (eq (nth 1 s) 'fail)) steps))
         (buf (get-buffer-create "*use-package-vc testdrive*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# use-package-vc testdrive: %s\n\n"
                        (if (zerop fails) "PASS" (format "FAIL (%d)" fails))))
        (insert (format "%d steps, %d failed, %s\n\n"
                        (length steps) fails
                        (format-time-string "%F %T")))
        (dolist (s steps)
          (insert (format "- %s **%s**%s\n"
                          (if (eq (nth 1 s) 'pass) "✓" "✗")
                          (nth 0 s)
                          (if (and (nth 2 s) (eq (nth 1 s) 'fail))
                              (concat " — " (nth 2 s))
                            ""))))
        (when kept-dir
          (insert (format "\nTemp directory kept for inspection: `%s`\n" kept-dir)))
        (when use-package-vc-testdrive--dump
          (insert "\n## State at failures\n")
          (dolist (d (reverse use-package-vc-testdrive--dump))
            (insert (format "\n### %s\n\n```\n%s\n```\n" (car d) (cdr d)))))
        (insert "\n## Tour (eyes only)\n\n"
                "The drive's packages are gone with its temp directory, so the\n"
                "tour needs any Emacs with at least one `:vc' package installed.\n"
                "`M-x use-package-vc-list` and look, without pressing `x`:\n\n"
                "- the Behind column is right-aligned and amber for non-zero\n"
                "- the Upstream column shows a relative date and a subject\n"
                "- `RET` opens the log in another window and `q` closes it\n")
        (goto-char (point-min))
        (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
        (view-mode 1)))
    (pop-to-buffer buf)
    (message "use-package-vc testdrive: %s" (if (zerop fails) "PASS" (format "%d FAILED" fails)))))

(provide 'use-package-vc-testdrive)
;;; use-package-vc-testdrive.el ends here
