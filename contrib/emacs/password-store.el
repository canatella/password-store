;;; password-store.el --- Password store (pass) support  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Svend Sorensen <svend@svends.net>

;; Author: Svend Sorensen <svend@svends.net>
;; Maintainer: Tino Calancha <tino.calancha@gmail.com>
;; Version: 2.1.5
;; URL: https://www.passwordstore.org/
;; Package-Requires: ((emacs "25") (s "1.9.0") (with-editor "2.5.11") (auth-source-pass "5.0.0"))
;; Keywords: tools pass password password-store

;; This file is not part of GNU Emacs.

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

;; This package provides functions for working with pass ("the
;; standard Unix password manager").
;;
;; http://www.passwordstore.org/

;;; Code:
(require 'seq)
(require 'with-editor)
(require 'auth-source-pass)

(defgroup password-store '()
  "Emacs mode for password-store."
  :prefix "password-store-"
  :group 'password-store)

(defcustom password-store-password-length 25
  "Default password length."
  :group 'password-store
  :type 'number)

(defcustom password-store-time-before-clipboard-restore
  (if (getenv "PASSWORD_STORE_CLIP_TIME")
      (string-to-number (getenv "PASSWORD_STORE_CLIP_TIME"))
    45)
  "Number of seconds to wait before restoring the clipboard."
  :group 'password-store
  :type 'number)

(defcustom password-store-url-field "url"
  "Field name used in the files to indicate an url."
  :group 'password-store
  :type 'string)

(defcustom password-store-hide-password t
  "Whether to use `password-store-hidden' to hide passwords in pass files."
  :type 'boolean)

(defcustom password-store-executable (executable-find "pass")
  "Pass executable."
  :group 'password-store
  :type '(file :must-match t))

(defvar password-store-timeout-timer nil
  "Timer for clearing clipboard.")


(defvar password-store-process nil "The pass process if running.")

(defvar password-store-entry nil "The pass entry being edited if any.")

(setq password-store-last-copied nil)

(defun password-store-timeout ()
  "Number of seconds to wait before clearing the password.

This function just returns `password-store-time-before-clipboard-restore'.
Kept for backward compatibility with other libraries."
  password-store-time-before-clipboard-restore)

(defun password-store--run-1 (callback &rest args)
  "Run pass with ARGS.

Nil arguments are ignored.  Calls CALLBACK with the output on success,
or outputs error message on failure."
  (let ((output ""))
    (make-process
     :name "password-store-gpg"
     :command (cons password-store-executable (delq nil args))
     :connection-type 'pipe
     :noquery t
     :filter (lambda (process text)
               (setq output (concat output text)))
     :sentinel (lambda (process state)
                 (cond
                  ((and (eq (process-status process) 'exit)
                        (zerop (process-exit-status process)))
                   (funcall callback output))
                  ((eq (process-status process) 'run) (accept-process-output process))
                  (t (error (concat "password-store: " state))))))))

(defun password-store--cmd (args)
  "Create shell command from list ARGS."
  (string-join (seq-map #'shell-quote-argument (cons password-store-executable (seq-filter #'identity args))) " "))

(defun password-store--run (&rest args)
  "Run pass with ARGS.

Nil arguments are ignored.  Returns the output on success, or
outputs error message on failure."
  (shell-command-to-string (password-store--cmd args)))


(defun password-store--run-async (&rest args)
  "Run pass asynchronously for ENTRY.

Nil arguments are ignored.  Output is discarded."
  (when (process-live-p password-store-process)
    (user-error "Process pass already running"))
  (let* ((command (password-store--cmd args))
         (buffer (get-buffer-create "*pass*")))
    (with-editor
      (setq password-store-process (start-process-shell-command "pass" buffer command)))))

(defun password-store--run-init (gpg-ids &optional folder)
  (apply 'password-store--run "init"
         (if folder (format "--path=%s" folder))
         gpg-ids))

(defun password-store--run-list (&optional subdir)
  (error "Not implemented"))

(defun password-store--run-grep (&optional string)
  (error "Not implemented"))

(defun password-store--run-find (&optional string)
  (error "Not implemented"))

(defun password-store--run-show (entry &optional callback)
  (if callback
      (password-store--run-1 callback "show" entry)
    (password-store--run "show" entry)))

(defun password-store--run-insert (entry password &optional force)
  (error "Not implemented"))

(defun password-store--run-edit (entry)
  (setq password-store-entry entry)
  (password-store--run-async "edit" entry))

(defun password-store--run-generate (entry password-length &optional force no-symbols)
  (password-store--run "generate"
                       (if force "--force")
                       (if no-symbols "--no-symbols")
                       entry
                       (number-to-string password-length)))

(defun password-store--run-remove (entry &optional recursive)
  (password-store--run "remove"
                       "--force"
                       (if recursive "--recursive")
                       entry))

(defun password-store--run-rename (entry new-entry &optional force)
  (password-store--run "rename"
                       (if force "--force")
                       entry
                       new-entry))

(defun password-store--run-copy (entry new-entry &optional force)
  (password-store--run "copy"
                       (if force "--force")
                       entry
                       new-entry))

(defun password-store--run-git (&rest args)
  (apply 'password-store--run "git"
         args))

(defun password-store--run-version ()
  (password-store--run "version"))

(defvar password-store-kill-ring-pointer nil
  "The tail of of the kill ring ring whose car is the password.")

(defun password-store-dir ()
  "Return password store directory."
  (or (bound-and-true-p auth-source-pass-filename)
      (getenv "PASSWORD_STORE_DIR")
      "~/.password-store"))

(defun password-store--entry-to-file (entry)
  "Return file name corresponding to ENTRY."
  (concat (expand-file-name entry (password-store-dir)) ".gpg"))

(defun password-store--file-to-entry (file)
  "Return entry name corresponding to FILE."
  (file-name-sans-extension (file-relative-name file (password-store-dir))))

(defun password-store--completing-read (&optional require-match)
  "Read a password entry in the minibuffer, with completion.

Require a matching password if `REQUIRE-MATCH' is 't'."
  (completing-read "Password entry: " (password-store-list) nil require-match))

(defun password-store-parse-entry (entry)
  "Return an alist of the data associated with ENTRY.

ENTRY is the name of a password-store entry."
  (auth-source-pass-parse-entry entry))

(defun password-store-read-field (entry)
  "Read a field in the minibuffer, with completion for ENTRY."
  (let* ((inhibit-message t)
         (valid-fields (seq-map #'car (password-store-parse-entry entry))))
    (completing-read "Field: " valid-fields nil 'match)))

(defun password-store-list (&optional subdir)
  "List password entries under SUBDIR."
  (unless subdir (setq subdir ""))
  (let ((dir (expand-file-name subdir (password-store-dir))))
    (if (file-directory-p dir)
        (seq-uniq
         (seq-map #'password-store--file-to-entry
                 (directory-files-recursively dir ".+\\.gpg\\'"))))))

;;;###autoload
(defun password-store-edit (entry)
  "Edit password for ENTRY."
  (interactive (list (password-store--completing-read t)))
  (password-store--run-edit entry))

;;;###autoload
(defun password-store-get (entry &optional field)
  "Return FIELD for ENTRY, defaults to password is field is nil.

Returns the first line of the password data."
  (auth-source-pass-get (or field 'secret) entry))

;;;###autoload
(defun password-store-clear ()
  "Clear secret in the kill ring."
  (interactive "i")
  (when password-store-last-copied
    (seq-let (pointer timer field) password-store-last-copied
      (setcar pointer "")
      ;; Remove it from the system clipboard
      (gui-select-text "")
      (cancel-timer timer)
      (message "Password store field %s removed from kill ring." field))
    (setq password-store-last-copied nil)))

;;;###autoload
(defun password-store-copy (entry &optional field)
  "Add password for ENTRY into the kill ring.

Clear previous password from the kill ring.  Pointer to the kill ring
is stored in `password-store-kill-ring-pointer'.  Password is cleared
after `password-store-time-before-clipboard-restore' seconds."
  (interactive
   (let* ((entry (password-store--completing-read t))
          (field (if current-prefix-arg (password-store-read-field entry) 'secret)))
     (list entry field)))
  (password-store-clear)
  (kill-new (password-store-get entry field))
  (let ((pointer kill-ring-yank-pointer)
        (timer (run-at-time password-store-time-before-clipboard-restore nil #'password-store-clear)))
    (setq password-store-last-copied (list pointer timer field)))
  (message "Password store copied %s for %s to the kill ring for %s seconds."
           field entry password-store-time-before-clipboard-restore))


;;;###autoload
(defun password-store-init (gpg-id)
  "Initialize new password store and use GPG-ID for encryption.

Separate multiple IDs with spaces."
  (interactive (list (read-string "GPG ID: ")))
  (message "%s" (password-store--run-init (split-string gpg-id))))

;;;###autoload
(defun password-store-insert (entry password)
  "Insert a new ENTRY containing PASSWORD."
  (interactive (list (password-store--completing-read)
                     (read-passwd "Password: " t)))
  (let* ((command (format "echo %s | %s insert -m -f %s"
                          (shell-quote-argument password)
                          password-store-executable
                          (shell-quote-argument entry)))
         (ret (process-file-shell-command command)))
    (if (zerop ret)
        (message "Successfully inserted entry for %s" entry)
      (message "Cannot insert entry for %s" entry))
    nil))

;;;###autoload
(defun password-store-generate (entry &optional password-length)
  "Generate a new password for ENTRY with PASSWORD-LENGTH.

Default PASSWORD-LENGTH is `password-store-password-length'."
  (interactive (list (password-store--completing-read)
                     (and current-prefix-arg
                          (abs (prefix-numeric-value current-prefix-arg)))))
  ;; A message with the output of the command is not printed because
  ;; the output contains the password.
  (password-store--run-generate
   entry
   (or password-length password-store-password-length)
   'force)
  nil)

;;;###autoload
(defun password-store-generate-no-symbols (entry &optional password-length)
  "Generate a new password without symbols for ENTRY with PASSWORD-LENGTH.

Default PASSWORD-LENGTH is `password-store-password-length'."
  (interactive (list (password-store--completing-read)
                     (and current-prefix-arg
                          (abs (prefix-numeric-value current-prefix-arg)))))

  ;; A message with the output of the command is not printed because
  ;; the output contains the password.
  (password-store--run-generate
   entry
   (or password-length password-store-password-length)
   'force 'no-symbols)
  nil)

;;;###autoload
(defun password-store-create (entry &optional password-length)
  "Generate a new password for ENTRY with PASSWORD-LENGTH and edit it.

Default PASSWORD-LENGTH is `password-store-password-length'."
  (interactive (list (password-store--completing-read)
                     (when current-prefix-arg
                       (abs (prefix-numeric-value current-prefix-arg)))))
  (password-store-generate entry password-length)
  (password-store-edit entry)
  (password-store-copy entry))

;;;###autoload
(defun password-store-remove (entry)
  "Remove existing password for ENTRY."
  (interactive (list (password-store--completing-read t)))
  (message "%s" (password-store--run-remove entry t)))

;;;###autoload
(defun password-store-rename (entry new-entry)
  "Rename ENTRY to NEW-ENTRY."
  (interactive (list (password-store--completing-read t)
                     (read-string "Rename entry to: ")))
  (message "%s" (password-store--run-rename entry new-entry t)))

;;;###autoload
(defun password-store-version ()
  "Show version of pass executable."
  (interactive)
  (message "%s" (password-store--run-version)))

;;;###autoload
(defun password-store-url (entry)
  "Browse URL stored in ENTRY."
  (interactive (list (password-store--completing-read t)))
  (let ((url (password-store-get-field entry password-store-url-field)))
    (if url (browse-url url)
      (error "Field `%s' not found" password-store-url-field))))

(defvar password-store-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")     'with-editor-finish)
    (define-key map (kbd "C-c C-k")     'with-editor-cancel)
    map)
  "Key map used by `password-store-mode'.")

(defconst password-store-filename-regexp "pass\\.[a-zA-Z0-9]+/.*\\.txt$")

(defun password-store-setup-check-buffer ()
  "Check if we are opening a password store entry."
  (and buffer-file-name
       (string-match-p password-store-filename-regexp buffer-file-name)
       (password-store-mode)))

(defvar password-store-font-lock-keywords '(("\n\\([^\n:]+:\\)" 1 font-lock-variable-name-face)))

(defun password-store--toggle-display (overlay hide)
  (if hide
      (overlay-put overlay 'display
                   (propertize "****" 'face 'font-lock-doc-face))
    (overlay-put overlay 'display nil)))

(defun password-store--hide-password ()
  "Hide password."
  (save-excursion
    (let ((overlay (make-overlay (point-min) (line-end-position))))
      (overlay-put overlay 'display
                   (propertize "****" 'face 'font-lock-doc-face))
      (overlay-put overlay 'reveal-toggle-invisible
                   #'password-store--toggle-display))))
;;;###autoload
(define-derived-mode password-store-mode text-mode "pass"
  "`password-store-mode' is a major mode for Unix password manager password entries.

\\{password-store-mode-map}"
  (setq font-lock-defaults '(password-store-font-lock-keywords))
  (rename-buffer (format "*pass %s*" password-store-entry))
  (forward-line)
  (when password-store-hide-password
    (password-store--hide-password)
    (reveal-mode))
  ;; Delay message to hide the default emacsclient message
  (run-at-time 0.3 nil (lambda () (message "Use C-c C-c to commit changes or C-c C-k to abort."))))

(define-minor-mode global-password-store-mode
  "Edit password store passwords.

This global mode arranges for `password-store-edit-setup' to be called
when a password file is opened.  That usually happens
when pass uses the Emacsclient as $EDITOR to have the user
edit the password.

Loading the library `password-store' by default enables this mode."
  :group 'password-store
  :type 'boolean
  :global t
  :init-value t
  :initialize (lambda (symbol exp)
                (custom-initialize-default symbol exp)
                (when global-password-store-mode
                  (add-hook 'find-file-hook 'password-store-setup-check-buffer)))
  (if global-password-store-mode
      (add-hook  'find-file-hook 'password-store-setup-check-buffer)
    (remove-hook 'find-file-hook 'password-store-setup-check-buffer)))



(provide 'password-store)

;;; password-store.el ends here
