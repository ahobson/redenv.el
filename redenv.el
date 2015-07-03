;;; redenv.el --- Emacs integration for redenv

;; Copyright (C) 2015 Andrew Hobson

;; Author: Andrew Hobson <ahobson@gmail.com>
;; URL: https://github.com/ahobson/redenv.el
;; Version: 0.5.0
;; Created: 3 July 2015
;; Keywords: ruby redenv

;; This code is almost entirely inspired by rvm.el by Yves Senn
;; <yves.senn@gmx.ch>.

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; M-x redenv-use allows you to switch the current session to the ruby
;; implementation of your choice. You can also change the active
;; gemset.

;;; Compiler support:

(eval-when-compile (require 'cl))
(defvar eshell-path-env)
(defvar persp-mode)
(defvar perspectives-hash)
(declare-function persp-switch "perspective" (name))

;;; Code:

(defvar redenv-global-env-prefix
  nil
  "Redenv global prefix for finding redenv environment")

(defvar redenv-local-env-file-name
  ".redenv"
  "Redenv local filename that contains the redenv environment")

(defvar redenv-configuration-ruby-version-file-name
  ".ruby-version"
  "Ruby version configuration file name")

(defvar redenv-configuration-ruby-gemset-file-name
  ".ruby-gemset"
  "Ruby version configuration file name")

(defvar redenv-configuration-gemfile-file-name
  "Gemfile"
  "Gemfile file name")

(defcustom redenv-executable
  (or (executable-find "redenv")
      (and (file-readable-p "~/bin/redenv") "~/bin/redenv"))
  "Location of redenv executable."
  :group 'redenv
  :type 'file)

(defcustom redenv-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by redenv.el to interactivly complete user input"
  :group 'redenv
  :type 'function)

(defcustom redenv-interactive-find-file-function
  (if ido-mode 'ido-find-file 'find-file)
  "The function which is used by redenv.el to interactivly open files"
  :group 'redenv
  :type 'function)

(defcustom redenv-verbose t
  "If true, redenv will print messages for various tasks."
  :group 'redenv
  :type 'boolean)


(defvar redenv--current-ruby nil
  "Current active Ruby version.")

(defvar redenv--current-gemset nil
  "Current active gemset.")

(defvar redenv--gemset-separator "@"
  "character that separates the ruby version from the gemset.")

(defvar redenv--current-ruby-binary-path nil
  "reflects the path to the current 'ruby' executable.
This path gets added to the PATH variable and the exec-path list.")

(defvar redenv--current-gem-binary-path nil
  "reflects the path to the current 'rubygems' executables.
This path gets added to the PATH variable and the exec-path list.")

;; Support Code

;; Put with other utils
;; From http://www.emacswiki.org/emacs/ElispCookbook
(defun redenv--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun redenv--message (format-string &rest objects)
  "Like `message', but will only print if `redenv-verbose' is true."
  (when redenv-verbose
    (apply 'message (cons format-string objects))))


;; Application Code

;;;###autoload
(defun redenv-activate-corresponding-ruby ()
  "activate the corresponding ruby version for the file in the current buffer.
This function searches for local-env, ruby-version and
ruby-gemset files and activates the configured ruby. If no env is
found, nothing is changed."
  (interactive)

  (when (redenv-working-p)
    (let ((redenv-info (redenv--load-info-ruby-version)))
      (when redenv-info
        (redenv-use (first redenv-info) (second redenv-info))))))

(defun redenv--load-info-ruby-version (&optional path)
  (let ((local-env-path (redenv--locate-file
                         redenv-local-env-file-name))
        (config-file-path (redenv--locate-file
                           redenv-configuration-ruby-version-file-name path))
        (gemset-file-path (redenv--locate-file
                           redenv-configuration-ruby-gemset-file-name path)))
    (cond ((redenv-local-env-p local-env-path)
           (list local-env-path nil))

          ((and config-file-path gemset-file-path)
           (list (redenv--chomp
                  (redenv--get-string-from-file config-file-path))
                 (redenv--chomp
                  (redenv--get-string-from-file gemset-file-path)))))))

;;;###autoload
(defun redenv-use (new-ruby new-gemset)
  "switch the current ruby version to any ruby, which is
installed with redenv"
  (interactive
   (let* ((picked-ruby-gemset (redenv--completing-read "Ruby/gemset: "
                                             (redenv/list))))
     (split-string picked-ruby-gemset "@")))
  (when (redenv-working-p)
    (let* ((new-ruby-with-gemset
            (redenv--ruby-gemset-string new-ruby new-gemset))
           (ruby-info (redenv/info new-ruby-with-gemset))
           (new-ruby-binary (cdr (assoc "ruby" ruby-info)))
           (new-ruby-gemhome (cdr (assoc "GEM_HOME" ruby-info)))
           (new-ruby-gempath (cdr (assoc "GEM_PATH" ruby-info))))
      (setq redenv--current-ruby new-ruby)
      (setq recenv--current-gemset new-gemset)
      (redenv--set-ruby (file-name-directory new-ruby-binary))
      (redenv--set-gemhome new-ruby-gemhome new-ruby-gempath new-gemset))
    (redenv--message (concat "Ruby: " new-ruby " Gemset: " new-gemset))))

;;;###autoload
(defun redenv-open-gem (gemhome)
  (interactive (list (redenv--emacs-gemhome)))
  (when (redenv-working-p)
    (let* ((gems-dir (concat gemhome "/gems/"))
           (gem-name (redenv--completing-read
                      "Gem: "
                      (directory-files gems-dir nil "^[^.]")))
           (gem-dir (concat gems-dir gem-name)))
      (when (and (featurep 'perspective) persp-mode)
        (let ((initialize (not (gethash gem-name perspectives-hash))))
          (persp-switch gem-name)))
      (redenv--find-file gem-dir))))

(defun redenv-activate-ruby-for (path &optional callback)
  "Activate Ruby for PATH.

If CALLBACK is specified, active Ruby for PATH only in that
function."
  (let* ((path (directory-file-name path))
         (prev-ruby redenv--current-ruby)
         (prev-gemset redenv--current-gemset)
         (redenv-info (redenv--load-info-ruby-version path)))
    (apply 'redenv-use redenv-info)
    (when callback
      (unwind-protect
          (funcall callback)
        (redenv-use prev-ruby prev-gemset)))))

;;;; TODO: take buffer switching into account
(defun redenv-autodetect-ruby ()
  (interactive)
  (when (redenv-working-p)
    (add-hook 'ruby-mode-hook 'redenv-activate-corresponding-ruby)
    (redenv--message "redenv.el is now autodetecting the ruby version")))

(defun redenv-autodetect-ruby-stop ()
  (interactive)
  (when (redenv-working-p)
    (remove-hook 'ruby-mode-hook 'redenv-activate-corresponding-ruby)
    (redenv--message "stopped redenv.el from autodetecting ruby versions")))

(defun redenv/list (&optional default-ruby)
  (if (redenv-global-env-p)
      (directory-files redenv-global-env-prefix nil "@")
    '()))

(defun redenv/info (&optional ruby-version)
  (let ((parsed-info '())
        (redenv-dir (cond ((redenv-local-env-p ruby-version)
                           (file-truename ruby-version))

                          ((redenv-global-env-p)
                           (concat
                            (file-name-as-directory redenv-global-env-prefix)
                            ruby-version)))))


    (when (not redenv-dir)
      (error "The ruby version: %s is not installed" ruby-version))
    (add-to-list 'parsed-info (cons "ruby" (concat redenv-dir "bin/ruby")))
    (add-to-list 'parsed-info (cons "GEM_HOME" redenv-dir))
    (add-to-list 'parsed-info (cons "GEM_PATH" redenv-dir))
    parsed-info))

(defun redenv--string-trim (string)
  (replace-regexp-in-string "^\\s-*\\|\\s-*$" "" string))

(defun redenv--ruby-gemset-string (ruby-version gemset)
  (if (redenv-local-env-p ruby-version) ruby-version
    (concat ruby-version redenv--gemset-separator gemset)))

(defun redenv--completing-read (prompt options)
  (let ((selected (funcall redenv-interactive-completion-function prompt
                           options)))
    (redenv--string-trim selected)))

(defun redenv--find-file (directory)
  (let ((default-directory directory))
    (call-interactively redenv-interactive-find-file-function)))

(defun redenv--emacs-ruby-binary ()
  redenv--current-ruby-binary-path)

(defun redenv--emacs-gemhome ()
  (getenv "GEM_HOME"))

(defun redenv--emacs-gempath ()
  (getenv "GEM_PATH"))

(defun redenv--change-path (current-binary-var new-binaries)
  (let ((current-binaries-for-path
         (mapconcat 'identity (eval current-binary-var) ":"))
        (new-binaries-for-path (mapconcat 'identity new-binaries ":")))
    (if (and (eval current-binary-var)
             (not (string= (first (eval current-binary-var)) "/bin")))
        (progn
          (setenv "PATH" (replace-regexp-in-string
                          (regexp-quote current-binaries-for-path)
                          new-binaries-for-path
                          (getenv "PATH")))
          (dolist (binary (eval current-binary-var))
            (setq exec-path (remove binary exec-path))))
      (setenv "PATH" (concat new-binaries-for-path ":" (getenv "PATH"))))
    (dolist (binary new-binaries)
      (add-to-list 'exec-path binary))
    (setq eshell-path-env (getenv "PATH"))
    (set current-binary-var new-binaries)))

(defun redenv--set-ruby (ruby-binary)
  (redenv--change-path 'redenv--current-ruby-binary-path (list ruby-binary)))

(defun redenv--locate-file (file-name &optional path)
  "searches the directory tree for an given file. Returns nil if
the file was not found."
  (let ((directory (locate-dominating-file
                    (or path (expand-file-name (or buffer-file-name "")))
                    file-name)))
    (when directory (expand-file-name file-name directory))))

(defun redenv--get-string-from-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun redenv--gem-binary-path-from-gem-path (gempath)
  (let ((gem-paths (split-string gempath ":")))
    (mapcar (lambda (path) (concat path "/bin")) gem-paths)))

(defun redenv--set-gemhome (gemhome gempath gemset)
  (if (and gemhome gempath gemset)
      (progn
        (setenv "GEM_HOME" gemhome)
        (setenv "GEM_PATH" gempath)
        (setenv "BUNDLE_PATH" gemhome)
        (redenv--change-path 'redenv--current-gem-binary-path
                             (redenv--gem-binary-path-from-gem-path gempath)))
    (setenv "GEM_HOME" "")
    (setenv "GEM_PATH" "")
    (setenv "BUNDLE_PATH" "")))

(defun redenv-working-p ()
  (and redenv-executable (file-exists-p redenv-executable)))

(defun redenv-local-env-p (ruby-version)
  (and ruby-version
       (eq (file-name-base ruby-version) redenv-local-env-file-name)
       (file-exists-p (concat
                       (file-name-as-directory ruby-version)
                       "gems"))))

(defun redenv-global-env-p ()
    (and redenv-global-env-prefix
         (file-directory-p redenv-global-env-prefix)))

(defun redenv-gem-install (gem)
  "Install GEM into the currently active redenv Gemset."
  (interactive "Gem Install: ")
  (shell-command (format "%s install %s&" ; & executes async
                         (concat (first redenv--current-ruby-binary-path)
                                 "/gem") gem))
  (pop-to-buffer "*Async Shell Command*"))

(provide 'redenv)
;;; redenv.el ends here
