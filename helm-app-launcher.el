;;; helm-app-launcher.el --- Launch applications from Helm Emacs -*- lexical-binding: t -*-

;; Author: Brian Caruso
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/bdc34/helm-app-launcher

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

;; helm-app-launcher defines the `helm-app-launch' command which uses
;; Helm to select an application installed on your machine and launch it.

;;; Acknowledgements:

;; This packages is mostly made up of code from Sebastien Waegeneire commit 9e4cb77
;; https://github.com/sebastienwae/app-launcher
;; That uses uses code from the Counsel package by Oleh Krehel.
;; https://github.com/abo-abo/swiper


(require 'xdg)
(require 'cl-seq)

(defcustom helm-app-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
      (cons (xdg-data-home)
        (xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defvar helm-app-launcher--cache nil
  "Cache of desktop files data.")

(defvar helm-app-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar helm-app-launcher--cached-files nil
  "List of cached desktop files.")

(defun helm-app-launcher-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
    result)
    (dolist (dir helm-app-launcher-apps-directories)
      (when (file-exists-p dir)
    (let ((dir (file-name-as-directory dir)))
      (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
        (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
          (when (and (not (gethash id hash)) (file-readable-p file))
        (push (cons id file) result)
        (puthash id file hash)))))))
    result))

(defun helm-app-launcher-parse-files (files)
  "Parse the .desktop files to return usable informations."
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (entry files hash)
      (let ((file (cdr entry)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
        (end (re-search-forward "^\\[" nil t))
        (visible t)
        name comment exec)
        (catch 'break
          (unless start
        (message "Warning: File %s has no [Desktop Entry] group" file)
        (throw 'break nil))

          (goto-char start)
          (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
        (setq visible nil))
          (setq name (match-string 1))

          (goto-char start)
          (unless (re-search-forward "^Type *= *Application *$" end t)
        (throw 'break nil))
          (setq name (match-string 1))

          (goto-char start)
          (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
        (push file counsel-linux-apps-faulty)
        (message "Warning: File %s has no Name" file)
        (throw 'break nil))
          (setq name (match-string 1))

          (goto-char start)
          (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
        (setq comment (match-string 1)))

          (goto-char start)
          (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
        ;; Don't warn because this can technically be a valid desktop file.
        (throw 'break nil))
          (setq exec (match-string 1))

          (goto-char start)
          (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
        (let ((try-exec (match-string 1)))
          (unless (locate-file try-exec exec-path nil #'file-executable-p)
            (throw 'break nil))))

          (puthash name
               (list (cons 'file file)
                 (cons 'exec exec)
                 (cons 'comment comment)
                 (cons 'visible visible))
               hash))))))))

(defun helm-app-launcher-list-apps ()
  "Return list of all Linux .desktop applications."
  (let* ((new-desktop-alist (helm-app-launcher-list-desktop-files))
	 (new-files (mapcar 'cdr new-desktop-alist)))
    (unless (and (equal new-files helm-app-launcher--cached-files)
		 (null (cl-find-if
			(lambda (file)
			  (time-less-p
			   helm-app-launcher--cache-timestamp
			   (nth 5 (file-attributes file))))
			new-files)))
      (setq helm-app-launcher--cache (helm-app-launcher-parse-files new-desktop-alist))
      (setq helm-app-launcher--cache-timestamp (current-time))
      (setq helm-app-launcher--cached-files new-files)))
  helm-app-launcher--cache)


(defun helm-app-launcher--action-function (selected)
  "Default function used to run the selected application."
  (let* ((exec (cdr (assq 'exec selected)))
	     (command (let (result)
		            (dolist (chunk (split-string exec " ") result)
		              (unless (or (equal chunk "%U")
				                  (equal chunk "%F")
				                  (equal chunk "%u")
				  (equal chunk "%f"))
			            (setq result (concat result chunk " ")))))))
    (call-process-shell-command command nil 0 nil)))

;;;;;;;;;;; Code not from Waegeneire ;;;;;;;;;;;;


(defun helm-app-launcher-format-candidate (k v)
  (let* ((name (format "%s" k))
         (comment (cdr (assoc 'comment v)))
         (additional (if comment (format "- %s" comment) "")))
    (format "%s %s" name additional)))

(defun helm-app-launcher-candidates ()
  "Return a list of XDG menu apps."
  (let* ((apps (helm-app-launcher-list-apps))
         (app-list '()))
    (maphash (lambda (k v)
               (push (cons (helm-app-launcher-format-candidate k v) v) app-list))
             apps)
    app-list)
  )

(defvar helm-source-apps
  (helm-build-sync-source "Applications"
    :fuzzy-match t
    :candidates 'helm-app-launcher-candidates
    :action '(("Run" . helm-app-launcher--action-function))))

;;;###autoload
(defun helm-app-launcher ()
  "Use helm to launch apps."
  (interactive)
  ;; (run-hooks 'helm-taskswitch-open-hooks )
  (select-frame-set-input-focus (window-frame (selected-window)))
  (make-frame-visible) 
  (helm :sources '(helm-source-apps)
        :buffer "*helm-app-launcher*"
        :truncate-lines t))


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;; Provide the helm-app-launcher feature
(provide 'helm-app-launcher)
