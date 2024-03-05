;;; gim-jira.el --- A better Jira interface

;; Author: George Mauer <gmauer@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dash "20240103.70123") (request "20230127.417"))
;; Keywords: jira
;; URL: https://github.com/togakangaroo/gim-jira

;; This file is not part of GNU Emacs.

;;; Commentary:

;; gim-jira is a simple Emacs package that provides basic jira integration

;;; Code:
(require 'dash)
(require 'request)
(require 'pcache)

(defcustom gim-jira/jira-instance-url (getenv "JIRA_INSTANCE_URL")
  "URL for jira."
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/jira-api-token (getenv "JIRA_API_TOKEN")
  "JIRA API token used for authentication. https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/."
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/jira-username (getenv "JIRA_API_USERNAME")
  "JIRA username to use for authentication."
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/property-for-jira-issue-key "gim-jira-issue-key"
  "Property to be used to store the jira issue key."
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/property-for-jira-issue-parent-key "gim-jira-issue-parent-key"
  "Property to be searched for the issue parent's key. This can be set higher up in the outline."
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/property-for-issue-description-prefix "gim-jira-issue-description-prefix"
  "Property to be searched for that can optionally contain a prefix to use for all descriptions."
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/cache-name "gim-jira"
  "Name of cache used with pcase to store persistent info"
  :type 'string
  :group 'gim-jira)

(defvar gj/-known-users
  (pcache-get (pcache-repository gj/cache-name) 'known-users)
  "All jira users known by the system.")

;;;###autoload
(defun gim-jira/refresh-known-users ()
  "Refresh the list of known users from the server"
  (interactive)
  (-let ((new-value (--> (gim-jira/-request "users" :params '((maxResults . 500)))
                         (--map (-let (((&alist 'displayName dn 'accountId id 'emailAddress ea) it))
                                   `((displayName . ,dn)
                                     (emailAddress . ,ea)
                                     (accountId . ,id)))
                                it))))
       (pcache-put (pcache-repository gj/cache-name) 'known-users new-value)
       (setq gj/-known-users new-value)))

;;;###autoload
(defun gim-jira/standard-headers ()
  "Standard set of headers - including authentication - to be appended to all requests."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(--> (format "%s:%s" gj/jira-username gim-jira/jira-api-token)
                             (base64-encode-string it 't)
                             (format "Basic %s" it)))))

(cl-defun gim-jira/-org-get-current-property (property-name &optional &key (inherit 't) (default nil))
  "Get the currently available property value, inheriting by default"
  (or (org-entry-get (point) property-name 't) default))

(cl-defun gj/-request (api-action &optional &key (type "GET") params)
  (-let* ((url (format "%s/rest/api/3/%s" gj/jira-instance-url api-action))
          (res (request url
                :type type
                :params params
                :sync t
                :parser 'json-read
                :headers (gj/standard-headers)))
          (res-data (request-response-data res)))
    res-data))

;;;###autoload
(cl-defun gim-jira/create-or-update-issue-from-heading (&optional heading-pos (buffer (current-buffer)))
  "For heading at the char, either create or update the jira issue in the heading denoted by the position. If no heading-pos provided, use the heading we are currently in."
  (interactive)

  (save-excursion
    (unless heading-pos
      (setq heading-pos (org-back-to-heading)))

    (-let* ((element-title (org-element-property :title (org-element-at-point)))
            ((heading ...) (-list element-title))) ; element title might be a string or a list where we need the first element
      (goto-char heading-pos)
      (org-next-block 1)
      (-let* ((description-prefix (gj/-org-get-current-property gjpf-issue-description-prefix :default ""))
              (parent-issue-key (gj/-org-get-current-property gjpf-jira-issue-parent-key))
              (current-issue-key (gj/-org-get-current-property gjpf-jira-issue-key))
              (issue-description (format "%s%s"
                                         description-prefix
                                         (org-element-property :value (org-element-at-point))))
              ((confirm-question verb extra-fields) (cond (current-issue-key `(,(format "Would you like to update issue %s? " current-issue-key)
                                                                               "PUT"
                                                                               nil))
                                                          (:else `("Would you like to create this issue?"
                                                                   "POST"
                                                                   ,`((summary . ,heading)
                                                                      (parent . ((key . ,parent-issue-key)))
                                                                      (project . ((key . "BI")))
                                                                      (issuetype . ((name . "Tech Task"))))))))
              (payload `((fields . ((description . ,issue-description)
                                    . ,extra-fields))))
              (json-payload (json-encode payload)))

        (with-temp-buffer
          (insert json-payload)
          (json-mode)
          (json-pretty-print-buffer)
          (read-only-mode)
          (pop-to-buffer (current-buffer))
          (unless (y-or-n-p confirm-question)
            (cl-return-from gj/create-or-update-issue-from-heading)))

        (-let* ((path (format "issue/%s" (or current-issue-key "")))
                (res-data (gj/-request path :type verb))
                (new-issue-key (alist-get 'key res-data)))
          (message (format "Response: %s" res-data))
          (when new-issue-key
            (org-entry-put nil gjpf-jira-issue-key new-issue-key)))))))

(cl-defun gim-jira/format-user-for-selection (user)
  "Format a user for selection"
  (format "%s (%s)" (alist-get 'displayName user)
                    (or (alist-get 'emailAddress user) "")))

(defun gim-jira/-helm-source-users ()
  "Build the Helm source to insert a reference to one of the current jira users."
  (helm-build-sync-source "User"
    :candidates (--map `(,(gj/format-user-for-selection it), it)
                       gj/-known-users)
    :action (helm-make-actions
             "Print user" (lambda (user)
                            (message "Selected user: %s" user)))))

(defun gim-jira/helm-select-user ()
  "Use Helm to select a user from the `gim-temp/users` list."
  (interactive)
  (helm :sources (gim-jira/-helm-source-users) :buffer "*helm select user*"))

(provide 'gim-jira)

;; Local Variables:
;; coding: utf-8
;; read-symbol-shorthands: (("gj/" . "gim-jira/")
;;                          ("gjpf-" . "gim-jira/property-for-"))
;; End:

;;; gim-jira.el ends here
