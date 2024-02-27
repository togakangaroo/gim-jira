;;; gim-jira.el --- A better Jira interface...in emacs.

;; Author: George Mauer <gmauer@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (dash "20240103.70123") (request "20230127.417")
;; Keywords: jira
;; URL: https://github.com/togakangaroo/gim-jira

;; This file is not part of GNU Emacs.

;;; Commentary:

;; gim-jira is a simple Emacs package that provides basic jira integration

;;; Code:
(require 'dash)
(require 'request)

(defcustom gim-jira/jira-instance-url (getenv "JIRA_INSTANCE_URL")
  "URL for jira"
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/jira-api-token (getenv "JIRA_API_TOKEN")
  "JIRA API token used for authentication. https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/"
  :type 'string
  :group 'gim-jira)

(defcustom gim-jira/jira-username (getenv "JIRA_USERNAME")
  "JIRA username to use for authentication"
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

;;;###autoload
(defun gim-jira/standard-headers ()
  "Standard set of headers - including authentication - to be appended to all requests"
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(--> (format "%s:%s" gim-jira/jira-api-username gim-jira/jira-api-token)
                             (base64-encode-string it 't)
                             (format "Basic %s" it)))))

(cl-defun gim-jira/create-or-update-issue-from-heading (heading &optional (buffer (current-buffer)))
  "For heading with the given text, either create or update the heading from the "
  (save-excursion
    (cl-block 'gim-jira/create-or-update-issue-from-heading
        (--> heading
            (org-find-exact-headline-in-buffer it buffer 't)
            (goto-char it))
      (org-next-block 1)
      (let* ((issue-description (org-element-property :value (org-element-at-point)))
             (parent-issue-key (org-entry-get (point) gim-jira/property-for-jira-issue-parent-key 't))
             (current-issue-key (org-entry-get (point) gim-jira/property-for-jira-issue-key 't))
             (is-update current-issue-key)
             (verb (if is-update "PUT" "POST"))
             (extra-fields (unless is-update
                             `((summary . ,heading)
                               (parent . ((key . ,parent-issue-key)))
                               (project . ((key . "BI")))
                               (issuetype . ((name . "Tech Task"))))))
             (payload `((fields . ((description . ,issue-description)
                                   . ,extra-fields)))))
        (with-temp-buffer
          (insert (pp-to-string payload))
          (emacs-lisp-mode)
          (read-only-mode)
          (pop-to-buffer (current-buffer))
          (unless (y-or-n-p "Would you like to continue with the operation? ")
            (cl-return-from 'gim-jira/create-or-update-issue-from-heading)))
        (--> payload
             json-encode)))))

(provide 'gim-jira)

;; Local Variables:
;; coding: utf-8
;; End:

;;; gim-jira.el ends here
