(defpackage cl-utils--mail-via-gmail
  (:use :cl :cl-smtp))
;; (ql:quickload :cl-smtp)

(in-package :cl-utils--mail-via-gmail)

(defun send-mail-via-gmail (&key (from nil) (to nil) (subject "") (body "") (password nil) (cc nil) (bcc nil))
  "Send an email via Gmail's SMTP server.

   PASSWORD is Gmail app password, not regular password

   Requires :cl-smtp package

   cl-smtp requires OpenSSL
   download Win64 OpenSSL v3.x.x Light
      from https://slproweb.com/products/Win32OpenSSL.html
   install
   add 'bin' subdirectory to PATH
   check: 'openssl version' in cmd or PowerShell

   (v1 as of 2025-10-29, available in occisn/cl-utils GitHub repository)"

  (unless from
    (error "'From' is required"))
  (unless to
    (error "'To' is required"))
  (unless password
    (error "Gmail app password is required"))

  (cl-smtp:send-email "smtp.gmail.com"
                      from
                      to
                      subject
                      body
                      :port 587
                      :ssl :starttls
                      :authentication (list from password)
                      :cc cc
                      :bcc bcc))

;;; end
