;;; explain.el --- Elisp docstrings delivered over HTTP  -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2023 Roni Kallio

;; Author: Roni Kallio <roni.jj.kallio@gmail.com>
;; Maintainer: Roni Kallio <roni.jj.kallio@gmail.com>
;; Version: 1
;; Homepage: https://github.com/rkallio/explain
;; Package-Requires: (
;;     (emacs "28.0")
;;     (simple-httpd "1.5.1"))

;; Explain is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Explain is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Explain.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Prepare to serve Elisp docstrings over HTTP.  After the feature is
;; loaded, call `httpd-start' to start the server.

;; Configure the accepted host and port by setting the variables
;; `httpd-host' and `httpd-port'.  By default the server will listen
;; for requests sent to localhost at port 8080.  See the source code
;; of `httpd-start' and `make-network-process'.

;; You might want disable file and directory serving.  In case, set
;; `httpd-serve-files' and `httpd-listings' to nil before starting the
;; server.

;; Once the server is up, there are four accessible paths:

;; - /explain/help -- Serves a summary of the servers features

;; - /explain/value -- Serves value docstrings

;; - /explain/function -- Serves function definition docstrings

;; - /explain/explain -- Serves both value and function docstrings

;;; Code:

(require 'simple-httpd)

;;; Variables:

(defvar explain--docstring-header-format "%s docstring is:\n%s")

(defvar explain--missing-symbol-name-message
  "Request didn't contain a symbol name.")

(defvar explain--mime-type "text/plain; charset=utf-8"
  "The Content-Type header of the response.")

;;; Functions:

(defun explain--format-symbol-not-found-message (symbol-string)
  "Format an invalid symbol message.

SYMBOL-STRING is the invalid symbol."
  (format "%s is invalid." symbol-string))

;; `message-indent-citation' doesn't work :(
(defun explain--indent-docstring (docstring)
  "Indent and quote DOCSTRING.

Prefixes each line of DOCSTRING with \"> \"."
  (concat "> " (string-replace "\n" "\n> " docstring)))

(defun explain--format-string-for-http (string)
  "Prepare STRING to be sent as the body of an HTTP response."
  (format "%s\n" string))

;;; Macros:

(defmacro explain--respond-with (status-code message)
  (declare (indent defun))
  `(progn
     (insert (explain--format-string-for-http ,message))
     (httpd-send-header t explain--mime-type ,status-code)))

(defmacro explain--validate-input-exists (param)
  "Validate that PARAM was passed to the servlet.

In some cases, PARAM might be nil or an empty string.  For
example, in the case of a servlet bound to the path
/explain/explain/:symbol-string will result with a nil
symbol-string if a request is made to the path /explain/explain.
Likewise, an empty string will result if a request is made to
/explain/explain/.

If the PARAM doesn't pass avalidation check, respond back to the
client with an HTTP status code of 400."
  `(when (or (null ,param)
             (string-empty-p ,param))
     (explain--respond-with 400 explain--missing-symbol-name-message)))

(defmacro explain--404 (symbol-string)
  `(explain--respond-with 404
     (explain--format-symbol-not-found-message ,symbol-string)))

(defmacro explain--validate-and-intern-input (symbol-string symbol-variable &rest body)
  (declare (indent defun))
  `(progn
     (explain--validate-input-exists ,symbol-string)
     (if-let ((,symbol-variable (intern-soft ,symbol-string)))
         (progn ,@body)
       (explain--404 ,symbol-string))))

;;; Servlets:

(defservlet* explain/value/:symbol-string text/plain ()
  (explain--validate-and-intern-input symbol-string sym
    (if-let ((docstring (documentation-property sym 'variable-documentation)))
        (explain--respond-with 200 docstring)
      (explain--404 symbol-string))))

(defservlet* explain/function/:symbol-string text/plain ()
  (explain--validate-and-intern-input symbol-string sym
    (if-let ((docstring
              (and (fboundp sym)
                   (documentation sym))))
        (explain--respond-with 200 docstring)
      (explain--404 symbol-string))))

(defservlet* explain/explain/:symbol-string text/plain ()
  (explain--validate-and-intern-input symbol-string sym
    (let ((docstrings))
      (when-let ((value-docstring (documentation-property sym 'variable-documentation)))
        (push (cons 'value value-docstring) docstrings))
      (when-let ((function-docstring
                  (and (fboundp sym)
                       (documentation sym))))
        (push (cons 'function function-docstring) docstrings))
      (if (not (null docstrings))
          (let ((message))
            (when-let ((value-docstring (alist-get 'value docstrings)))
              (setq message
                    (format
                     explain--docstring-header-format "Value"
                     (explain--indent-docstring value-docstring))))
            (when-let ((function-docstring (alist-get 'function docstrings)))
              (setq message
                    (concat message
                            ;; if the previous `when-let' block wrote
                            ;; a value to `message', append an empty
                            ;; line before writing function docstring,
                            (when message "\n\n")
                            (format explain--docstring-header-format "Function"
                                    (explain--indent-docstring function-docstring)))))
            (explain--respond-with 200 message))
        (explain--404 symbol-string)))))

(defservlet* explain/help text/plain ()
  (explain--respond-with 200 "Endpoints:

* /help/:symbol -- Look up the documentation for :symbol.  If
  neither exists, returns status code 404.

* /value/:symbol -- Look up the value documentation for :symbol.
  If it doesn't exist, returns status code 404.

* /function/:symbol -- Look up the function documentation for
  :symbol.  If it doesn't exist, returns status code 404."))

(provide 'explain)
;;; explain.el ends here
