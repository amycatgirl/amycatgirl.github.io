;; amycatgirl.github.io no-js fallback
;; Last authored: Thu 19 Mar 18:41:03 AST 2026

(defconstant +user_did+ "")
(defconstant +max-entries+ 5)

;; Git
(defun git-add (files))
(defun git-commit (message))

;; ATProto
(defun resolve-pds (did))
(defun fetch-entries (did pds max))
(defun build-elements-from-entries (entries))

;; Generator
(defun sort-entries-by-date (entries))
(defun generate-html ())
(defun publish ())
