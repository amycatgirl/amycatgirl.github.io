;; amycatgirl.github.io no-js fallback
;; Last authored: Thu 19 Mar 18:41:03 AST 2026
;; Depends on: drakma, yason, uiop, cl-ppcre, trivia, alexandria, spinneret, local-time

(ql:quickload '(drakma yason uiop cl-ppcre trivia alexandria spinneret local-time))

(defconstant +user_did+ "did:plc:gijpvbkdbr56kazbdjhfvb3d")
(defconstant +viewer+ "https://amybunny.leaflet.pub/%s"
  "URL to standard.site compliant viewer. Default is leaflet.
%s is passed to `format' with the CID of the record.")
(defconstant +max-entries+ 5)

;; utilities
(defmacro run-command (command)
  `(uiop:run-program ,command :output '(:string :stripped t)))

(defun keywordize (name)
  (intern (string-upcase name) "KEYWORD"))

(defmacro get-in-plist (place indicators)
  (let ((rev-indicators (reverse indicators)))
    (labels ((construct (y ys) (cond
				 ((null ys) (list 'getf place y))
				 (t (list 'getf (construct (car ys) (cdr ys)) y)))))
      (construct (car rev-indicators) (cdr rev-indicators)))))

;; Git
(defun git-add (&rest files)
  (run-command `("git" "add" ,@files)))

(defun git-commit (message)
  (run-command `("git" "commit" "-m" ,(concatenate 'string "[fallback-gen] " message))))
;; HTTP
(defun build-query-params-from-plist (parameters)
  "Build a string of URL-encoded query parameters from a plist."
  ;; HACK: ~: consumes the argument, so we need to pass it twice for iteration to work
  (let ((params-cleaned (mapcar #'(lambda (value)
				    (if (keywordp value)
					(string-downcase (symbol-name value))
					value))
				parameters)))
    (format nil "~:[~;?~{~(~A~)=~A~^&~}~]" parameters params-cleaned)))

(defun make-request (where)
  (let ((stream (drakma:http-request where
				     :want-stream t
				     :user-agent " amycatgirl.github.io/1.0")))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (yason:parse stream :object-as :plist :object-key-fn #'keywordize)))

;; ATProto
;;; DID
(defun resolve-did-document--plc (did)
  (make-request (format nil "https://plc.directory/~a" did)))

(defun resolve-did-document--web (did)
  "Resolve a DID document via DID:WEB method.
See https://w3c-ccg.github.io/did-method-web/ for specification details."
  (let* ((document-location (subseq did (length "did:web:")))
	 (qualified-path (concatenate 'string "https://" document-location "/.well-known/did.json"))
	 (document (make-request qualified-path)))
    (if (equal (getf document :id) did)
	document
	(error "Could not resolve document from did:web ~S" did))))

(defun get-did-method (did)
  (cl-ppcre:register-groups-bind (method) ("^did:([a-z]+):[a-zA-Z0-9._:%-]*[a-zA-Z0-9._-]$" did)
   method))

(defun resolve-pds (did)
  "Resolve a PDS by DID."
  (let* ((did-method (get-did-method did))
	 (document (trivia:match did-method
		     ("web" (resolve-did-document--web did))
		     ("plc" (resolve-did-document--plc did))
		     (_ (error "Unsuported DID method ~S" did-method))))
	 (services (getf document :service)))
    (getf (find-if (lambda (service)
		     (equal (getf service :id) "#atproto_pds"))
		   services)
	  :serviceEndpoint)))

;;; PDS
(defun make-pds-request (pds method &rest query)
  "Make a unauthenticated XRPC call to the PDS, where `method' is the NSID of the endpoint to call.
Aditional query parameters in the request must be passed inside of `query', where key-value pairs are denoted by keywords and values."
  (let ((url (format nil "~A/xrpc/~A~A" pds method (build-query-params-from-plist query))))
    (make-request url)))

(defun fetch-entries (did &optional (maximum 5))
  (let* ((pds (resolve-pds did)))
    (make-pds-request pds "com.atproto.repo.listRecords"
		      :repo did
		      :collection "site.standard.document"
		      :limit maximum)))

;; Generator
(defun build-elements-from-entries (entries))
(defun sort-entries-by-date (entries))
(defun generate-html ())
(defun publish ())
