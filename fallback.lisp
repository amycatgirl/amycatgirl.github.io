;; amycatgirl.github.io no-js fallback
;; Last authored: Thu 19 Mar 18:41:03 AST 2026
;; Depends on: drakma, yason, uiop, cl-ppcre, trivia, alexandria, spinneret, local-time

(ql:quickload '(drakma yason uiop cl-ppcre trivia alexandria spinneret local-time))

(defconstant +user-did+ "did:plc:gijpvbkdbr56kazbdjhfvb3d"
  "DID of the user to fetch entries from")

(defconstant +viewers+ '(("3mi2fpvnluk2b" . "https://amybunny.offprint.app~A")
			 ("3mbpuyp4pqk2j" . "https://amybunny.leaflet.pub/~A"))
  "An alist of publication keys -> URLs.
~A is passed to `format' with the `path' of the record.")

(defconstant +max-entries+ 5
  "Maximum amount of entries fetched.")

;; utilities
(defmacro run-command (command)
  `(uiop:run-program ,command :output '(:string :stripped t)))

(defun keywordize (name)
  (intern (string-upcase name) "KEYWORD"))

(defmacro get-in (place indicators &key (accessor-fn 'getf))
  (let ((rev-indicators (reverse indicators)))
    (labels ((construct (y ys) (cond
				 ((null ys) (list accessor-fn place y))
				 (t (list accessor-fn (construct (car ys) (cdr ys)) y)))))
      (construct (car rev-indicators) (cdr rev-indicators)))))

;; Git
(defun git-add (&rest files)
  (run-command `("git" "add" ,@files)))

(defun git-commit (message)
  (run-command `("git" "commit" "-m" ,(concatenate 'string "[fallback-gen] " message))))

(defun git-push ()
  (run-command '("git" "push")))
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

;; Standard.site
(defun get-publication-rkey (site)
  (car (last (uiop:split-string
	       site :separator '(#\/)))))

;; Generator
(defun published-at-element (date)
  (let ((time (local-time:parse-timestring date)))
    (spinneret:with-html
      (:time.published-at :attrs (list :datetime date)
			  (local-time:format-timestring nil time :format '((:day 2) "/" (:month 2) "/" :year ", "
									   (:hour 2) #\: (:min 2)))))))

(defun entry-element (entry)
  (let ((title (get-in entry (:value :title)))
	(description (or (get-in entry (:value :description)) ""))
	(date (get-in entry (:value :publishedat)))
	(link (format nil (cdr (assoc (get-publication-rkey (get-in entry (:value :site)))
				      +viewers+ :test 'equal))
		      (get-in entry (:value :path))))
	(spinneret:*html-style* :tree))
    (spinneret:with-html-string
      (:div.entry
       (:a.base-anchor :href link
		       (:h3.title title))
       (:p.description description)
       (:div.metadata
	(published-at-element date))))))

(defun generate-html (entries)
  (format nil "~%~{~A~^~%~}~%" (mapcar #'entry-element entries)))

(defun read-file-to-string (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
    (let ((buf (make-string (file-length stream))))
      (read-sequence buf stream)
      buf)))

(defun write-to-noscript-block (page entries)
  (let* ((fragment (generate-html entries))
	 (file-content (read-file-to-string page))
	 (start-pos (or (search "<noscript>" file-content)
			(error "Could not find opening tag in ~A" page)))
	 (end-pos (or (search "</noscript>" file-content)
		      (error "Could not find closing tag in ~A" page)))
	 (insert-start (+ start-pos (length "<noscript>")))
	 (before (subseq file-content 0 insert-start))
	 (after (subseq file-content end-pos)))
    (with-open-file (file page :direction :output :if-exists :supersede)
      (write-string (concatenate 'string before fragment after) file))))

(defun publish ()
  (let ((records (getf (fetch-entries +user-did+
				      +max-entries+) :records)))
    (write-to-noscript-block #p"./index.html"
			     records))
  (handler-case
      (progn (git-add "index.html")
	     (git-commit "generate noscript fallback")
	     (git-push))
    (t ()
      (format t "Either:~%- There were no changes between runs~%- Creating a commit failed for some reason (check git status)~%- Or the current branch doesn't have an upstream~2%Please try running the publishing steps manually or run the fallback script again")
      (uiop:quit 1))))
